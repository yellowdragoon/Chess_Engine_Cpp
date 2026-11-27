"""Browser tests for play.html, driven with real mouse input in headless Chromium.

Piece images normally come from jsDelivr; here those requests get a stub SVG
so the tests run offline and deterministically (one test blocks them instead,
to check the Unicode fallback).
"""

import pytest
from playwright.sync_api import Page, expect, sync_playwright

from conftest import BROKEN_ENGINE, serve, write_script

STUB_SVG = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 45 45"><circle cx="22.5" cy="22.5" r="14"/></svg>'

IDLE = """() => typeof state !== 'undefined' && state && !thinking && pending === 0
          && (state.status || !state.engine || state.turn === state.human)"""


@pytest.fixture(scope="session")
def browser():
    with sync_playwright() as p:
        b = p.chromium.launch()
        yield b
        b.close()


@pytest.fixture(scope="module")
def server(engine_path):
    with serve({"engine": engine_path}) as (url, game):
        yield url


def open_page(browser, url, *, viewport=(1280, 800), offline_pieces=False):
    ctx = browser.new_context(viewport={"width": viewport[0], "height": viewport[1]})
    if offline_pieces:
        ctx.route("**/cdn.jsdelivr.net/**", lambda r: r.abort())
    else:
        ctx.route("**/cdn.jsdelivr.net/**",
                  lambda r: r.fulfill(status=200, content_type="image/svg+xml", body=STUB_SVG))
    page = ctx.new_page()
    errors = []
    page.on("pageerror", lambda e: errors.append(str(e)))
    page.on("console", lambda m: m.type == "error" and errors.append(m.text))
    page.goto(url)
    page.wait_for_function(IDLE)
    return ctx, page, errors


@pytest.fixture
def ui(browser, server):
    ctx, page, errors = open_page(browser, server)
    yield UI(page)
    ctx.close()
    assert errors == [], f"browser errors: {errors}"


class UI:
    def __init__(self, page: Page):
        self.page = page

    # --- reading the page ---
    def state(self):
        return self.page.evaluate("() => state")

    def piece(self, sq):
        """The piece drawn on a square, as a FEN letter, or None."""
        return self.page.evaluate(
            """sq => { const p = document.querySelector(`[data-sq="${sq}"] .pc`);
                       return p ? (p.alt || p.dataset.code || p.textContent) : null; }""", sq)

    def glyph_or_img(self, sq):
        return self.page.evaluate(
            """sq => { const p = document.querySelector(`[data-sq="${sq}"] .pc`);
                       return p ? p.tagName.toLowerCase() : null; }""", sq)

    def targets(self):
        return sorted(self.page.evaluate(
            "() => [...document.querySelectorAll('.sq.target')].map(e => e.dataset.sq)"))

    def classes(self, sq):
        return self.page.locator(f'[data-sq="{sq}"]').get_attribute("class").split()

    def center(self, sq):
        box = self.page.locator(f'[data-sq="{sq}"]').bounding_box()
        return box["x"] + box["width"] / 2, box["y"] + box["height"] / 2

    def banner(self):
        b = self.page.locator("#banner")
        return b.inner_text() if "show" in (b.get_attribute("class") or "") else ""

    def moves_text(self):
        return " ".join(self.page.locator("#moves").inner_text().split())

    # --- acting ---
    def wait(self):
        self.page.wait_for_function(IDLE)

    def click(self, sq):
        self.page.mouse.click(*self.center(sq))

    def move_click(self, a, b):
        self.click(a)
        self.click(b)
        self.wait()

    def move_drag(self, a, b):
        m = self.page.mouse
        m.move(*self.center(a))
        m.down()
        m.move(*self.center(b), steps=8)
        m.up()
        self.wait()

    def new_game(self, fen="", color="w", time="0.1", engine=None):
        p = self.page
        if engine:
            p.select_option("#engine", engine)
        p.fill("#fen", fen)
        p.click(f'#color button[data-v="{color}"]')
        p.click(f'#time button[data-v="{time}"]')
        p.click("#new")
        self.wait()


# ---------------------------------------------------------------- basics

def test_initial_render(ui):
    assert ui.page.locator(".sq").count() == 64
    assert ui.page.locator(".sq .pc").count() == 32
    assert [ui.piece(s) for s in ("a1", "e1", "d8", "h7")] == ["R", "K", "q", "p"]
    assert ui.page.locator(".coord.f").count() == 8 and ui.page.locator(".coord.r").count() == 8
    expect(ui.page.locator("#bottom .name")).to_have_text("You")
    expect(ui.page.locator("#top .name")).to_have_text("engine")
    expect(ui.page.locator("#moves")).to_contain_text("No moves yet")
    expect(ui.page.locator("#undo")).to_be_disabled()


def test_board_squares_are_equal_size_even_with_empty_ranks(ui):
    ui.new_game()
    ui.move_click("e2", "e4")
    heights = ui.page.evaluate(
        "() => [1,2,3,4,5,6,7,8].map(r => document.querySelector(`[data-sq=a${r}]`).getBoundingClientRect().height)")
    assert max(heights) - min(heights) < 1


def test_click_shows_targets_then_moves_and_engine_replies(ui):
    ui.new_game()
    ui.click("g1")
    assert ui.targets() == ["f3", "h3"]
    assert "sel" in ui.classes("g1")
    ui.click("f3")
    ui.wait()
    s = ui.state()
    assert s["san"][0] == "Nf3" and len(s["san"]) == 2 and s["turn"] == "w"
    assert ui.piece("f3") == "N" and ui.piece("g1") is None
    # the engine's move is highlighted as the last move
    last = s["last"]
    assert "last" in ui.classes(last[:2]) and "last" in ui.classes(last[2:4])
    assert ui.moves_text().startswith("1. Nf3")


def test_drag_to_move(ui):
    ui.new_game()
    ui.move_drag("e2", "e4")
    assert ui.state()["san"][0] == "e4" and ui.piece("e4") == "P" and ui.piece("e2") is None


def test_illegal_drop_snaps_back(ui):
    ui.new_game()
    ui.move_drag("e2", "e5")
    assert ui.state()["san"] == [] and ui.piece("e2") == "P" and ui.piece("e5") is None
    assert ui.page.locator(".ghost").count() == 0


def test_drop_off_board_cancels(ui):
    ui.new_game()
    m = ui.page.mouse
    m.move(*ui.center("e2")); m.down(); m.move(5, 5, steps=5); m.up()
    assert ui.state()["san"] == [] and ui.piece("e2") == "P"
    assert ui.page.locator(".ghost").count() == 0


def test_cannot_select_engines_pieces(ui):
    ui.new_game()
    ui.click("e7")
    assert ui.targets() == [] and "sel" not in ui.classes("e7")


def test_click_again_deselects_and_switching_selection(ui):
    ui.new_game()
    ui.click("e2")
    assert ui.targets() == ["e3", "e4"]
    ui.click("d2")
    assert ui.targets() == ["d3", "d4"]
    ui.click("d2")
    assert ui.targets() == []
    ui.click("b1"); ui.click("h5")  # empty, unreachable square clears the selection
    assert ui.targets() == [] and ui.state()["san"] == []


def test_capture_targets_are_ringed(ui):
    ui.new_game("4k3/8/8/3p4/4P3/8/8/4K3 w - - 0 1")
    ui.click("e4")
    assert "occ" in ui.classes("d5") and "occ" not in ui.classes("e5")


# ---------------------------------------------------------------- special moves

def test_castle_kingside_by_drag(ui):
    ui.new_game("r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1")
    ui.click("e1")
    assert ui.targets() == ["c1", "d1", "f1", "g1"]
    ui.click("e1")
    ui.move_drag("e1", "g1")
    assert ui.state()["san"][0] == "O-O"
    assert (ui.piece("g1"), ui.piece("f1"), ui.piece("h1"), ui.piece("e1")) == ("K", "R", None, None)


def test_castle_queenside_by_click(ui):
    ui.new_game("r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1")
    ui.move_click("e1", "c1")
    assert ui.state()["san"][0] == "O-O-O"
    assert (ui.piece("c1"), ui.piece("d1"), ui.piece("a1")) == ("K", "R", None)


def test_castle_as_black(ui):
    ui.new_game("r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R b KQkq - 0 1", color="b")
    ui.move_drag("e8", "g8")
    assert ui.state()["san"][0] == "O-O"
    assert (ui.piece("g8"), ui.piece("f8"), ui.piece("h8")) == ("k", "r", None)


@pytest.mark.parametrize("fen", [
    "4k3/8/8/8/2b5/8/8/4K2R w K - 0 1",    # f1 attacked
    "4k3/8/8/8/8/8/8/4K2R w - - 0 1",      # no castling rights
    "4k3/8/8/8/8/8/8/4KN1R w K - 0 1",     # f1 occupied
    "4k3/8/8/8/1b6/8/8/4K2R w K - 0 1",    # in check
])
def test_castling_not_offered_when_illegal(ui, fen):
    ui.new_game(fen)
    ui.click("e1")
    assert "g1" not in ui.targets()
    ui.move_drag("e1", "g1")
    assert ui.state()["san"] == []


def test_en_passant_white(ui):
    ui.new_game("4k3/8/8/3pP3/8/8/8/4K3 w - d6 0 1")
    ui.click("e5")
    assert ui.targets() == ["d6", "e6"]
    ui.click("d6")
    ui.wait()
    assert ui.state()["san"][0] == "exd6"
    assert ui.piece("d6") == "P" and ui.piece("d5") is None and ui.piece("e5") is None


def test_en_passant_black_on_flipped_board(ui):
    ui.new_game("4k3/8/8/8/3pP3/8/8/4K3 b - e3 0 1", color="b")
    ui.move_drag("d4", "e3")
    assert ui.state()["san"][0] == "dxe3"
    assert ui.piece("e3") == "p" and ui.piece("e4") is None
    assert ui.moves_text().startswith("1. … dxe3")


def test_en_passant_not_offered_without_ep_square(ui):
    ui.new_game("4k3/8/8/3pP3/8/8/8/4K3 w - - 0 1")
    ui.click("e5")
    assert ui.targets() == ["e6"]


@pytest.mark.parametrize("choice,piece,san", [(0, "Q", "e8=Q"), (1, "R", "e8=R"),
                                              (2, "B", "e8=B"), (3, "N", "e8=N+")])
def test_promotion_picker(ui, choice, piece, san):
    ui.new_game("8/4P1k1/8/8/8/8/8/K7 w - - 0 1")
    ui.move_drag("e7", "e8")
    promo = ui.page.locator("#promo")
    expect(promo).to_be_visible()
    assert promo.locator("button").count() == 4
    assert ui.state()["san"] == []  # nothing sent until a piece is picked
    promo.locator("button").nth(choice).click()
    ui.wait()
    expect(promo).to_be_hidden()
    assert ui.state()["san"][0] == san and ui.piece("e8") == piece


def test_capture_promotion_as_black(ui):
    ui.new_game("k7/8/8/8/8/8/3p4/2R3K1 b - - 0 1", color="b")
    ui.move_click("d2", "c1")
    promo = ui.page.locator("#promo")
    expect(promo).to_be_visible()
    alts = [promo.locator("button .pc").nth(i).get_attribute("alt") for i in range(4)]
    assert alts == ["q", "r", "b", "n"]
    promo.locator("button").nth(0).click()
    ui.wait()
    assert ui.state()["san"][0] == "dxc1=Q+" and ui.piece("c1") == "q"


def test_promotion_cancel(ui):
    ui.new_game("8/4P1k1/8/8/8/8/8/K7 w - - 0 1")
    ui.move_click("e7", "e8")
    box = ui.page.locator("#promo").bounding_box()
    ui.page.mouse.click(box["x"] + 5, box["y"] + 5)
    expect(ui.page.locator("#promo")).to_be_hidden()
    assert ui.state()["san"] == [] and ui.piece("e7") == "P"
    ui.move_click("e7", "e8")  # and promoting still works afterwards
    ui.page.locator("#promo button").nth(0).click()
    ui.wait()
    assert ui.piece("e8") == "Q"


# ---------------------------------------------------------------- game flow

def test_play_black_engine_moves_first_board_flipped(ui):
    ui.new_game(color="b")
    s = ui.state()
    assert s["human"] == "b" and len(s["san"]) == 1 and s["turn"] == "b"
    # flipped: a8 bottom-right of h1
    a8, h1 = ui.center("a8"), ui.center("h1")
    assert a8[1] > h1[1] and a8[0] > h1[0]
    expect(ui.page.locator("#bottom .name")).to_have_text("You")
    assert "flipped" in ui.page.locator("#evalbar").get_attribute("class")


def test_flip_button(ui):
    ui.new_game()
    y = ui.center("a1")[1]
    ui.page.click("#flip")
    assert ui.center("a1")[1] < y
    expect(ui.page.locator("#top .name")).to_have_text("You")
    ui.move_click("e2", "e4")  # moving still works on a flipped board
    assert ui.state()["san"][0] == "e4"


def test_takeback(ui):
    ui.new_game()
    ui.move_click("e2", "e4")
    ui.move_click("d2", "d4")
    ui.page.click("#undo")
    ui.wait()
    s = ui.state()
    assert len(s["san"]) == 2 and ui.piece("d2") == "P" and ui.piece("d4") is None
    ui.page.click("#undo")
    ui.wait()
    assert ui.state()["san"] == [] and ui.piece("e2") == "P"
    expect(ui.page.locator("#undo")).to_be_disabled()


def test_takeback_as_black_before_moving_keeps_game_going(ui):
    ui.new_game(color="b")
    ui.page.click("#undo")
    ui.wait()
    s = ui.state()
    assert len(s["san"]) == 1 and s["turn"] == "b"


def test_you_win_by_checkmate(ui):
    ui.new_game("6k1/5ppp/8/8/8/8/8/R5K1 w - - 0 1")
    ui.move_click("a1", "a8")
    assert ui.banner() == "You win by checkmate"
    ui.click("g1")  # board is frozen
    assert ui.targets() == []
    assert "check" in ui.classes("g8")


def test_engine_wins_by_checkmate(ui):
    ui.new_game("r5k1/8/8/8/8/8/5PPP/6K1 w - - 0 1", time="0.5")
    ui.move_click("g1", "h1")
    assert ui.banner() == "engine wins by checkmate"
    assert "check" in ui.classes("h1")


def test_stalemate(ui):
    ui.new_game("7k/8/8/6Q1/8/8/8/K7 w - - 0 1")
    ui.move_click("g5", "g6")
    assert ui.banner() == "Draw by stalemate"


def test_eval_bar_and_depth(ui):
    ui.new_game(time="0.5")
    ui.move_click("e2", "e4")
    s = ui.state()
    assert s["depth"] and s["depth"] > 1
    expect(ui.page.locator("#top .sub")).to_contain_text("depth")
    assert ui.page.locator("#evalnum").inner_text() != ""
    # a big material edge moves the bar well off centre
    ui.new_game("4k3/8/8/8/8/8/8/QQQ1K3 w - - 0 1", time="0.5")
    ui.move_click("a1", "a2")
    h = ui.page.evaluate("() => parseFloat(document.querySelector('#evalbar .fill').style.height)")
    assert h > 90


def test_move_list_numbering_from_fen(ui):
    ui.new_game("4k3/8/8/8/3pP3/8/8/4K3 b - e3 0 12", color="b")
    ui.move_click("d4", "e3")
    assert ui.moves_text().startswith("12. … dxe3 13.")
    expect(ui.page.locator("#moves .cur")).to_have_count(1)


def test_bad_fen_shows_error(ui):
    ui.new_game("this is not a fen")
    assert ui.banner().startswith("bad FEN")
    assert "err" in ui.page.locator("#banner").get_attribute("class")
    assert ui.state()["fen"].startswith("rnbqkbnr/pppppppp")


def test_controls_apply_to_new_game(ui):
    ui.new_game(time="0.5")
    assert ui.state()["movetime"] == 0.5
    assert "on" in ui.page.locator('#time button[data-v="0.5"]').get_attribute("class")
    ui.page.evaluate("() => { Math.random = () => 0.9; }")
    ui.new_game(color="r")
    assert ui.state()["human"] == "b"
    ui.page.evaluate("() => { Math.random = () => 0.1; }")
    ui.new_game(color="r")
    assert ui.state()["human"] == "w"


def test_reload_restores_game(ui):
    ui.new_game(color="b")
    before = ui.state()["fen"]
    ui.page.reload()
    ui.wait()
    assert ui.state()["fen"] == before
    assert "on" in ui.page.locator('#color button[data-v="b"]').get_attribute("class")


# ---------------------------------------------------------------- engines

def test_choose_engine_and_broken_engine(browser, engine_path, tmp_path):
    (tmp_path / "alpha").symlink_to(engine_path)
    (tmp_path / "beta").symlink_to(engine_path)
    write_script(tmp_path / "broken", BROKEN_ENGINE)
    from play import find_engines
    with serve(find_engines([str(tmp_path)])) as (url, game):
        ctx, page, errors = open_page(browser, url)
        ui = UI(page)
        assert page.locator("#engine option").all_inner_texts() == ["alpha", "beta", "broken"]

        ui.new_game(engine="beta")
        expect(page.locator("#top .name")).to_have_text("beta")
        ui.move_click("e2", "e4")
        assert len(ui.state()["san"]) == 2

        ui.new_game(engine="broken")
        assert ui.banner().startswith("could not start broken")
        ui.click("e2")  # no engine: the board is frozen
        assert ui.targets() == [] and ui.state()["san"] == []

        ui.new_game(engine="alpha")
        assert ui.banner() == "" and ui.state()["engine"] == "alpha"
        ui.move_click("e2", "e4")
        assert len(ui.state()["san"]) == 2
        ctx.close()
        assert errors == []


def test_version_descriptions_in_picker(browser, engine_path):
    from versions import Version
    vs = {"old": Version("old", "x", 2, "Plain alpha-beta"), "new": Version("new", "y", None, "Tuned eval")}
    with serve({"old": engine_path, "new": engine_path}, vs) as (url, game):
        ctx, page, errors = open_page(browser, url)
        ui = UI(page)
        expect(page.locator("#engine-desc")).to_contain_text("Plain alpha-beta")
        expect(page.locator("#engine-desc")).to_contain_text("depth 2 (no time control)")
        page.select_option("#engine", "new")
        expect(page.locator("#engine-desc")).to_have_text("Tuned eval")
        ui.new_game(engine="old")
        ui.move_click("e2", "e4")
        assert ui.state()["depth"] == 2
        assert page.locator("#top .name").get_attribute("title") == "Plain alpha-beta"
        ctx.close()
        assert errors == []


# ---------------------------------------------------------------- layout and assets

def test_offline_falls_back_to_glyphs(browser, server):
    ctx, page, errors = open_page(browser, server, offline_pieces=True)
    ui = UI(page)
    page.wait_for_function("() => useGlyphs")
    assert ui.glyph_or_img("e1") == "span" and ui.piece("e1") == "♚"
    ui.new_game()
    ui.move_drag("e2", "e4")
    assert ui.state()["san"][0] == "e4"
    ctx.close()
    # the aborted image requests are expected console noise here
    assert all("ERR_FAILED" in e or "Failed to load" in e for e in errors)


def test_piece_images_load_from_pinned_commit(ui):
    src = ui.page.locator('[data-sq="e1"] img').get_attribute("src")
    assert src.startswith("https://cdn.jsdelivr.net/gh/lichess-org/lila@") and src.endswith("/wK.svg")
    assert "@master" not in src


def test_phone_layout(browser, server):
    ctx, page, errors = open_page(browser, server, viewport=(375, 812))
    ui = UI(page)
    assert page.evaluate("() => document.documentElement.scrollWidth") <= 375
    box = page.locator("#board").bounding_box()
    assert box["x"] >= 0 and box["x"] + box["width"] <= 375
    ui.new_game()
    ui.move_drag("e2", "e4")
    assert ui.state()["san"][0] == "e4"
    ctx.close()
    assert errors == []


def test_dark_mode_has_readable_panel(browser, server):
    ctx = browser.new_context(color_scheme="dark")
    ctx.route("**/cdn.jsdelivr.net/**",
              lambda r: r.fulfill(status=200, content_type="image/svg+xml", body=STUB_SVG))
    page = ctx.new_page()
    page.goto(server)
    bg = page.evaluate("() => getComputedStyle(document.body).backgroundColor")
    fg = page.evaluate("() => getComputedStyle(document.body).color")
    assert bg != fg and bg.startswith("rgb(2")  # dark background
    ctx.close()
