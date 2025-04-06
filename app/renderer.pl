:- module(renderer, [render/1]).
:- use_module('../src/Editor/extended_piece_table.pl').
:- use_module('../src/Editor/cursor.pl').

% ----- Placeholder Renderer -----
render(State) :-
    tty_clear,
    State = editor_state(_, PieceTable, Cursor, Viewport, _, _, _, _, _, _, _, _, _),
    render_viewport(PieceTable, Viewport),
    render_cursor(Cursor),
    flush_output.

render_cursor(Cursor) :-
    cursor(Y, X) = Cursor,
    move_cursor_to(X, Y),
    flush_output.

render_viewport(ExtendedPieceTable, Viewport) :-
    extended_piece_table_to_string(PieceTable, Str),
    split_string(Str, "\n", "", Lines),
    forall(member(Line, Lines), writeln(Line)).
     

