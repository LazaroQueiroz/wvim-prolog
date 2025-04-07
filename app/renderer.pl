:- module(renderer, [render/1]).
:- use_module('../src/Editor/extended_piece_table.pl').
:- use_module('../src/Editor/cursor.pl').

% ----- Placeholder Renderer -----
render(State) :-
    % tty_clear,
    State = editor_state(Mode, PieceTable, Cursor, Viewport, _, Filename, StatusBar, _, _, _, _, _, _),
    render_status_bar(Mode, Viewport, Cursor, PieceTable, Filename),
    render_viewport(PieceTable, Viewport),
    render_cursor(Cursor),
    flush_output.

render_status_bar(Mode, Viewport, Cursor, PieceTable, Filename) :-
  [Rows, Columns, _, _] = Viewport,
  move_cursor_to(1, Rows),
  write(Mode), write(" | "),
  write(Cursor), write(" | "),
  piece_table(Pieces, OriginalBuffer, AddBuffer, InsertBuffer, InsertIndex, LineSizes) = PieceTable,
  write(LineSizes), write(" | "), 
  write(InsertIndex), write(" | "),
  write(Rows), write(" | "), write(Columns).

render_cursor(Cursor) :-
    cursor(X, Y) = Cursor,
    NX is X + 1,
    NY is Y + 1,
    move_cursor_to(NY, NX),
    flush_output.

render_viewport(PieceTable, Viewport) :-
    move_cursor_to(1, 1),
    extended_piece_table_to_string(PieceTable, Str),
    split_string(Str, "\r", "", Lines),
    forall(member(Line, Lines), writeln(Line)).
     

