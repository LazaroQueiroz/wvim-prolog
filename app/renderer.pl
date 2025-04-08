:- module(renderer, [render/1]).
:- use_module('../src/Editor/extended_piece_table.pl').
:- use_module('../src/Editor/cursor.pl').

% ----- Placeholder Renderer -----
render(State) :-
  State = editor_state(Mode, PieceTable, Cursor, Viewport, _, Filename, StatusBar, _, _, _, _, _, _),
  render_viewport(PieceTable, Viewport),
  render_status_bar(Mode, Viewport, Cursor, PieceTable, Filename),
  render_cursor(Cursor, Viewport),
  flush_output.

render_status_bar(Mode, Viewport, Cursor, PieceTable, Filename) :-
  viewport(Rows, Columns, _, _) = Viewport,
  move_cursor_to(1, Rows),
  write(Mode), write(" | "),
  write(Cursor), write(" | "),
  piece_table(Pieces, OriginalBuffer, AddBuffer, InsertBuffer, InsertIndex, LineSizes) = PieceTable,
  % write(Pieces), write(" | "),
  write(LineSizes), write(" | "), 
  write(InsertIndex), write(" | "),
  write(Viewport), write(" | "),
  % write(InsertBuffer), write(" | "),
  write(Rows), write(" | "), write(Columns).

render_cursor(Cursor, viewport(Rows, Columns, InitialRow, InitialColumn)) :-
    cursor(X, Y) = Cursor,
    NX is min(Rows, (X + 1) - InitialRow),
    NY is min(Columns - 2, (Y + 1) - InitialColumn),
    move_cursor_to(NY, NX),
    flush_output.

% render_viewport(PieceTable, Viewport) :-
%     move_cursor_to(1, 1),
%     extended_piece_table_to_string(PieceTable, Str),
%     split_string(Str, "\r", "", Lines),
%     forall(member(Line, Lines), writeln(Line)).
%
take_string(N, String, Result) :-
    string_chars(String, Chars),
    take_chars(N, Chars, Taken),
    string_chars(Result, Taken).

take_chars(0, _, []) :- !.
take_chars(_, [], []) :- !.
take_chars(N, [H|T], [H|Rest]) :-
    N > 0,
    N1 is N - 1,
    take_chars(N1, T, Rest).

drop_string(N, String, Result) :-
    string_chars(String, Chars),
    drop_chars(N, Chars, Dropped),
    string_chars(Result, Dropped).

drop_chars(0, L, L) :- !.
drop_chars(_, [], []) :- !.
drop_chars(N, [_|T], Rest) :-
    N > 0,
    N1 is N - 1,
    drop_chars(N1, T, Rest).

     
render_viewport(PieceTable, viewport(TotalRows, TotalColumns, InitialRow, InitialColumn)) :-
    move_cursor_to(1, 1),
    extended_piece_table_to_string(PieceTable, Str),
    split_string(Str, "\r", "", AllLines),
    length(AllLines, NumLines),
    StartRowIndex is InitialRow,
    EndRowIndex is min(StartRowIndex + (TotalRows - 1), NumLines),
    % Get only the lines from InitialRow to the viewport's height
    slice_lines(AllLines, StartRowIndex, EndRowIndex, ViewportLines),
    % For each line, print from InitialColumn up to TotalColumns
    InitialColumnMinusOne is InitialColumn,
    move_cursor_to(1, 1),
    AvailableColumns is TotalColumns - 2,
    forall(member(Line, ViewportLines), (
        drop_string(InitialColumn, Line, AuxiliaryVisibleLine),
        take_string(AvailableColumns, AuxiliaryVisibleLine, VisibleLine),
        writeln(VisibleLine)
    )).

% Helper predicate to slice a list from Start to End (exclusive)
slice_lines(Lines, Start, End, Slice) :-
    findall(Line,
        (nth0(Index, Lines, Line), Index >= Start, Index < End),
        Slice).
