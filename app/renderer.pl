:- module(renderer, [render/2]).
:- use_module('../src/Editor/extended_piece_table.pl').
:- use_module('../src/Editor/cursor.pl').

% ----- Placeholder Renderer -----
render(State, DebugMode) :-
  (DebugMode =:= 0 -> tty_clear; write("")),
    State = editor_state(Mode, PieceTable, Cursor, Viewport, _, Filename, StatusBar, _, _, _, _, _, _),
    render_viewport(PieceTable, Viewport),
    render_status_bar(Mode, Viewport, Cursor, PieceTable, Filename),
    render_cursor(Cursor, Viewport),
    flush_output.

render_status_bar(Mode, Viewport, Cursor, PieceTable, Filename) :-
  viewport(Rows, Columns, _, _) = Viewport,
  % write("\t\t\t\t\t\t\t\t\t this are the row: "), writeln(Rows),
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
    InitialColumnMinusOne is max(0, InitialColumn),
    move_cursor_to(1, 1),
    forall(member(Line, ViewportLines), (
        string_length(Line, LineLength),
        TotalColumnsForTheLine is min(TotalColumns - 2, LineLength),
        sub_string(Line, InitialColumnMinusOne, TotalColumnsForTheLine, _, VisibleLine),
        writeln(VisibleLine)
    )),
    write("\n\n\n\n\n\nFinish the loop (render viewport)").

% Helper predicate to slice a list from Start to End (exclusive)
slice_lines(Lines, Start, End, Slice) :-
    findall(Line,
        (nth0(Index, Lines, Line), Index >= Start, Index < End),
        Slice).
