:- module(renderer, [render/1]).
:- use_module('../src/Editor/extended_piece_table.pl').
:- use_module('../src/Editor/cursor.pl').

% ----- Placeholder Renderer -----
render(State) :-
  tty_clear,
  State = editor_state(Mode, PieceTable, Cursor, Viewport, _, Filename, StatusBar, CommandBuffer, _, _, _, CopyBuffer, SearchBuffer),
  render_viewport(PieceTable, Viewport),
  render_status_bar(Mode, Viewport, Cursor, PieceTable, Filename, StatusBar, CommandBuffer, CopyBuffer, SearchBuffer),
  render_cursor(Mode, Cursor, Viewport),
  flush_output.

render_status_bar(Mode, Viewport, Cursor, PieceTable, Filename, StatusBar, CommandBuffer, CopyBuffer, SearchBuffer) :-
  viewport(Rows, Columns, _, _) = Viewport,
  cursor(X,Y) = Cursor,
  move_cursor_to(1, Rows),
  write(Mode), write(" | "),
  write("("), write(X), write(","), write(Y), write(")"), write(" | "),
  piece_table(Pieces, OriginalBuffer, AddBuffer, InsertBuffer, InsertIndex, LineSizes) = PieceTable,
  status_bar(Status, Message) = StatusBar,
  (Status == exception -> write(Message), write(" | ")
  ; Filename == "" -> write("Path: None | ") 
  ; write("Path: "), write(Filename), write(" | ")
  ),
  ( Mode == command -> write(":"), write(CommandBuffer), write(" | Copy: ") ; write("Copy: ")),
  write(CopyBuffer), write(" | Search: "),
  write(SearchBuffer), write(" | "),
  % write(Pieces), write(" | "),
  % write(LineSizes), write(" | "), 
  % write(InsertIndex), write(" | "),
  % write(Viewport), write(" | "),
  % write(InsertBuffer), write(" | "),
  write(Rows), write("x"), write(Columns), write(" | "),
  get_line_progress(PieceTable, Cursor, Progress),
  write(Progress).

render_cursor(Mode, cursor(X, Y), viewport(TotalRows, TotalCols, InitialRow, InitialCol)) :-
  DisplayRow is min(X - InitialRow, TotalRows - 2),
  DisplayCol is min(Y - InitialCol, TotalCols - 1),
  format("\e[~d;~dH", [DisplayRow + 1, DisplayCol + 1]),

  apply_cursor_visibility(Mode),
  cursor_style(Mode, AnsiCode),
  write(AnsiCode),
  flush_output.

apply_cursor_visibility(Mode) :-
  cursor_visibility(Mode, Visibility),
  visibility_code(Visibility, Code),
  write(Code),
  flush_output.

visibility_code(hidden, "\033[?25l").
visibility_code(visible, "\033[?25h").

cursor_visibility(command, hidden).
cursor_visibility(_,       visible).

cursor_style(normal,       "\e[1 q").
cursor_style(visual,       "\e[1 q").
cursor_style(replace,      "\e[1 q").
cursor_style(insert,       "\e[5 q").
cursor_style(command,      "\e[5 q").
cursor_style(substitution, "\e[5 q").
cursor_style(_,            "\e[5 q").

get_line_progress(piece_table(_, _, _, _, _, LineSizes), cursor(X, _), "Top") :- X =:= 0, !.
get_line_progress(piece_table(_, _, _, _, _, LineSizes), cursor(X, _), "Bot") :- length(LineSizes, Len), LastIndex is Len - 1, X =:= LastIndex, !.
get_line_progress(piece_table(_, _, _, _, _, LineSizes), cursor(X, _), Progress) :-
  length(LineSizes, Len),
  LineNum is X + 1,
  Percent is (LineNum * 100) // Len,
  format(string(Progress), "~w%", [Percent]).

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
  split_string(Str, "\r\n", "", AllLines),
  length(AllLines, NumLines),
  StartRowIndex is InitialRow,
  EndRowIndex is min(StartRowIndex + (TotalRows - 1), NumLines),
  slice_lines(AllLines, StartRowIndex, EndRowIndex, VisibleLines),
  length(VisibleLines, RenderedLines),
  RemainingLines is (TotalRows - 1) - RenderedLines,
  print_lines(VisibleLines, InitialColumn, TotalColumns, RemainingLines).
 
print_lines([], _, TotalColumns, 0) :- !.
print_lines([], _, TotalColumns, Remaining) :-
  writeln("~"),
  NewRemaining is Remaining - 1,
  print_lines([], _, TotalColumns, NewRemaining).
print_lines([Line|Rest], InitialColumn, TotalColumns, Remaining) :-
  drop_string(InitialColumn, Line, Dropped),
  TotalColumnsMinusOne is TotalColumns - 1,
  take_string(TotalColumnsMinusOne, Dropped, Visible),
  writeln(Visible),
  print_lines(Rest, InitialColumn, TotalColumns, Remaining).

slice_lines(Lines, Start, End, Slice) :-
    findall(Line,
        (nth0(Index, Lines, Line), Index >= Start, Index < End),
        Slice).
