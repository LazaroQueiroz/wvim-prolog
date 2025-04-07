:- module(cursor, [clear_cursor/2, move_cursor_to/2, move_cursor/4]).

% Apaga o cursor anterior (colocando espaço)
clear_cursor(X, Y) :-
    format("\e[~d;~dH ", [Y, X]),  % vai até posição e escreve espaço
    flush_output.

% Move cursor do terminal para posição (X, Y)
move_cursor_to(X, Y) :-
    format("\e[~d;~dH", [Y, X]),
    flush_output.

move_cursor("h", Cursor, LineSizes, NewCursor) :-
  cursor(X, Y) = Cursor,
  NY is max(0, Y - 1),
  NewCursor = cursor(X, NY).

move_cursor("j", Cursor, LineSizes, NewCursor) :-
  cursor(X, Y) = Cursor,
  NX is X + 1,
  NewCursor = cursor(NX, Y).

move_cursor("k", Cursor, LineSizes, NewCursor) :-
  cursor(X, Y) = Cursor,
  NX is max(0, X - 1),
  NewCursor = cursor(NX, Y).

move_cursor("l", Cursor, LineSizes, NewCursor) :-
  cursor(X, Y) = Cursor,
  NY is Y + 1,
  NewCursor = cursor(X, NY).

move_cursor("\r", Cursor, LineSizes, NewCursor) :-
  cursor(X, Y) = Cursor,
  NX is X + 1,
  NewCursor = cursor(NX, 0).

move_cursor("\u007F", Cursor, LineSizes, NewCursor) :-
  cursor(X, Y) = Cursor,
  Y =\= 0,
  NY is Y - 1,
  NewCursor = cursor(X, NY).

move_cursor("\u007F", Cursor, LineSizes, NewCursor) :-
  cursor(X, Y) = Cursor,
  Y =:= 0,
  NX is max(0, X - 1),
  PreviousLine = nth0(NX, LineSizes),
  NY is PreviousLine - 1,
  NewCursor = cursor(NX, NY).

move_cursor(_, Cursor, LineSizes, NewCursor) :-
  cursor(X, Y) = Cursor,
  NY is Y + 1,
  NewCursor = cursor(X, NY).

