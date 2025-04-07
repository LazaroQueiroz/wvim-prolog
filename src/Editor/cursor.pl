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

move_cursor("\e[D", Cursor, LineSizes, NewCursor) :-
  cursor(X, Y) = Cursor,
  NY is max(0, Y - 1),
  NewCursor = cursor(X, NY).

move_cursor("j", Cursor, LineSizes, NewCursor) :-
  cursor(X, Y) = Cursor,
  NX is X + 1,
  NewCursor = cursor(NX, Y).

move_cursor("\e[B", Cursor, LineSizes, NewCursor) :-
  cursor(X, Y) = Cursor,
  NX is X + 1,
  NewCursor = cursor(NX, Y).

move_cursor("k", Cursor, LineSizes, NewCursor) :-
  cursor(X, Y) = Cursor,
  NX is max(0, X - 1),
  NewCursor = cursor(NX, Y).

move_cursor("\e[A", Cursor, LineSizes, NewCursor) :-
  cursor(X, Y) = Cursor,
  NX is max(0, X - 1),
  NewCursor = cursor(NX, Y).

move_cursor("l", Cursor, LineSizes, NewCursor) :-
  cursor(X, Y) = Cursor,
  NY is Y + 1,
  NewCursor = cursor(X, NY).

move_cursor("\e[C", Cursor, LineSizes, NewCursor) :-
  cursor(X, Y) = Cursor,
  NY is Y + 1,
  NewCursor = cursor(X, NY).

move_cursor("\r", Cursor, LineSizes, NewCursor) :-
  cursor(X, Y) = Cursor,
  NX is X + 1,
  NewCursor = cursor(NX, 0).

move_cursor("\u007F", cursor(0, 0), LineSizes, cursor(0, 0)).

move_cursor("\u007F", cursor(X, 0), LineSizes, NewCursor) :-
  X =\= 0,
  NX is max(0, X - 1),
  nth0(NX, LineSizes, PreviousLine),
  NY is PreviousLine,
  NewCursor = cursor(NX, NY).

move_cursor("\u007F", cursor(X,Y), LineSizes, NewCursor) :-
  Y =\= 0,
  NY is Y - 1,
  NewCursor = cursor(X, NY).


move_cursor(_, Cursor, LineSizes, NewCursor) :-
  cursor(X, Y) = Cursor,
  NY is Y + 1,
  NewCursor = cursor(X, NY).

