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
  NY is max(1, Y - 1),
  NewCursor = cursor(X, NY).

move_cursor("j", Cursor, LineSizes, NewCursor) :-
  cursor(X, Y) = Cursor,
  NX is X + 1,
  NewCursor = cursor(NX, Y).

move_cursor("k", Cursor, LineSizes, NewCursor) :-
  cursor(X, Y) = Cursor,
  NX is max(1, X - 1),
  NewCursor = cursor(NX, Y).

move_cursor("l", Cursor, LineSizes, NewCursor) :-
  cursor(X, Y) = Cursor,
  NY is Y + 1,
  NewCursor = cursor(X, NY).

