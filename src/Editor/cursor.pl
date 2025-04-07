:- module(cursor, [
  clear_cursor/2, 
  move_cursor_to/2, 
  move_cursor/5
]).

% Apaga o cursor anterior (colocando espaço)
clear_cursor(X, Y) :-
    format("\e[~d;~dH ", [Y, X]),  % vai até posição e escreve espaço
    flush_output.

% Move cursor do terminal para posição (X, Y)
move_cursor_to(X, Y) :-
    format("\e[~d;~dH", [Y, X]),
    flush_output.

% Left
move_cursor("h", Cursor, LineSizes, IsInsertMode, NewCursor) :-
  cursor(X, Y) = Cursor,
  NY is max(0, Y - 1),
  NewCursor = cursor(X, NY).

% Left Arrow
move_cursor("\e[D", Cursor, LineSizes, IsInsertMode, NewCursor) :-
  cursor(X, Y) = Cursor,
  NY is max(0, Y - 1),
  NewCursor = cursor(X, NY).

% Down
move_cursor("j", Cursor, LineSizes, IsInsertMode, NewCursor) :-
  cursor(X, Y) = Cursor,
  length(LineSizes, LLS),
  NX is min(LLS - 1, X + 1),
  nth0(NX, LineSizes, MaxY),
  NY is max(0, min(Y, MaxY - IsInsertMode)),
  NewCursor = cursor(NX, NY).

% Down Arrow
move_cursor("\e[B", Cursor, LineSizes, IsInsertMode, NewCursor) :-
  cursor(X, Y) = Cursor,
  length(LineSizes, LLS),
  NX is min(LLS - 1, X + 1),
  nth0(NX, LineSizes, MaxY),
  NY is max(0, min(Y, MaxY - IsInsertMode)),
  NewCursor = cursor(NX, NY).

% Up
move_cursor("k", Cursor, LineSizes, IsInsertMode, NewCursor) :-
  cursor(X, Y) = Cursor,
  NX is max(0, X - 1),
  nth0(NX, LineSizes, MaxY),
  NY is max(0, min(Y, MaxY - IsInsertMode)),
  NewCursor = cursor(NX, NY).

% Up Arrow
move_cursor("\e[A", Cursor, LineSizes, IsInsertMode, NewCursor) :-
  cursor(X, Y) = Cursor,
  NX is max(0, X - 1),
  nth0(NX, LineSizes, MaxY),
  NY is max(0, min(Y, MaxY - IsInsertMode)),
  NewCursor = cursor(NX, NY).

% Right
move_cursor("l", Cursor, LineSizes, IsInsertMode, NewCursor) :-
  cursor(X, Y) = Cursor,
  TempY is Y + 1,
  nth0(X, LineSizes, MaxY),
  NY is max(0, min(TempY, MaxY - IsInsertMode)),
  NewCursor = cursor(X, NY).

% Right Arrow
move_cursor("\e[C", Cursor, LineSizes, IsInsertMode, NewCursor) :-
  cursor(X, Y) = Cursor,
  TempY is Y + 1,
  nth0(X, LineSizes, MaxY),
  NY is max(0, min(TempY, MaxY - IsInsertMode)),
  NewCursor = cursor(X, NY).

% Enter
move_cursor("\r", Cursor, LineSizes, IsInsertMode, NewCursor) :-
  cursor(X, Y) = Cursor,
  NX is X + 1,
  NewCursor = cursor(NX, 0).

% Backspace
move_cursor("\u007F", cursor(0, 0), LineSizes, _, cursor(0, 0)).

move_cursor("\u007F", cursor(X, 0), LineSizes, _, NewCursor) :-
  X =\= 0,
  NX is max(0, X - 1),
  nth0(NX, LineSizes, PreviousLine),
  NY is PreviousLine,
  NewCursor = cursor(NX, NY).

move_cursor("\u007F", cursor(X,Y), LineSizes, _, NewCursor) :-
  Y =\= 0,
  NY is Y - 1,
  NewCursor = cursor(X, NY).

% Others
move_cursor(_, Cursor, LineSizes, 1, Cursor).

move_cursor(_, Cursor, LineSizes, 0, NewCursor) :-
  cursor(X, Y) = Cursor,
  TempY is Y + 1,
  nth0(X, LineSizes, MaxY),
  NY is max(0, min(TempY, MaxY - IsInsertMode)),
  NewCursor = cursor(X, NY).
