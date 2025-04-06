:- use_module(library(tty)).

start :-
    tty_clear(),
    loop(5, 5),
    halt.

loop(X, Y) :-
    move_cursor_to(X, Y),
    get_single_char(Code),
    clear_cursor(X, Y),
    handle_input(Code, X, Y, NewX, NewY),
    loop(NewX, NewY).

% Apaga o cursor anterior (colocando espaço)
clear_cursor(X, Y) :-
    format("\e[~d;~dH ", [Y, X]),  % vai até posição e escreve espaço
    flush_output.

% Move cursor do terminal para posição (X, Y)
move_cursor_to(X, Y) :-
    format("\e[~d;~dH", [Y, X]),
    flush_output.

% h/j/k/l = 104/106/107/108  | q = 113
handle_input(104, X, Y, NX, Y) :- NX is max(1, X - 1). % h
handle_input(108, X, Y, NX, Y) :- NX is X + 1.         % l
handle_input(107, X, Y, X, NY) :- NY is max(1, Y - 1). % k
handle_input(106, X, Y, X, NY) :- NY is Y + 1.         % j
handle_input(113, _, _, _, _) :- halt.     % q = quit
handle_input(_, X, Y, X, Y).  % outras teclas = sem ação

