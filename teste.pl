read_key(Input) :-
    get_single_char(C1),
    ( C1 = 27 ->
        catch(
            (
                alarm(0.05, throw(timeout), Id),
                get_single_char(C2),
                get_single_char(C3),
                remove_alarm(Id),
                Input = [C1, C2, C3]
            ),
            timeout,
            Input = [C1]
        )
    ;
        Input = [C1]
    ).
% Print key presses until ESC is pressed alone
key_loop :-
    read_key(Key),
    string_codes(Str, Key),
    format("Pressed: ~s", [Key]),
    ( Key = esc -> true ; key_loop ).



slice_lines(Lines, Start, End, Slice) :-
    findall(Line,
        (nth0(Index, Lines, Line), Index >= Start, Index < End),
        Slice),
    writeln(Slice).

% Set cursor to a blinking underline (might not work in all terminals)
set_cursor_style(blinking_underline) :-
    write('\e[3 q').

% Set cursor to a steady bar
set_cursor_style(bar) :-
    writeln('\e[6 q').

% Set cursor to a blinking block
set_cursor_style(blinking_block) :-
    write('\e[1 q').

% Set cursor to a steady block (default in most terminals)
set_cursor_style(block) :-
    write('\e[2 q').
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
