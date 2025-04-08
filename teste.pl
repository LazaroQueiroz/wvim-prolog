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

