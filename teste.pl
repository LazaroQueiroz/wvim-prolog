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

handle_insert_mode(State, Input, NewState) :- 
  member(Input, ["\e[A", "\e[B", "\e[C", "\e[D"]),
  writeln("IS A MEMBER AIFASOHDFOIAUSHDFIOUASHDIOFUHASODIUFHIOSUDHFIOHUS"),
  State = editor_state(M, PT, C, V, FS, FN, SB, CB, U, R, VS, Copy, NewSearch),
  insert_text(PT, NewPT),
  NewPT = piece_table(Pieces, OriginalBuffer, AddBuffer, InsertBuffer, InsertStartIndex, LineSizes),
  update_editor_cursor(State, Input, AuxiliaryState),
  AuxiliaryState = editor_state(M, NewPT, NewC, V, FS, FN, SB, CB, U, R, VS, Copy, NewSearch),
  cursor_xy_to_string_index(NewC, LineSizes, 0, 0, NewInsertStartIndex),
  FinalPT = piece_table(Pieces, OriginalBuffer, AddBuffer, InsertBuffer, NewInsertStartIndex, LineSizes),
  NewState = editor_state(M, FinalPT, NewC, V, FS, FN, SB, CB, U, R, VS, Copy, NewSearch).
