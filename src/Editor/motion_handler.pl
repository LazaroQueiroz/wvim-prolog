:- module(motion_handler, [
    handle_motion/3,
    run_command/4,
    run_motion/3,
    split_num_cmd/3,
    span_digits/3,
    check_last_line_char/2,
    paste_copy_buffer/2,
    create_new_line/2,
    remove_line/2,
    replace_char/3,
    delete_char/2,
    undo_editorstate/2,
    redo_editorstate/2,
    move_to_start_of_line/2,
    move_to_end_of_line/2,
    move_to_next_word/2,
    move_to_previous_word/2,
    move_to_next_regex_occurence/2,
    move_to_previous_regex_occurence/2,
    iterate_to_next_regex_occurence/4,
    iterate_to_previous_regex_occurence/5,
    iterate_to_next_blank_space/3,
    iterate_to_previous_word_start/4,
    getRemainingInput/2,
    split_list_at/4
]).

:- use_module('editorState.pl').
:- use_module('extended_piece_table.pl').
:- use_module('mode_manager.pl').

% ===============================
% PROLOG: Motion Handler
% ===============================

% Input check-up
handle_motion(State, Input, NewState) :-
    %[FirstChar | Rest] = Input,
    %valid_motion_char(FirstChar),
    %Command = [FirstChar | Rest],
    split_num_cmd(Input, Multiplier, Motion),
    run_command(State, Multiplier, Motion, NewState).

split_num_cmd(Chars, Number, RestChars) :-
  span_digits(Chars, NumChars, RestChars),
    ( NumChars = [] -> Number = 1
    ; number_chars(Number, NumChars)
    ).

span_digits([], [], []).
span_digits([H|T], [H|Ds], Rest) :-
    char_type(H, digit),
    span_digits(T, Ds, Rest).
span_digits([H|T], [], [H|T]) :-
    \+ char_type(H, digit).

run_command(State, 0, _, State).
run_command(State, Multiplier, Motion, NewState) :-
    run_motion(State, Motion, NextState),
    NextMultiplier is Multiplier - 1,
    run_command(NextState, NextMultiplier, Motion, NewState).

move_to_end_of_line(editor_state(M, PT, cursor(X, Y), V, FS, FN, SB, CB, U, R, VS, Copy, Search), NewState) :-
    PT = piece_table(Pieces, OriginalBuffer, AddBuffer, InsertBuffer, InsertStartIndex, LinesSizes),
    nth0(X, LinesSizes, LineSize),
    Y1 is LineSize - 1,
    NewCursor = cursor(X, Y1),
    NewState = editor_state(M, PT, NewCursor, V, FS, FN, SB, CB, U, R, VS, Copy, Search).

move_to_start_of_line(editor_state(M, PT, cursor(X, Y), V, FS, FN, SB, CB, U, R, VS, Copy, Search), NewState) :-
    NewCursor = cursor(X, 0),
    NewState = editor_state(M, PT, NewCursor, V, FS, FN, SB, CB, U, R, VS, Copy, Search).

create_new_line(CurrentState, NewState) :-
    CurrentState = editor_state(M, PT, C, V, FS, FN, SB, CB, Undo, Redo, VS, Copy, Search),
    add_to_undo_stack(CurrentState, Undo, NewUndo),
    PT = piece_table(Pieces, Orig, Add, Insert, OldInsertIndex, LineSizes),
    C = cursor(X, Y),
    nth0(X, LineSizes, LineLen),
    NewLineLen is LineLen + 1, % Deveria inserir após o último caractere da linha, mas faz 2 após. (???)
    Split is X + 1,
    NewCursor = cursor(X, NewLineLen),
    cursor_xy_to_string_index(NewCursor, LineSizes, 0, 0, InsertIndex),
    string_concat(Insert, "\n", NewInsert),
    insert_text(piece_table(Pieces, Orig, Add, NewInsert, InsertIndex, LineSizes), NewPT),
    split_list_at(Split, LineSizes, Left, Right),
    append(Left, [0], LeftZero),
    append(LeftZero, Right, UpdatedLines),
    NewPT = piece_table(P1, O1, A1, "", NewIndex, OldLinesSizes),
    FinalPT = piece_table(P1, O1, A1, "", NewIndex, UpdatedLines),
    NewX is X + 1, % Vai para a linha abaixo do cursor onde foi pressionado "o"
    FinalCursor = cursor(NewX, 0),
    NewState = editor_state(insert, FinalPT, FinalCursor, V, FS, FN, SB, CB, NewUndo, [], VS, Copy, Search).

split_list_at(Index, List, Left, Right) :-
    length(Left, Index),
    append(Left, Right, List).

run_motion(State, "w", NewState) :- move_to_next_word(State, NewState), !.
run_motion(State, "b", NewState) :- move_to_previous_word(State, NewState), !.
run_motion(State, [$], NewState) :- move_to_end_of_line(State, NewState), !.
run_motion(State, [^], NewState) :- move_to_start_of_line(State, NewState), !.
run_motion(State, "dd", NewState):- remove_line(State, NewState), !.
run_motion(State, [x], NewState) :- delete_char(State, NewState), !.
run_motion(State, [o], NewState) :- create_new_line(State, NewState), !.
run_motion(State, "u", NewState) :- undo_editorstate(State,NewState), !.
run_motion(State, "t", NewState) :- redo_editorstate(State,NewState), !.
run_motion(State, "p", NewState) :- paste_copy_buffer(State,NewState), !.
run_motion(State, "n", NewState) :- move_to_next_regex_occurence(State,NewState), !.
run_motion(State, "N", NewState) :- move_to_previous_regex_occurence(State, NewState), !.
run_motion(State, Motion, NewState) :-
    atom_chars(Motion, [Head | MotionChars]),
    last(MotionChars, Last),
    ( Head == "r" -> replace_char(State, Last, NewState); NewState = State), !.

delete_char(CurrentState, CurrentState) :-
    CurrentState = editor_state(Mode, PT, Cursor, View, FS, FN, SB, CB, U, R, VS, CopyText, Search),
    cursor(X, Y) = Cursor,
    PT = piece_table(_, _, _, _, _, LinesSizes),
    nth0(X, LinesSizes, LineSize),
    LineSize =:= 0
    , !.

delete_char(CurrentState, NewState) :-
    CurrentState = editor_state(M, PT, cursor(X, Y), V, FS, FN, SB, CB, Undo, Redo, VS, Copy, Search),
    PT = piece_table(Pieces, Orig, Add, Insert, Index, LineSizes),
    add_to_undo_stack(CurrentState, Undo, NewUndo),
    cursor_xy_to_string_index(cursor(X, Y), LineSizes, 0, 0, DeleteStartIndex),
    DeleteIndex is DeleteStartIndex + 1,
    delete_text(DeleteIndex, 1, PT, piece_table(NewPieces, Orig, NewAdd, NewInsert, NewIndex, LineSizes1)),
 
    NewY is Y + 1,
    update_lines_sizes("\u007F", cursor(X, NewY), LineSizes1, NewLineSizes),
    nth0(X, LineSizes, LineSize),
    ( Y >= LineSize - 1 -> NewCursorY is max(0, Y - 1), NewCursor = cursor(X, NewCursorY) ; NewCursor = cursor(X, Y)
    ),
    NewState = editor_state(M, piece_table(NewPieces, Orig, NewAdd, NewInsert, NewIndex, NewLineSizes), NewCursor, V, FS, FN, SB, CB, NewUndo, Redo, VS, Copy, Search).

pasteCopyBuffer(editor_state(M, PT, C, V, FS, FN, SB, CB, Undo, Redo, VS, Copy, Search), NewState) :-
    add_to_undo_stack(editor_state(M, PT, C, V, FS, FN, SB, CB, Undo, Redo, VS, Copy, Search), Undo, NewUndo),
    PT = piece_table(Pieces, Orig, Add, Insert, Index, LineSizes),
    cursor_xy_to_string_index(C, LineSizes, 0, 0, InsertIndex),
    string_concat(Insert, Copy, NewInsert),
    insert_text(piece_table(Pieces, Orig, Add, NewInsert, InsertIndex, LineSizes), NewPT),
    NewState = editor_state(M, NewPT, C, V, FS, FN, SB, CB, NewUndo, [], VS, Copy, Search).

replaceChar(editor_state(M, PT, C, V, FS, FN, SB, CB, Undo, Redo, VS, Copy, Search), Char, NewState) :-
    add_to_undo_stack(editor_state(M, PT, C, V, FS, FN, SB, CB, Undo, Redo, VS, Copy, Search), Undo, NewUndo),
    PT = piece_table(Pieces, Orig, Add, Insert, _, LineSizes),
    cursor_xy_to_string_index(C, LineSizes, 0, 0, Index),
    delete_text(Index, 1, PT, TempPT),
    TempPT = piece_table(P2, O2, A2, Insert2, _, LS2),
    string_concat(Insert2, Char, NewInsert),
    insert_text(piece_table(P2, O2, A2, NewInsert, Index, LS2), NewPT),
    C = cursor(X, Y),
    FinalCursor = cursor(X, Y+1),
    NewState = editor_state(M, NewPT, FinalCursor, V, FS, FN, SB, CB, NewUndo, [], VS, Copy, Search).

undo_editor_state(CurrentState, NewState) :-
    CurrentState = editor_state(_, _, _, _, _, _, _, _, UndoStack, RedoStack, _, _, _),
    ( UndoStack = [] -> NewState = CurrentState
    ; append(NewUndoStack, [LastUndo], UndoStack),
    add_to_redo_stack(CurrentState, RedoStack, NewRedoStack),
    LastUndo = editor_state(_, PT, C, V, FS, FN, SB, CB, _, _, VS, Copy, Search),
    NewState = editor_state(normal, PT, C, V, FS, FN, SB, CB, NewUndoStack, NewRedoStack, VS, Copy, Search)
    ).

redo_editor_state(CurrentState, NewState) :-
    CurrentState = editor_state(_, _, _, _, _, _, _, _, UndoStack, RedoStack, _, _, _),
    ( RedoStack = [] -> NewState = CurrentState
    ; RedoStack = [RedoHead | RedoTail],    
    add_to_undo_stack(CurrentState, UndoStack, NewUndoStack),
    RedoHead = editor_state(_, PT, C, V, FS, FN, SB, CB, _, _, VS, Copy, Search),
    NewState = editor_state(normal, PT, C, V, FS, FN, SB, CB, NewUndoStack, RedoTail, VS, Copy, Search)
    ).
