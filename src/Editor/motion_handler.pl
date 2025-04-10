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
    undo_editor_state/2,
    redo_editor_state/2,
    move_to_start_of_line/2,
    move_to_end_of_line/2,
    move_to_next_word/2,
    move_to_previous_word/2,
    move_to_next_regex_occurrence/2,
    move_to_previous_regex_occurrence/2,
    iterate_to_next_regex_occurrence/4,
    iterate_to_previous_regex_occurrence/5,
    iterate_to_next_blank_space/3,
    iterate_to_previous_word_start/4,
    getRemainingInput/2,
    split_list_at/4
]).

:- use_module('editorState.pl').
:- use_module('cursor.pl').
:- use_module('extended_piece_table.pl').
:- use_module('mode_manager.pl').

% ===============================
% PROLOG: Motion Handler
% ===============================
valid_motion_char('$').
valid_motion_char('w').
valid_motion_char('b').
valid_motion_char('x').
valid_motion_char('o').
valid_motion_char('u').
valid_motion_char('t').
valid_motion_char('p').
valid_motion_char('n').
valid_motion_char('N').
valid_motion_char('^').

% Input check-up
handle_motion(State, Input, NewState) :-
    [FirstChar | Rest] = Input,
    ( valid_motion_char(FirstChar) -> 
      split_num_cmd(Input, Multiplier, Motion),
      run_command(State, Multiplier, Motion, NewState)
    ; get_remaining_input(Input, NewInput),
      split_num_cmd(NewInput, Multiplier, Motion),
      run_command(State, Multiplier, Motion, NewState)
).

get_remaining_input(Input, NewInput) :-
  [Head | Rest] = Input,
  get_single_char(Code),
  char_code(HeadChar, Code),
  ( valid_motion_char(HeadChar) ->
      append(Input, [HeadChar], NewInput)
  ; \+ char_type(HeadChar, digit),
    \+ char_type(Head, digit) ->
      append(Input, [HeadChar], NewInput)
  ; get_remaining_input([HeadChar], AllChars),
    append(Input, AllChars, NewInput)
  ).

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

undo_editor_state(editor_state(Mode, PT, Cursor, View, FS, FN, SB,CB, [], RedoStack, VS, CopyText, Search), State) :- 
  State = editor_state(Mode, PT, Cursor, View, FS, FN, SB, CB, [], RedoStack, VS, CopyText, Search),!.  % Nada para desfazer
undo_editor_state(
    editor_state(Mode, PT, Cursor, View, FS, FN, SB, CB, [PrevState | UndoTail], RedoStack, VS, CopyText, Search),
    editor_state(PrevMode, PrevPT, PrevCursor, PrevView, PrevFS, PrevFN, PrevSB, PrevCB, UndoTail, [CurrentStateToRedoStack|RedoStack], PrevVS, PrevCopyText, PrevSearch)
) :-
    CurrentState = editor_state(Mode, PT, Cursor, View, FS, FN, SB, CB, [PrevState | UndoTail], RedoStack, VS, CopyText, Search),
    CurrentStateToRedoStack = editor_state(Mode, PT, Cursor, View, FS, FN, SB, CB, [], [], VS, CopyText, Search),
    PrevState = editor_state(PrevMode, PrevPT, PrevCursor, PrevView, PrevFS, PrevFN, PrevSB, PrevCB, _, _, PrevVS, PrevCopyText, PrevSearch).

redo_editor_state(editor_state(Mode, PT, Cursor, View, FS, FN, SB,CB, UndoStack, [], VS, CopyText, Search), State) :- 
  State = editor_state(Mode, PT, Cursor, View, FS, FN, SB, CB, UndoStack, [], VS, CopyText, Search),!.  % Nada para refazer
redo_editor_state(
    editor_state(Mode, PT, Cursor, View, FS, FN, SB, CB, UndoStack, [NextState|RedoTail], VS, CopyText, Search),
    editor_state(NextMode, NextPT, NextCursor, NextView, NextFS, NextFN, NextSB, NextCB, [CurrentStateToUndoStack | UndoStack], RedoTail, NextVS, NextCopyText, NextSearch)
) :-
    CurrentState = editor_state(Mode, PT, Cursor, View, FS, FN, SB, CB, UndoStack, [NextState|RedoTail], VS, CopyText, Search),
    CurrentStateToUndoStack = editor_state(Mode, PT, Cursor, View, FS, FN, SB, CB, [], [], VS, CopyText, Search),
    NextState = editor_state(NextMode, NextPT, NextCursor, NextView, NextFS, NextFN, NextSB, NextCB, _, _, NextVS, NextCopyText, NextSearch).

paste_copy_buffer(CurrentState, NewState) :-
    CurrentState = editor_state(M, PT, C, V, FS, FN, SB, CB, Undo, Redo, VS, Copy, Search),
    add_to_undo_stack(CurrentState, Undo, NewUndo),
    PT = piece_table(Pieces, Orig, Add, Insert, Index, LineSizes),
    cursor_xy_to_string_index(C, LineSizes, 0, 0, InsertIndex),
    string_concat(Insert, Copy, NewInsert),
    insert_text(piece_table(Pieces, Orig, Add, NewInsert, InsertIndex, LineSizes), NewPT),
    NewPT = piece_table(NewPieces, Orig, NewAdd, InsertBuffer, InsertIndex, LineSizes),
    extended_piece_table_to_string(NewPT, Text),
    string_chars(Text, Chars),
    get_lines_sizes(Chars, 0, [], FinalLineSizes),
    FinalPT = piece_table(NewPieces, Orig, NewAdd, InsertBuffer, InsertIndex, FinalLineSizes),
    NewState = editor_state(M, FinalPT, C, V, FS, FN, SB, CB, NewUndo, [], VS, Copy, Search), !.

remove_line(State, NewState) :-
    State = editor_state(M, PT, Cursor, V, FS, FN, SB, CB, Undo, Redo, VS, Copy, Search),
    PT = piece_table(Pieces, Orig, Add, Insert, Index, LineSizes),
    Cursor = cursor(X, Y),
    add_to_undo_stack(State, Undo, NewUndo),
    length(LineSizes, N),
    LN is N - 1,
 
    sum_list_prefix(LineSizes, X, Sum),
    ( X =:= LN -> ExtraStart is 0
    ; ExtraStart is 1),
    LineStartIndex is Sum + X + ExtraStart,
    nth0(X, LineSizes, LineSize), !,
    ( N \= 1 -> ExtraLength is 1
    ; ExtraLength is 0),
    LineLength is LineSize + ExtraLength,
    (N == 1 -> NewLineSizes = [0]
    ; length(Prefix, X),
    append(Prefix, [_|Suffix], LineSizes),
    append(Prefix, Suffix, NewLineSizes)
    ),
    (X =:= LN -> NewX is max(0,X-1), NewCursor = cursor(NewX, 0)
    ; NewCursor = cursor(X, 0)),
    delete_text(LineStartIndex, LineLength, PT, TempPT),
    TempPT = piece_table(NewPieces, _, NewAdd, NewInsert, NewIndex, _),
    NewPT = piece_table(NewPieces, Orig, NewAdd, NewInsert, NewIndex, NewLineSizes),
    NewState = editor_state(M, NewPT, NewCursor, V, not_saved, FN, SB, CB, NewUndo, [], VS, Copy, Search).

sum_list_prefix(_, 0, 0) :- !.
sum_list_prefix([H|T], X, Sum) :-
    X > 0,
    X1 is X - 1,
    sum_list_prefix(T, X1, RestSum),
    Sum is H + RestSum.

move_to_next_word(CurrentState, NewState) :-
  CurrentState = editor_state(M, PT, C, V, FS, FN, SB, CB, Undo, Redo, VS, Copy, Search),
  PT = piece_table(Pieces, Orig, Add, Insert, Index, LineSizes),
  cursor_xy_to_string_index(C, LineSizes, 0, 0, InsertIndex),
  extended_piece_table_to_string(PT, FullText),
  drop_string(InsertIndex, FullText, RemainingText),
  string_chars(RemainingText, RemainingChars),
  iterate_to_next_blank_space(RemainingChars, C, NewCursor),
  NewState = editor_state(M, PT, NewCursor, V, FS, FN, SB, CB, Undo, Redo, VS, Copy, Search), !.

iterate_to_next_blank_space([], Cursor, Cursor) :- !.
iterate_to_next_blank_space([Char|Rest], cursor(X, Y), NewCursor) :-
  member(Char, ['\r', '\n']),
  NewX is X + 1,
  NewCursor = cursor(NewX, 0), !.
iterate_to_next_blank_space([Char|Rest], cursor(X, Y), NewCursor) :-
  Rest = [NextChar|_],
  member(Char, [' ', '\t']),
  \+ member(NextChar, [' ', '\t']),
  NewY is Y + 1,
  NewCursor = cursor(X, NewY), !.
iterate_to_next_blank_space([Char|Rest], cursor(X, Y), NewCursor) :-
  Y1 is Y + 1,
  iterate_to_next_blank_space(Rest, cursor(X, Y1), NewCursor), !.

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

move_to_previous_word(CurrentState, NewState) :-
  CurrentState = editor_state(M, PT, C, V, FS, FN, SB, CB, Undo, Redo, VS, Copy, Search),
  PT = piece_table(Pieces, Orig, Add, Insert, Index, LineSizes),
  cursor_xy_to_string_index(C, LineSizes, 0, 0, InsertIndex),
  extended_piece_table_to_string(PT, FullText),
  take_string(InsertIndex, FullText, PreviousText),
  string_chars(PreviousText, PreviousChars),
  iterate_to_previous_word_start(PreviousChars, C, LineSizes, NewCursor),
  NewState = editor_state(M, PT, NewCursor, V, FS, FN, SB, CB, Undo, Redo, VS, Copy, Search), !.

iterate_to_previous_word_start([], Cursor, _, Cursor) :- !.
iterate_to_previous_word_start([_], cursor(X, Y), _, cursor(X, Y1)) :-
  Y1 is Y - 1, !.
iterate_to_previous_word_start(Text, cursor(X, Y), LineSizes, cursor(X, Y1)) :-
  append(Init, [Prev, Last], Text),
  \+ member(Last, ['\r', '\n', '\t', ' ']),
  member(Prev, ['\r', '\n', '\t', ' ']),
  Y1 is Y - 1, !.
iterate_to_previous_word_start(Text, cursor(X, _), LineSizes, cursor(X1, Y1)) :-
  append(_, [BeforeLast, Last], Text),
  member(Last, ['\r', '\n']),
  member(BeforeLast, ['\r', '\n']),
  X1 is X - 1,
  nth0(X1, LineSizes, Y1), !.
iterate_to_previous_word_start(Text, cursor(X, _), LineSizes, NewCursor) :-
  append(Rest, [Last], Text),
  member(Last, ['\r', '\n']),
  X1 is X - 1,
  nth0(X1, LineSizes, Y1),
  iterate_to_previous_word_start(Rest, cursor(X1, Y1), LineSizes, NewCursor), !.
iterate_to_previous_word_start(Text, cursor(X, Y), LineSizes, NewCursor) :-
  append(Rest, [_], Text),
  Y1 is Y - 1,
  iterate_to_previous_word_start(Rest, cursor(X, Y1), LineSizes, NewCursor).

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

run_motion(State, [w], NewState) :- move_to_next_word(State, NewState), !.
run_motion(State, [b], NewState) :- move_to_previous_word(State, NewState), !.
run_motion(State, [$], NewState) :- move_to_end_of_line(State, NewState), !.
run_motion(State, [^], NewState) :- move_to_start_of_line(State, NewState), !.
run_motion(State, [z], NewState):- remove_line(State, NewState), !.
run_motion(State, [x], NewState) :- delete_char(State, NewState), !.
run_motion(State, [o], NewState) :- create_new_line(State, NewState), !.
run_motion(State, [u], NewState) :- undo_editor_state(State, NewState), !.
run_motion(State, [t], NewState) :- redo_editor_state(State, NewState), !.
run_motion(State, [p], NewState) :- paste_copy_buffer(State, NewState), !.
run_motion(State, [n], NewState) :- 
  move_to_next_regex_occurrence(State, NewState), !.
run_motion(State, [N], NewState) :- move_to_previous_regex_occurrence(State, NewState), !.
run_motion(State, Motion, NewState) :-
    Motion = [Head | MotionChars],
    last(MotionChars, Last),
    ( Head == 'r' -> replace_char(State, Last, NewState)
    ; Head == 'd', Last == 'd' -> remove_line(State, NewState)
    ; NewState = State), !.
run_motion(State, _, State).

move_to_next_word(CurrentState, NewState) :-
  CurrentState = editor_state(M, PT, C, V, FS, FN, SB, CB, Undo, Redo, VS, Copy, Search),
  PT = piece_table(Pieces, Orig, Add, Insert, Index, LineSizes),
  cursor_xy_to_string_index(C, LineSizes, 0, 0, InsertIndex),
  extended_piece_table_to_string(PT, FullText),
  drop_string(InsertIndex, FullText, RemainingText),
  string_chars(RemainingText, RemainingChars),
  iterate_to_next_blank_space(RemainingChars, C, NewCursor),
  NewState = editor_state(M, PT, NewCursor, V, FS, FN, SB, CB, Undo, Redo, VS, Copy, Search), !.

move_to_next_regex_occurrence(State, State) :- 
  State = editor_state(M, PT, C, V, FS, FN, SB, CB, Undo, Redo, VS, Copy, Search), 
  Search == "".
move_to_next_regex_occurrence(CurrentState, NewState) :-
  CurrentState = editor_state(M, PT, C, V, FS, FN, SB, CB, Undo, Redo, VS, Copy, Search),
  not(Search == ""),
  PT = piece_table(Pieces, Orig, Add, Insert, Index, LineSizes),
  cursor_xy_to_string_index(C, LineSizes, 0, 0, InsertIndex),
  extended_piece_table_to_string(PT, FullText),
  drop_string(InsertIndex, FullText, RemainingText),
  drop_string(1, RemainingText, RemainingTextMinusOne),
  C = cursor(X, Y),
  NY is Y + 1,
  NC = cursor(X, NY),
  (sub_string(RemainingTextMinusOne, _, _, _, Search) -> iterate_to_next_regex_occurrence(Search, RemainingTextMinusOne, NC, NewCursor) ; NewCursor = C),
  NewState = editor_state(M, PT, NewCursor, V, FS, FN, SB, CB, Undo, Redo, VS, Copy, Search), !.

iterate_to_next_regex_occurrence(_, "", Cursor, Cursor).
iterate_to_next_regex_occurrence(Regex, Text, Cursor, Cursor) :-
  string_length(Regex, Len),
  sub_string(Text, 0, Len, A, Regex).
iterate_to_next_regex_occurrence(Regex, Text, Cursor, NewCursor) :-
  cursor(X, Y) = Cursor,
  string_chars(Text, [Char | Rest]),
  string_chars(RestStr, Rest),
  ( member(Char, ['\n', '\r']) ->
      NewX is X + 1,
      iterate_to_next_regex_occurrence(Regex, RestStr, cursor(NewX, 0), NewCursor)
  ; NewY is Y + 1,
    iterate_to_next_regex_occurrence(Regex, RestStr, cursor(X, NewY), NewCursor)
  ).

move_to_previous_regex_occurrence(State, State) :- 
  State = editor_state(M, PT, C, V, FS, FN, SB, CB, Undo, Redo, VS, Copy, Search), 
  Search == "".
move_to_previous_regex_occurrence(CurrentState, NewState) :-
  CurrentState = editor_state(M, PT, C, V, FS, FN, SB, CB, Undo, Redo, VS, Copy, Search),
  not(Search == ""),
  PT = piece_table(Pieces, Orig, Add, Insert, Index, LineSizes),
  cursor_xy_to_string_index(C, LineSizes, 0, 0, InsertIndex),
  extended_piece_table_to_string(PT, FullText),
  InsertIndexMinusOne is InsertIndex - 1,
  take_string(InsertIndex, FullText, PreviousText),
  take_string(InsertIndexMinusOne, FullText, PreviousTextMinusOne),
  (sub_string(PreviousTextMinusOne, _, _, _, Search) -> iterate_to_previous_regex_occurrence(Search, PreviousText, C, NewCursor, LineSizes) ; NewCursor = C),
  NewState = editor_state(M, PT, NewCursor, V, FS, FN, SB, CB, Undo, Redo, VS, Copy, Search), !.

iterate_to_previous_regex_occurrence(_, "", Cursor, Cursor, _).
iterate_to_previous_regex_occurrence(Regex, Text, Cursor, NewCursor, _) :-
  Cursor = cursor(X, Y),
  string_length(Regex, Len),
  string_length(Text, TextLen),
  PreviousStartIndex is TextLen - Len,
  sub_string(Text, PreviousStartIndex, Len, A, Regex),
  NY is max(0, Y - Len),
  NewCursor = cursor(X, NY).
iterate_to_previous_regex_occurrence(Regex, Text, Cursor, NewCursor, LineSizes) :-
  cursor(X, Y) = Cursor,
  string_chars(Text, Chars),
  last(Chars, LastChar),
  append(Init, [LastChar], Chars),
  string_chars(InitStr, Init),
  ( member(LastChar, ['\n', '\r']) ->
      NewX is max(0, X - 1),
      nth0(NewX, LineSizes, AboveLineSize),
      AboveLineSizeMinusOne is max(0, AboveLineSize - 1),
      NewY is AboveLineSize,
      iterate_to_previous_regex_occurrence(Regex, InitStr, cursor(NewX, NewY), NewCursor, LineSizes)
  ; NewY is max(0, Y - 1),
    iterate_to_previous_regex_occurrence(Regex, InitStr, cursor(X, NewY), NewCursor, LineSizes)
  ).

replace_char(editor_state(M, PT, C, V, FS, FN, SB, CB, Undo, Redo, VS, Copy, Search), Char, NewState) :-
    add_to_undo_stack(editor_state(M, PT, C, V, FS, FN, SB, CB, Undo, Redo, VS, Copy, Search), Undo, NewUndo),
    PT = piece_table(Pieces, Orig, Add, Insert, _, LineSizes),
    move_cursor("l", C, LineSizes, 0, Cursor),
    cursor_xy_to_string_index(Cursor, LineSizes, 0, 0, Index),
    delete_text(Index, 1, PT, TempPT),
    TempPT = piece_table(P2, O2, A2, Insert2, _, LS2),
    string_concat(Insert2, Char, NewInsert),
    RightIndex is max(0, Index - 1),
    insert_text(piece_table(P2, O2, A2, NewInsert, RightIndex, LS2), NewPT),
    NewState = editor_state(M, NewPT, Cursor, V, FS, FN, SB, CB, NewUndo, [], VS, Copy, Search).


