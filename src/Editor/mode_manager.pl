:- module(mode_manager, [
    handle_mode/3,
    handle_command_mode/3,
    handle_insert_mode/3,
    handle_replace_mode/3,
    handle_normal_mode/3,
    handle_visual_mode/3,
    handle_substitution_mode/3,
    handle_insert/3,
    handle_delete/2,
    handle_replace/3,
    switch_mode/4
]).

:- use_module('editorState.pl').
:- use_module('extended_piece_table.pl').

% ----- Entry Point -----
handle_mode(State, Input, NewState) :-
    State = editor_state(Mode, _, _, _, _, _, _, _, _, _, _, _, _),
    handle_mode_dispatch(Mode, State, Input, NewState).

handle_mode_dispatch(normal, State, Input, NewState) :- handle_normal_mode(State, Input, NewState).
handle_mode_dispatch(visual, State, Input, NewState) :- handle_visual_mode(State, Input, NewState).
handle_mode_dispatch(insert, State, Input, NewState) :- handle_insert_mode(State, Input, NewState).
handle_mode_dispatch(replace, State, Input, NewState) :- handle_replace_mode(State, Input, NewState).
handle_mode_dispatch(substitution, State, Input, NewState) :- handle_substitution_mode(State, Input, NewState).
handle_mode_dispatch(command, State, Input, NewState) :- handle_command_mode(State, Input, NewState).
handle_mode_dispatch(_, State, _, State).

% Normal Mode Handler
% cursor_xy_to_string_index(+Cursor, +LineSizes, +Acc, +LineIndex, -Index)
handle_normal_mode(State, "i", NewState) :- 
    State = editor_state(Mode, PT, Cursor, View, FS, FN, SB, CB, U, R, VS, CopyText, Search),
    PT = piece_table(Pieces, OriginalBuffer, AddBuffer, InsertBuffer, InsertStartIndex, LineSizes),
    cursor(X, Y) = Cursor,
    cursor_xy_to_string_index(Cursor, LineSizes, 0, 0, NewInsertStartIndex), 
    NewPT = piece_table(Pieces, OriginalBuffer, AddBuffer, InsertBuffer, NewInsertStartIndex, LineSizes),
    AuxiliaryState = editor_state(Mode, NewPT, Cursor, View, FS, FN, SB, CB, U, R, VS, CopyText, Search),
    switch_mode(AuxiliaryState, insert, false, NewState).
handle_normal_mode(State, "a", NewState) :- 
  State = editor_state(Mode, PT, Cursor, View, FS, FN, SB, CB, U, R, VS, CopyText, Search),
  InsertState = editor_state(insert, PT, Cursor, View, FS, FN, SB, CB, U, R, VS, CopyText, Search),
  update_editor_cursor(InsertState, "l", UpdatedCursorState),
  UpdatedCursorState = editor_state(insert, PT, NewCursor, View, FS, FN, SB, CB, U, R, VS, CopyText, Search),
  PT = piece_table(Pieces, OriginalBuffer, AddBuffer, InsertBuffer, _, LineSizes),
  cursor_xy_to_string_index(NewCursor, LineSizes, 0, 0, NewInsertStartIndex),
  NewPT = piece_table(Pieces, OriginalBuffer, AddBuffer, InsertBuffer, NewInsertStartIndex, LineSizes),
  AuxiliaryState = editor_state(normal, NewPT, NewCursor, View, FS, FN, SB, CB, U, R, VS, CopyText, Search),
  switch_mode(AuxiliaryState, insert, false, NewState).
    % State = editor_state(Mode, PT, Cursor, View, FS, FN, SB, CB, U, R, VS, CopyText, Search),
    % MidState = editor_state(insert, PT, Cursor, View, FS, FN, SB, CB, U, R, VS, CopyText, Search),
    % update_editor_cursor(MidState, "l", AlmostFinalState),
    % AlmostFinalState = editor_state(Mode, PT, Cursor, View, FS, FN, SB, CB, U, R, VS, CopyText, Search),
    % PT = piece_table(Pieces, OriginalBuffer, AddBuffer, InsertBuffer, InsertStartIndex, LineSizes),
    % cursor(X, Y) = Cursor,
    % cursor_xy_to_string_index(Cursor, LineSizes, 0, 0, NewInsertStartIndex), 
    % NewPT = piece_table(Pieces, OriginalBuffer, AddBuffer, InsertBuffer, NewInsertStartIndex, LineSizes),
    % AuxiliaryState = editor_state(Mode, NewPT, Cursor, View, FS, FN, SB, CB, U, R, VS, CopyText, Search),
    % switch_mode(AuxiliaryState, insert, false, NewState).
handle_normal_mode(State, "v", NewState) :- switch_mode(State, visual, false, NewState).
handle_normal_mode(State, "R", NewState) :- switch_mode(State, replace, false, NewState).
handle_normal_mode(State, ":", NewState) :- switch_mode(State, command, false, NewState).
handle_normal_mode(State, "/", NewState) :- switch_mode(State, substitution, false, NewState).
handle_normal_mode(State, Input, NewState) :- update_editor_cursor(State, Input, NewState).

% Visual Mode Handler
handle_visual_mode(State, "\u001B", NewState) :- switch_mode(State, normal, false, NewState).
handle_visual_mode(State, "v", State) :- State = editor_state(_, PT, Cursor, View, FS, FN, SB, CB, U, R, VS, Copy, Search),
    PT = piece_table(_, _, _, _, _, Lines),
    cursor_xy_to_string_index(Cursor, Lines, 0, 0, Index),
    extended_piece_table_to_string(PT, Str),
    Start is min(VS, Index),
    End is max(VS, Index),
    sub_string(Str, Start, Len, _, CopyText), Len is End - Start + 1,
    NewState = editor_state(normal, PT, Cursor, View, FS, FN, SB, CB, U, R, VS, CopyText, Search).
handle_visual_mode(State, Input, NewState) :- update_editor_cursor(State, Input, NewState).

% Insert Mode Handler
handle_insert_mode(State, "\e", NewState) :- 
  State = editor_state(M, PT, C, V, FS, FN, SB, CB, U, R, VS, Copy, NewSearch),
  insert_text(PT, NewPT),
  AuxiliaryState = editor_state(M, NewPT, C, V, FS, FN, SB, CB, U, R, VS, Copy, NewSearch),
  switch_mode(AuxiliaryState, normal, false, TempState),
  update_editor_cursor(TempState, "h", NewState).
handle_insert_mode(State, "\u007F", NewState) :- handle_delete(State, NewState), !.
handle_insert_mode(State, Input, NewState) :- 
  member(Input, ["\e[A", "\e[B", "\e[C", "\e[D"]),
  State = editor_state(M, PT, C, V, FS, FN, SB, CB, U, R, VS, Copy, NewSearch),
  insert_text(PT, NewPT),
  NewPT = piece_table(Pieces, OriginalBuffer, AddBuffer, InsertBuffer, InsertStartIndex, LineSizes),
  update_editor_cursor(State, Input, AuxiliaryState),
  AuxiliaryState = editor_state(M, NPT, NewC, V, FS, FN, SB, CB, U, R, VS, Copy, NewSearch),
  cursor_xy_to_string_index(NewC, LineSizes, 0, 0, NewInsertStartIndex),
  FinalPT = piece_table(Pieces, OriginalBuffer, AddBuffer, InsertBuffer, NewInsertStartIndex, LineSizes),
  NewState = editor_state(M, FinalPT, NewC, V, FS, FN, SB, CB, U, R, VS, Copy, NewSearch), !.
handle_insert_mode(State, Input, NewState) :- 
  handle_insert(State, Input, NewState).

% Replace Mode Handler
handle_replace_mode(State, "\e", NewState) :- switch_mode(State, normal, false, NewState).
handle_replace_mode(State, "\u007F", NewState) :- handle_delete(State, NewState).
handle_replace_mode(State, Input, NewState) :- handle_replace(State, Input, NewState).

% Substitution Mode Handler
handle_substitution_mode(State, "\e", NewState) :- switch_mode(State, normal, false, NewState).
handle_substitution_mode(State, "\u007F", NewState) :- State = editor_state(M, PT, C, V, FS, FN, SB, CB, U, R, VS, Copy, Search),
    string_length(Search, Len), Len > 0,
    sub_string(Search, 0, Len-1, _, NewSearch),
    NewState = editor_state(M, PT, C, V, FS, FN, SB, CB, U, R, VS, Copy, NewSearch).
handle_substitution_mode(State, Input, NewState) :-
    State = editor_state(M, PT, C, V, FS, FN, SB, CB, U, R, VS, Copy, Search),
    string_concat(Search, Input, NewSearch),
    NewState = editor_state(M, PT, C, V, FS, FN, SB, CB, U, R, VS, Copy, NewSearch).

% Command Mode Handler
handle_command_mode(State, "\e", NewState) :- switch_mode(State, normal, false, NewState).
handle_command_mode(State, "\u007F", NewState) :- State = editor_state(M, PT, C, V, FS, FN, SB, CB, U, R, VS, Copy, Search),
    string_length(CB, Len), Len > 0,
    sub_string(CB, 0, Len-1, _, NewCB),
    NewState = editor_state(M, PT, C, V, FS, FN, SB, NewCB, U, R, VS, Copy, Search).
handle_command_mode(State, Input, NewState) :-
    State = editor_state(M, PT, C, V, FS, FN, SB, CB, U, R, VS, Copy, Search),
    string_concat(CB, Input, NewCB),
    NewState = editor_state(M, PT, C, V, FS, FN, SB, NewCB, U, R, VS, Copy, Search).

% Handle insert
handle_insert(State, Input, NewState) :-
    State = editor_state(M, piece_table(Pieces, Orig, Add, InsertBuf, Index, LineSizes), Cursor, View, FS, FN, SB, CB, U, R, VS, Copy, Search),
    string_concat(InsertBuf, Input, NewInsert),
    update_lines_sizes(Input, Cursor, LineSizes, NewLines),
    NewPT = piece_table(Pieces, Orig, Add, NewInsert, Index, NewLines),
    AuxiliaryState = editor_state(M, NewPT, Cursor, View, not_saved, FN, SB, CB, U, R, VS, Copy, Search),
    get_direction(Input, Direction),
    update_editor_cursor(AuxiliaryState, Direction, NewState).

get_direction("\r", "\r").
get_direction("\u007F", "\u007F").
get_direction(Input, "l").

% Handle delete
handle_delete(State, NewState) :-
    State = editor_state(M, piece_table(Pieces, Orig, Add, InsertBuf, Index, LineSizes), Cursor, View, _, FN, SB, CB, U, R, VS, Copy, Search),
    string_length(InsertBuf, Len),
    Len > 0,
    NewLen is Len - 1,
    sub_string(InsertBuf, 0, NewLen, 1, NewInsertBuf),
    update_lines_sizes("\u007F", Cursor, LineSizes, NewLineSizes),
    Cursor = cursor(X, _),
    NewPT = piece_table(Pieces, Orig, Add, NewInsertBuf, Index, NewLineSizes),
    AuxiliaryState = editor_state(M, NewPT, Cursor, View, not_saved, FN, SB, CB, U, R, VS, Copy, Search),
    update_editor_cursor(AuxiliaryState, "\u007F", NewState). 

handle_delete(State, NewState) :-
  State = editor_state(_, _, cursor(0, 0), _, _, _, _, _, _, _, _, _, _),
  NewState = State.

handle_delete(State, NewState) :-
    State = editor_state(M, piece_table(Pieces, Orig, Add, InsertBuf, Index, LineSizes), Cursor, View, _, FN, SB, CB, U, R, VS, Copy, Search),
    delete_text(Index, 1, piece_table(Pieces, Orig, Add, "", Index, LineSizes), TempState),
    TempState = piece_table(NewPieces, NewOrig, NewAdd, NewInsert, NewIndex, _),
    update_editor_cursor(State, "\u007F", AuxiliaryState), 
    AuxiliaryState = editor_state(_, _, NewCursor, _, _, _, _, _, _, _, _, _, _),
    update_lines_sizes("\u007F", Cursor, LineSizes, NewLineSizes),
    NewPT = piece_table(NewPieces, NewOrig, NewAdd, NewInsert, NewIndex, NewLineSizes),
    NewState = editor_state(M, NewPT, NewCursor, View, not_saved, FN, SB, CB, U, R, VS, Copy, Search).


% Handle replace
handle_replace(State, Input, NewState) :- handle_insert(State, Input, NewState).

% Switch mode
switch_mode(State, NewMode, _, NewState) :-
    State = editor_state(_, PT, Cursor, View, FS, FN, SB, CB, U, R, VS, Copy, Search),
    NewState = editor_state(NewMode, PT, Cursor, View, FS, FN, SB, CB, U, R, VS, Copy, Search).

