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
    switch_mode/4,
    add_current_state_to_undo_stack/2,
    undo_editor_state/2,
    redo_editor_state/2
]).

:- use_module('editorState.pl').
:- use_module('extended_piece_table.pl').
:- use_module('file_manager.pl').
:- use_module('motion_handler.pl').

% ===============================
% PROLOG: Mode Manager
% ===============================

% ----- Entry Point -----
handle_mode(State, "[", State) :- !.
handle_mode(State, "]", State) :- !.
handle_mode(State, "}", State) :- !.
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


% add_current_state_to_undo_stack(
%   editor_state(Mode, PT, Cursor, View, FS, FN, SB, CB, [], RedoStack, VS, CopyText, Search),
%   NewState
% ) :-
%   CurrentState = editor_state(Mode, PT, Cursor, View, FS, FN, SB, CB, [], RedoStack, VS, CopyText, Search),
%   CurrentStateToUndoStack = editor_state(Mode, PT, Cursor, View, FS, FN, SB, CB, [], [], VS, CopyText, Search),
%   NewState = editor_state(Mode, PT, Cursor, View, FS, FN, SB, CB, [CurrentStateToUndoStack], RedoStack, VS, CopyText, Search).

add_current_state_to_undo_stack(
  editor_state(Mode, PT, Cursor, View, FS, FN, SB, CB, UndoStack, RedoStack, VS, CopyText, Search), 
  editor_state(Mode, PT, Cursor, View, FS, FN, SB, CB, NewUndoStack, NewRedoStack, VS, CopyText, Search)
) :-
  ( (UndoStack = [LastState | _],
    LastState = editor_state(_, LastPT, LastCursor, _, _, _, _, _, _, _, _, _, _),
    ( PT \= LastPT) ; UndoStack = [])
  ->
    append([editor_state(Mode, PT, Cursor, View, FS, FN, SB, CB, [], [], VS, CopyText, Search)], UndoStack, NewUndoStack),
    NewRedoStack = []
  ;
    NewUndoStack = UndoStack
  ).

% Normal Mode Handler
handle_normal_mode(OldState, "i", NewState) :- 
    add_current_state_to_undo_stack(OldState, State), 
    State = editor_state(Mode, PT, Cursor, View, FS, FN, SB, CB, U, R, VS, CopyText, Search),
    PT = piece_table(Pieces, OriginalBuffer, AddBuffer, InsertBuffer, InsertStartIndex, LineSizes),
    cursor(X, Y) = Cursor,
    cursor_xy_to_string_index(Cursor, LineSizes, 0, 0, NewInsertStartIndex), 
    NewPT = piece_table(Pieces, OriginalBuffer, AddBuffer, InsertBuffer, NewInsertStartIndex, LineSizes),
    AuxiliaryState = editor_state(Mode, NewPT, Cursor, View, FS, FN, SB, CB, U, R, VS, CopyText, Search),
    switch_mode(AuxiliaryState, insert, false, NewState).
handle_normal_mode(OldState, "a", NewState) :- 
  add_current_state_to_undo_stack(OldState, State), 
  State = editor_state(Mode, PT, Cursor, View, FS, FN, SB, CB, U, R, VS, CopyText, Search),
  InsertState = editor_state(insert, PT, Cursor, View, FS, FN, SB, CB, U, R, VS, CopyText, Search),
  update_editor_cursor(InsertState, "l", UpdatedCursorState),
  UpdatedCursorState = editor_state(insert, PT, NewCursor, View, FS, FN, SB, CB, U, R, VS, CopyText, Search),
  PT = piece_table(Pieces, OriginalBuffer, AddBuffer, InsertBuffer, _, LineSizes),
  cursor_xy_to_string_index(NewCursor, LineSizes, 0, 0, NewInsertStartIndex),
  NewPT = piece_table(Pieces, OriginalBuffer, AddBuffer, InsertBuffer, NewInsertStartIndex, LineSizes),
  AuxiliaryState = editor_state(normal, NewPT, NewCursor, View, FS, FN, SB, CB, U, R, VS, CopyText, Search),
  switch_mode(AuxiliaryState, insert, false, NewState).
handle_normal_mode(State, "v", NewState) :- 
  State = editor_state(Mode, PT, Cursor, View, FS, FN, SB, CB, U, R, VS, CopyText, Search),
  PT = piece_table(_, _, _, _, _, LineSizes),
  cursor_xy_to_string_index(Cursor, LineSizes, 0, 0, CopyStartIndex),
  AuxiliaryState = editor_state(Mode, PT, Cursor, View, FS, FN, SB, CB, U, R, CopyStartIndex, CopyText, Search),
  switch_mode(AuxiliaryState, visual, false, NewState).
handle_normal_mode(State, "R", NewState) :- switch_mode(State, replace, false, NewState).
handle_normal_mode(State, ":", NewState) :- switch_mode(State, command, false, NewState).
handle_normal_mode(State, "/", NewState) :- 
  State = editor_state(Mode, PT, Cursor, View, FS, FN, SB, CB, UndoStack, RedoStack, VS, CopyText, _),  % Nada para desfazer
  NoSearchBufferState = editor_state(Mode, PT, Cursor, View, FS, FN, SB, CB, UndoStack, RedoStack, VS, CopyText, ""),  % Nada para desfazer
  switch_mode(NoSearchBufferState, substitution, false, NewState).
handle_normal_mode(State, "u", NewState) :- undo_editor_state(State, NewState).
handle_normal_mode(State, "t", NewState) :- redo_editor_state(State, NewState).
handle_normal_mode(State, Input, NewState) :-
  (member(Input, ["h", "j", "k", "l"])
  ;
  atom_chars(Input, Chars),
  Chars = ['\u001B' | _]),
  update_editor_cursor(State, Input, NewState), !.
handle_normal_mode(State, Input, NewState) :- atom_chars(Input, Chars), handle_motion(State, Chars, NewState).

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


% Visual Mode Handler
handle_visual_mode(State, "\u001B", NewState) :- switch_mode(State, normal, false, NewState).
handle_visual_mode(State, "v", NewState) :- 
    State = editor_state(_, PT, Cursor, View, FS, FN, SB, CB, U, R, VS, Copy, Search),
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
handle_substitution_mode(State, "\r", NewState) :- 
  handle_substitution(State, NewState). 
handle_substitution_mode(State, "\u007F", NewState) :- State = editor_state(M, PT, C, V, FS, FN, SB, CB, U, R, VS, Copy, Search),
    string_length(Search, Len), Len > 0,
    LenMinusOne is Len - 1,
    sub_string(Search, 0, LenMinusOne, _, NewSearch),
    NewState = editor_state(M, PT, C, V, FS, FN, SB, CB, U, R, VS, Copy, NewSearch).
handle_substitution_mode(State, Input, NewState) :-
    State = editor_state(M, PT, C, V, FS, FN, SB, CB, U, R, VS, Copy, Search),
    string_concat(Search, Input, NewSearch),
    NewState = editor_state(M, PT, C, V, FS, FN, SB, CB, U, R, VS, Copy, NewSearch).

handle_substitution(OldState, SubstitutedState) :-
  add_current_state_to_undo_stack(OldState, State), 
  State = editor_state(M, PT, C, V, FS, FN, SB, CB, U, R, VS, Copy, SearchBuffer),
  split_string(SearchBuffer, "/", "/", Parts),
  length(Parts, L),
  L > 1,
  extended_piece_table_to_string(PT, Str),
  [ReplaceWord, WithWord] = Parts,
  re_replace(ReplaceWord/g, WithWord, Str, SubstitutedStr),
  create_extended_piece_table(SubstitutedStr, NewPT),
  SubstitutedState = editor_state(normal, NewPT, C, V, FS, FN, SB, CB, U, R, VS, Copy, SearchBuffer).

handle_substitution(OldState, OldState) :-
  State = editor_state(M, PT, C, V, FS, FN, SB, CB, U, R, VS, Copy, SearchBuffer),
  split_string(SearchBuffer, "/", "/", Parts),
  length(Parts, L),
  L =< 1.

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
    NewState = editor_state(NewMode, PT, Cursor, View, FS, FN, SB, "", U, R, VS, Copy, Search).

% Command Mode Handler
handle_command_mode(State, "\e", NewState) :- switch_mode(State, normal, false, NewState).
handle_command_mode(State, "\u007F", NewState) :- State = editor_state(M, PT, C, V, FS, FN, SB, CB, U, R, VS, Copy, Search),
    string_length(CB, Len), Len > 0,
    LenMinusOne is Len - 1,
    sub_string(CB, 0, LenMinusOne, _, NewCB),
    NewState = editor_state(M, PT, C, V, FS, FN, SB, NewCB, U, R, VS, Copy, Search).
handle_command_mode(State, "\r", NewState) :- 
    State = editor_state(M, PT, C, V, FS, FN, SB, CB, U, R, VS, Copy, Search),
    handle_command(State, CB, NewState).
handle_command_mode(State, Input, NewState) :-
    State = editor_state(M, PT, C, V, FS, FN, SB, CB, U, R, VS, Copy, Search),
    string_concat(CB, Input, NewCB),
    NewState = editor_state(M, PT, C, V, FS, FN, SB, NewCB, U, R, VS, Copy, Search).

trim_string(Str, Trimmed) :-
    string_chars(Str, Chars),
    trim_leading(Chars, NoLeading),
    reverse(NoLeading, Rev),
    trim_leading(Rev, NoTrailingRev),
    reverse(NoTrailingRev, TrimmedChars),
    string_chars(Trimmed, TrimmedChars).

trim_leading([' ' | T], R) :- trim_leading(T, R), !.
trim_leading(['\t' | T], R) :- trim_leading(T, R), !.
trim_leading(['\n' | T], R) :- trim_leading(T, R), !.
trim_leading(['\r' | T], R) :- trim_leading(T, R), !.
trim_leading(L, L).

% Handle Command
handle_command(State, Input, NewState) :-
    State = editor_state(M, PT, C, V, FS, FN, SB, CB, U, R, VS, Copy, Search),
    atomic_list_concat(Parts, ' ', Input),
    Parts = [Command | RawArgsList],
    atom_string(Command, StringCommand),
    atomic_list_concat(RawArgsList, ' ', RawArgs),
    atom_string(RawArgs, StringRawArgs),
    normalize_space(string(Args), StringRawArgs),
    ( StringCommand == "w"  -> save_file(State, false, StringRawArgs, NewState), !
    ; StringCommand == "w!" -> save_file(State, True, StringRawArgs, NewState)
    ; StringCommand == "q"  -> quit_editor(State, NewState)
    ; StringCommand == "q!" -> NewState = editor_state(closed, PT, C, V, FS, FN, SB, CB, U, R, VS, Copy, Search)
    ; StringCommand == "wq" -> save_and_quit(State, false, Args, NewState)
    ; Command == "wq!" -> save_and_quit(State, true, Args, NewState)
    ; set_error(State, "Command not found.", NewState)
).

