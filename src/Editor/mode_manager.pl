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
:- use_module(extended_piece_table).

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
handle_normal_mode(State, "i", NewState) :- switch_mode(State, insert, false, NewState).
handle_normal_mode(State, "a", NewState) :- switch_mode(State, insert, true, NewState).
handle_normal_mode(State, "v", NewState) :- switch_mode(State, visual, false, NewState).
handle_normal_mode(State, "R", NewState) :- switch_mode(State, replace, false, NewState).
handle_normal_mode(State, ":", NewState) :- switch_mode(State, command, false, NewState).
handle_normal_mode(State, "/", NewState) :- switch_mode(State, substitution, false, NewState).
handle_normal_mode(State, Input, NewState) :- update_editor_cursor(State, Input, NewState).

% Visual Mode Handler
handle_visual_mode(State, "\e", NewState) :- switch_mode(State, normal, false, NewState).
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
handle_insert_mode(State, "\e", NewState) :- switch_mode(State, normal, false, NewState).
handle_insert_mode(State, "\b", NewState) :- handle_delete(State, NewState).
handle_insert_mode(State, Input, NewState) :- handle_insert(State, Input, NewState).

% Replace Mode Handler
handle_replace_mode(State, "\e", NewState) :- switch_mode(State, normal, false, NewState).
handle_replace_mode(State, "\b", NewState) :- handle_delete(State, NewState).
handle_replace_mode(State, Input, NewState) :- handle_replace(State, Input, NewState).

% Substitution Mode Handler
handle_substitution_mode(State, "\e", NewState) :- switch_mode(State, normal, false, NewState).
handle_substitution_mode(State, "\b", NewState) :- State = editor_state(M, PT, C, V, FS, FN, SB, CB, U, R, VS, Copy, Search),
    string_length(Search, Len), Len > 0,
    sub_string(Search, 0, Len-1, _, NewSearch),
    NewState = editor_state(M, PT, C, V, FS, FN, SB, CB, U, R, VS, Copy, NewSearch).
handle_substitution_mode(State, Input, NewState) :-
    State = editor_state(M, PT, C, V, FS, FN, SB, CB, U, R, VS, Copy, Search),
    string_concat(Search, Input, NewSearch),
    NewState = editor_state(M, PT, C, V, FS, FN, SB, CB, U, R, VS, Copy, NewSearch).

% Command Mode Handler
handle_command_mode(State, "\e", NewState) :- switch_mode(State, normal, false, NewState).
handle_command_mode(State, "\b", NewState) :- State = editor_state(M, PT, C, V, FS, FN, SB, CB, U, R, VS, Copy, Search),
    string_length(CB, Len), Len > 0,
    sub_string(CB, 0, Len-1, _, NewCB),
    NewState = editor_state(M, PT, C, V, FS, FN, SB, NewCB, U, R, VS, Copy, Search).
handle_command_mode(State, Input, NewState) :-
    State = editor_state(M, PT, C, V, FS, FN, SB, CB, U, R, VS, Copy, Search),
    string_concat(CB, Input, NewCB),
    NewState = editor_state(M, PT, C, V, FS, FN, SB, NewCB, U, R, VS, Copy, Search).

% Handle insert
handle_insert(State, Input, NewState) :-
    State = editor_state(M, [Pieces, Orig, Add, InsertBuf, Index, LineSizes], Cursor, View, FS, FN, SB, CB, U, R, VS, Copy, Search),
    string_concat(InsertBuf, Input, NewInsert),
    update_lines_sizes(Input, Cursor, LineSizes, NewLines),
    NewPT = [Pieces, Orig, Add, NewInsert, Index, NewLines],
    AuxiliaryState = editor_state(M, NewPT, Cursor, View, not_saved, FN, SB, CB, U, R, VS, Copy, Search),
    update_editor_cursor(AuxiliaryState, "l", NewState).

% Handle delete
handle_delete(State, NewState) :- NewState = State.  % Placeholder

% Handle replace
handle_replace(State, Input, NewState) :- handle_insert(State, Input, NewState).

% Switch mode
switch_mode(State, NewMode, _, NewState) :-
    State = editor_state(_, PT, Cursor, View, FS, FN, SB, CB, U, R, VS, Copy, Search),
    NewState = editor_state(NewMode, PT, Cursor, View, FS, FN, SB, CB, U, R, VS, Copy, Search).

