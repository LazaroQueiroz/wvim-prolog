% ======================
% PROLOG: Editor State + Main Event Loop
% ======================

:- module(editor_main, [start_editor/0]).
:- use_module(library(readutil)).
:- use_module(library(tty)).
:- use_module(library(system)).
:- use_module(library(apply)).
:- use_module('../src/Editor/editorState.pl').
:- use_module('../src/Editor/mode_manager.pl').
:- use_module('../src/Editor/viewport.pl').
:- use_module('renderer.pl').

% (Previous editor state predicates go here...)
% Assuming editor_state/13 and helpers already defined above

% ----- Start Editor -----
start_editor :-
  %tty_clear,
  tty_size(Rows, Cols),
  current_prolog_flag(argv, Args),
  editor_state_initialization(Args, Rows, Cols, EditorState),
  event_loop([EditorState], 0, Args).

% ---- Initialization Editor State ----
editor_state_initialization([], Rows, Cols, EditorState) :-
  default_editor_state(Rows, Cols, "", EditorState).

editor_state_initialization([Filename | _], Rows, Cols, EditorState)  :-
  atom_string(Filename, FilenameString),
  ( exists_file(FilenameString) ->
    read_file_to_string(FilenameString, Content, []),
    editor_state_from_file(Content, Rows, Cols, FilenameString, EditorState)
  ;
    default_editor_state(Rows, Cols, FilenameString, EditorState)
  ).

% ----- Get Terminal Size -----
get_terminal_size(Rows, Cols) :-
    shell('stty size', SizeOut),
    read_line_to_codes(user_input, Codes),
    atom_codes(Atom, Codes),
    atomic_list_concat([R, C], ' ', Atom),
    atom_number(R, Rows),
    atom_number(C, Cols).

% ----- Event Loop -----
event_loop(States, Index, Args) :-
    nth0(Index, States, CurrentState),
    render(CurrentState),
    read_key(Code),
    string_codes(Input, Code),
    handle_other_inputs(Input, CurrentState, States, Index, NewStates, NewIndex), 
    handle_mode(CurrentState, Input, UpdatedState),
    UpdatedState = editor_state(Mode, PT, Cursor, View, FS, FN, SB, CB, U, R, VS, CopyText, Search),
    update_viewport(View, Cursor, UpdatedViewport),
    UpdatedViewportState = editor_state(Mode, PT, Cursor, UpdatedViewport, FS, FN, SB, CB, U, R, VS, CopyText, Search),
    replace_at(Index, UpdatedViewportState, NewStates, UpdatedStates),
    (Mode == closed -> FinalIndex = 0; FinalIndex = NewIndex),
    is_running(UpdatedStates),
    event_loop(UpdatedStates, FinalIndex, Args).

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

event_loop(_, Index) :-
    Index =:= -1, 
    format("~nExiting PrologVim~n", []).


% ----- Input Handler -----
handle_other_inputs("[", _, States, Index, States, NewIndex) :-
    NewIndex is max(0, Index - 1), !.
handle_other_inputs("]", _, States, Index, States, NewIndex) :-
    length(States, Len),
    NewIndex is min(Len - 1, Index + 1), !.
handle_other_inputs("{", CurrentState, States, Index, NewStates, NewIndex) :-
    CurrentState = editor_state(_, _, _, Viewport, _, _, _, _, _, _, _, _, _),
    Viewport = viewport(Rows, Cols, _, _),
    default_editor_state(Rows, Cols, "", NewState),
    append(States, [NewState], NewStates),
    length(NewStates, L),
    NewIndex is L - 1, !.
handle_other_inputs(Input, CurrentState, States, Index, States, Index).


handle_key_press(State, Char, NewState) :-
    member(Char, [104, 106, 107, 108, 113]),
    update_editor_cursor(State, Char, NewState).


% % ----- Replace At Index -----
replace_at(0, New, [_|Xs], Xs) :-
  New = editor_state(closed, _, _, _, _, _, _, _, _, _, _, _, _).
replace_at(0, New, [_|Xs], [New|Xs]) :-
  New = editor_state(Mode, _, _, _, _, _, _, _, _, _, _, _, _), 
  not(Mode == closed).
replace_at(I, New, [X|Xs], [X|Rest]) :-
    I > 0,
    I1 is I - 1,
    replace_at(I1, New, Xs, Rest).

% ----- Is Running? -----
is_running([]) :- halt.
is_running(_) :- true.

:- initialization(start_editor, main).
