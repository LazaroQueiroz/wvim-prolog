% ======================
% PROLOG: Editor State + Main Event Loop
% ======================

:- module(editor_main, [start_editor/1]).

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
start_editor(DebugMode):-
    tty_clear,
    tty_size(Rows, Cols),
    default_editor_state(Rows, Cols, "", EditorState),
    event_loop([EditorState], 0, DebugMode).

% ----- Get Terminal Size -----
get_terminal_size(Rows, Cols) :-
    shell('stty size', SizeOut),
    read_line_to_codes(user_input, Codes),
    atom_codes(Atom, Codes),
    atomic_list_concat([R, C], ' ', Atom),
    atom_number(R, Rows),
    atom_number(C, Cols).

% ----- Event Loop -----
event_loop(States, Index, DebugMode) :-
    nth0(Index, States, CurrentState),
    render(CurrentState, DebugMode),
    read_key(Code),
    string_codes(Input, Code), 
    handle_mode(CurrentState, Input, UpdatedState),
    UpdatedState = editor_state(Mode, PT, Cursor, View, FS, FN, SB, CB, U, R, VS, CopyText, Search),
    update_viewport(View, Cursor, UpdatedViewport),
    UpdatedViewportState = editor_state(Mode, PT, Cursor, UpdatedViewport, FS, FN, SB, CB, U, R, VS, CopyText, Search),
    replace_at(Index, UpdatedViewportState, States, NewStates),
    !,
    event_loop(NewStates, 0, DebugMode).

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
handle_input("[", _, States, Index, States, NewIndex) :-
    NewIndex is max(0, Index - 1).

handle_input("]", _, States, Index, States, NewIndex) :-
    length(States, Len),
    NewIndex is min(Len - 1, Index + 1).

handle_input("{", CurrentState, States, Index, NewStates, NewIndex) :-
    CurrentState = editor_state(_, _, _, Viewport, _, _, _, _, _, _, _, _, _),
    Viewport = viewport(Rows, Cols),
    default_editor_state(Rows, Cols, "", NewState),
    append(States, [NewState], NewStates),
    length(NewStates, L),
    NewIndex is L - 1.

handle_input(Input, CurrentState, States, Index, NewStates, Index) :-
    handle_key_press(CurrentState, Input, NewState),
    % update_editor_viewport(NewState1, NewState),
    replace_at(Index, NewState, States, NewStates).

handle_key_press(State, Char, NewState) :-
    member(Char, [104, 106, 107, 108, 113]),
    update_editor_cursor(State, Char, NewState).


% ----- Replace At Index -----
replace_at(0, New, [_|Xs], [New|Xs]).
replace_at(I, New, [X|Xs], [X|Rest]) :-
    I > 0,
    I1 is I - 1,
    replace_at(I1, New, Xs, Rest).

% ----- Is Running? -----
is_running(editor_state(closed, _, _, _, _, _, _, _, _, _, _, _, _)) :- !, fail.
is_running(_) :- true.


