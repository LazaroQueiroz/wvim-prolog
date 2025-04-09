:- module(editor_state, [
    mode/1,
    file_status/1,
    default_editor_state/4,
    editor_state_from_file/5,
    update_editor_cursor/3,
    update_editor_viewport/2,
    add_to_undo_stack/3,
    add_to_redo_stack/3
]).

:- use_module('extended_piece_table.pl').
:- use_module('cursor.pl').
:- use_module('viewport.pl').

% ======================
% PROLOG: Editor State
% ======================

% ----- Modes and Status -----
mode(normal).
mode(insert).
mode(command).
mode(replace).
mode(visual).
mode(substitution).
mode(closed).

file_status(saved).
file_status(not_saved).

% ----- Data Structures -----
% cursor(Line, Column).
% viewport(RowOffset, ColOffset).
% status_bar(StatusType, Message).
% piece_table(Pieces, OriginalBuffer, AddBuffer, InsertBuffer, InsertIndex, LineSizes).

% editor_state(
%     Mode, PieceTable, Cursor, Viewport, FileStatus,
%     Filename, StatusBar, CommandBuffer,
%     UndoStack, RedoStack, VisualStartIndex, CopyBuffer, SearchBuffer
% ).

% ----- Default Editor State -----
default_editor_state(Rows, Cols, Filename, EditorState) :-
    % create_extended_piece_table("", PieceTable),
    PieceTable = piece_table([piece(original, 0, 0)], "", "", "", 0, [0]),
    Cursor = cursor(0, 0),
    default_viewport(Rows, Cols, Viewport),
    FileStatus = saved,
    StatusBar = status_bar(no_exception, ""),
    EditorStateToUndoStack = editor_state(
        normal, PieceTable, Cursor, Viewport, FileStatus,
        Filename, StatusBar, "", [], [], 0, "", ""
    ), 
    EditorState = editor_state(
        normal, PieceTable, Cursor, Viewport, FileStatus,
        Filename, StatusBar, "", [EditorStateToUndoStack], [], 0, "", ""
    ).

% ----- Editor State from File -----
editor_state_from_file(Content, Rows, Cols, Filename, EditorState) :-
    create_extended_piece_table(Content, PieceTable),
    Cursor = cursor(0, 0),
    default_viewport(Rows, Cols, Viewport),
    FileStatus = saved,
    StatusBar = status_bar(no_exception, ""),
    EditorState = editor_state(
        normal, PieceTable, Cursor, Viewport, FileStatus,
        Filename, StatusBar, "", [], [], 0, "", ""
    ).

% ----- Update Cursor in Editor State -----
update_editor_cursor(editor_state(M, PT, C, V, FS, FN, SB, CB, U, R, VS, Copy, Search), Direction, UpdatedState) :-
    PT = piece_table(_, _, _, _, _, LineSizes),
    ( M = insert -> Cond = 0 ; Cond = 1 ),
    move_cursor(Direction, C, LineSizes, Cond, NewCursor),
    UpdatedState = editor_state(M, PT, NewCursor, V, FS, FN, SB, CB, U, R, VS, Copy, Search).

% ----- Update Viewport in Editor State -----
update_editor_viewport(editor_state(M, PT, C, V, FS, FN, SB, CB, U, R, VS, Copy, Search), UpdatedState) :-
    C = cursor(X, Y),
    update_viewport(V, (X, Y), TempView),
    update_viewport(TempView, (X, Y), NewViewport),
    UpdatedState = editor_state(M, PT, C, NewViewport, FS, FN, SB, CB, U, R, VS, Copy, Search).

% ----- Add to Undo Stack -----
add_to_undo_stack(CurrentState, UndoStack, NewStack) :-
    % clear undo/redo from snapshot
    CurrentState = editor_state(M, PT, C, V, FS, FN, SB, CB, _, _, VS, Copy, Search),
    Snapshot = editor_state(M, PT, C, V, FS, FN, SB, CB, [], [], VS, Copy, Search),
    append(UndoStack, [Snapshot], NewStack).

% ----- Add to Redo Stack -----
add_to_redo_stack(CurrentState, RedoStack, NewStack) :-
    CurrentState = editor_state(M, PT, C, V, FS, FN, SB, CB, _, _, VS, Copy, Search),
    Snapshot = editor_state(M, PT, C, V, FS, FN, SB, CB, [], [], VS, Copy, Search),
    NewStack = [Snapshot|RedoStack].


