:- module(file_manager, [
    save_file/4,
    write_file/3,
    quit_editor/2,
    save_and_quit/4,
    set_error/3,
    clear_error/2
]).
:- use_module('extended_piece_table.pl').

save_file(State, Force, Args, NewState) :-
    State = editor_state(Mode, PT, Cursor, View, FS, Filename, SB, CB, U, R, VS, CopyText, Search),
    ( Args == "" -> FName = Filename ; FName = Args ),
    ( FName == "" -> set_error(State, "File without name. Run \"w <filename>\".", NewState)
    ; write_file(State, FName, NewState)
).

replace_string(Original, Sub, Replacement, Result) :-
    split_string(Original, Sub, "", Parts),
    atomic_list_concat(Parts, Replacement, Result).

write_file(State, Path, NewState) :-
    State = editor_state(Mode, PT, Cursor, View, FS, FN, SB, CB, U, R, VS, CopyText, Search),
    split_dir_filename(Path, DName, FName),
    atomic_list_concat(['.', DName, '.', FName, '.swp'], TempFile),
    extended_piece_table_to_string(PT, NotSetContent),
    replace_string(NotSetContent, "\r", "\n", Content),
    open(TempFile, write, Stream),
    write(Stream, Content),
    close(Stream),
    rename_file(TempFile, Path),
    clear_error(StatusBar, NewStatusBar),
    ( FN == "" -> FinalName = FName ; FinalName = FN ),
    NewState = editor_state(normal, PT, Cursor, View, saved, FinalName, NewStatusBar, "", U, R, VS, CopyText, Search).

split_dir_filename(Path, DName, FName) :-
    atomic_list_concat(Parts, '/', Path),
    append(DirParts, [FName], Parts),
    atomic_list_concat(DirParts, '/', DirWithSlash),
    ( DirWithSlash == "" -> DName = "./" 
    ; atom_concat(DirWithSlash, "/", DName) 
).

save_and_quit(State, Force, Args, FinalState) :-
    save_file(State, Force, Args, TempState),
    TempState = editor_state(Mode, PT, Cursor, View, FS, FN, SB, CB, U, R, VS, CopyText, Search),
    ( SB = status_bar(exception, _) -> FinalState = TempState
    ; quit_editor(TempState, FinalState)
).

quit_editor(State, NewState) :-
    State = editor_state(Mode, PT, Cursor, View, FS, FN, SB, CB, U, R, VS, CopyText, Search),
    ( FS == not_saved -> set_error(State, "No write since last change. Use \"w\" or \"q!\" to quit without saving.", NewState)
    ; NewState = editor_state(closed, PT, Cursor, View, FS, FN, SB, CB, U, R, VS, CopyText, Search)
).

set_error(editor_state(Mode, PT, Cursor, View, FS, FN, SB, CB, U, R, VS, CopyText, Search), Msg, editor_state(normal, PT, Cursor, View, FS, FN, status_bar(exception,Msg), "", U, R, VS, CopyText, Search)).

clear_error(status_bar(_,_), status_bar(no_exception,"")).
