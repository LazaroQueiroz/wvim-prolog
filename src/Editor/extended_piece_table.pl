:- module(piece_table, [
    create_extended_piece_table/2,
    insert_text/2,
    delete_text/4,
    split_piece_collection/4,
    split_piece/4,
    extended_piece_table_to_string/2,
    extended_piece_table_to_line_array/2,
    cursor_xy_to_string_index/5,
    split_lines/2,
    get_lines_sizes/4,
    update_lines_sizes/4,
    teste_print/1
]).

% ===============================
% PROLOG: Extended Piece Table
% ===============================

% Buffer types
buffer_type(original).
buffer_type(add).

% piece(BufferType, StartIndex, Length)

% create_extended_piece_table(+OriginalText, -PieceTable)
create_extended_piece_table(Text, piece_table([piece(original, 0, Len)], Text, "", "", 0, LineSizes)) :-
    string_length(Text, Len),
    get_lines_sizes(Text, 0, [], LineSizes).

% insert_text(+PieceTable, -NewPieceTable)
insert_text(piece_table(Pieces, Orig, Add, "", Index, Lines), piece_table(Pieces, Orig, Add, "", Index, Lines)).
insert_text(piece_table(Pieces, Orig, Add, Insert, Index, Lines), piece_table(NewPieces, Orig, NewAdd, "", Index, Lines)) :-
    string_length(Add, AddLen),
    string_length(Insert, InsertLen),
    string_concat(Add, Insert, NewAdd),
    NewPiece = piece(add, AddLen, InsertLen),
    split_piece_collection(Index, Pieces, Before, After),
    append(Before, [NewPiece|After], NewPieces). 

% delete_text(+StartIndex, +Length, +PieceTable, -NewPieceTable)
delete_text(Start, Len, piece_table(Pieces, Orig, Add, Insert, Index, Lines), piece_table(NewPieces, Orig, Add, Insert, NewIndex, Lines)) :-
    S1 is Start - 1,
    split_piece_collection(S1, Pieces, Before, Rest),
    split_piece_collection(Len, Rest, _, After),
    append(Before, After, Combined),
    (Combined = [] -> NewPieces = [piece(original, 0, 0)] ; NewPieces = Combined),
    NewIndex is Index - Len.

% split_piece_collection(+Index, +Pieces, -Before, -After)
split_piece_collection(0, Pieces, Pieces, []).
split_piece_collection(_, [], [], []).
split_piece_collection(N, [P|Ps], [P|Before], After) :-
    P = piece(_, _, Len),
    N >= Len,
    N1 is N - Len,
    split_piece_collection(N1, Ps, Before, After).
split_piece_collection(N, [P|Ps], Split1, [Split2|Ps]) :-
    split_piece(N, P, Split1, [Split2]).

% split_piece(+Index, +Piece, -BeforeList, -AfterList)
split_piece(N, piece(Type, Start, Len), [piece(Type, Start, N)], [piece(Type, NewStart, AfterLen)]) :-
    N > 0,
    N < Len,
    NewStart is Start + N,
    AfterLen is Len - N.
split_piece(N, piece(Type, Start, Len), [], [piece(Type, Start, Len)]) :- N =< 0.
split_piece(N, piece(Type, Start, Len), [piece(Type, Start, Len)], []) :- N >= Len.

% extended_piece_table_to_string(+PieceTable, -String)
extended_piece_table_to_string(piece_table(Pieces, Orig, Add, Insert, Index, _), Result) :-
    extended_table_fold(Pieces, Orig, Add, Insert, Index, "", Result).

teste_print(String) :- writeln(String).

extended_table_fold([], _, _, _, _, Acc, Acc).
extended_table_fold([piece(Type, Start, Len)|Rest], Orig, Add, Insert, Index, Acc, Result) :-
    ( Type = original -> sub_string(Orig, Start, Len, _, S)
    ; Type = add      -> sub_string(Add, Start, Len, _, S)),
    string_length(Acc, Pos),
    Upper is Pos + Len,
    Lower is Pos,
    ( Index >= Lower, Index < Upper ->
        Mid is Index - Lower,
        sub_string(S, 0, Mid, _, Pre),
        sub_string(S, Mid, _, 0, Post),
        string_concat(Pre, Insert, Temp),
        string_concat(Temp, Post, Full)
    ; (Upper =:= Index) -> string_concat(S, Insert, Full) ; Full = S),
    string_concat(Acc, Full, Next),
    extended_table_fold(Rest, Orig, Add, Insert, Index, Next, Result).

% extended_piece_table_to_line_array(+PieceTable, -Lines)
extended_piece_table_to_line_array(PT, Lines) :-
    extended_piece_table_to_string(PT, String),
    split_lines(String, Lines).

% cursor_xy_to_string_index(+Cursor, +LineSizes, +Acc, +LineIndex, -Index)
% cursor_xy_to_string_index(cursor(X, Y), [], Acc, _, Acc) :- writeln("\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\tFUDEU, CHEGOU AQUI.").
cursor_xy_to_string_index(cursor(X, Y), [H|_], Acc, LineIdx, Index) :- 
  LineIdx =:= X,
  Index is Acc + Y + X, 
  writeln("this is Index:"), writeln(Index).
cursor_xy_to_string_index(cursor(X, Y), [H|T], Acc, LineIdx, Index) :-
  writeln("\r\r\r\r\r\r\r\r\r\r\r\rthis is LineIdx:"), writeln(LineIdx),
  Acc1 is Acc + H,
  LineIdx1 is LineIdx + 1,
  cursor_xy_to_string_index(cursor(X, Y), T, Acc1, LineIdx1, Index)).

% split_lines(+String, -Lines)
split_lines(Text, Lines) :-
    split_string(Text, "\n\r", "\n\r", Lines).

% get_lines_sizes(+String, +CurrentLen, +Acc, -Sizes)
get_lines_sizes([], Len, Acc, Sizes) :- reverse([Len|Acc], Sizes).
get_lines_sizes(['\n'|T], Len, Acc, Sizes) :- get_lines_sizes(T, 0, [Len|Acc], Sizes).
get_lines_sizes([_|T], Len, Acc, Sizes) :- Len1 is Len + 1, get_lines_sizes(T, Len1, Acc, Sizes).

% update_lines_sizes(+Input, +Cursor, +LineSizes, -NewSizes)
update_lines_sizes("\r", cursor(X, Y), LineSizes, New) :-
    split_at(X, LineSizes, Before, [Cur|After]),
    Y1 is Y,
    Y2 is Cur - Y,
    append(Before, [Y1, Y2|After], New).
update_lines_sizes("\b", cursor(X, 0), LineSizes, New) :-
    split_at(X, LineSizes, Before0, [Cur|After]),
    (Before0 = [] -> Before = [], Last = 0 ; append(Before, [Last], Before0)),
    NewSize is Cur + Last,
    append(Before, [NewSize|After], New).
update_lines_sizes("\b", cursor(X, Y), LineSizes, New) :-
    split_at(X, LineSizes, Before, [Cur|After]),
    NewSize is Cur - 1,
    append(Before, [NewSize|After], New).
update_lines_sizes(_, cursor(X, _), LineSizes, New) :-
    split_at(X, LineSizes, Before, [Cur|After]),
    NewSize is Cur + 1,
    append(Before, [NewSize|After], New).

% Helper: split_at(+Index, +List, -Before, -After)
split_at(0, L, Before, L) :- Before = [].
split_at(N, [H|T], [H|B], A) :- 
  N > 0, N1 is N - 1, split_at(N1, T, B, A).

