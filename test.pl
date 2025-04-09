replace_string(Original, Sub, Replacement, Result) :-
    split_string(Original, Sub, "", Parts),
    atomic_list_concat(Parts, Replacement, Result).

