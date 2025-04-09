replace_string(Original, Sub, Replacement, Result) :-
    split_string(Original, Sub, "", Parts),
    atomic_list_concat(Parts, Replacement, Result).



  teste([1, 2, 3], State):- State = [1, 2, 3], !.

