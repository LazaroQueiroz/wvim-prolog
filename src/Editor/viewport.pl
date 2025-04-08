:- module(editor_viewport, [
    default_viewport/3,
    update_viewport/3,
    resize_viewport/4,
    scroll_up/3,
    scroll_down/3,
    scroll_left/3,
    scroll_right/3
]).

% Default viewport
default_viewport(Rows, Columns, viewport(Rows, Columns, 0, 0)).


% Update viewport
update_viewport(viewport(Rows, Columns, InitRow, InitCol), cursor(X, Y), Updated) :- 
  X < InitRow, 
  Diff is InitRow - X, 
  scroll_up(viewport(Rows, Columns, InitRow, InitCol), Diff, Updated).
update_viewport(viewport(Rows, Columns, InitRow, InitCol), cursor(X, Y), Updated) :- 
  X - InitRow > Rows - 2, 
  Diff is X - InitRow - Rows + 2, 
  scroll_down(viewport(Rows, Columns, InitRow, InitCol), Diff, Updated).
update_viewport(viewport(Rows, Columns, InitRow, InitCol), cursor(X, Y), Updated) :- 
  Y < InitCol, Diff is InitCol - Y, 
  scroll_left(viewport(Rows, Columns, InitRow, InitCol), Diff, Updated).
update_viewport(viewport(Rows, Columns, InitRow, InitCol), cursor(X, Y), Updated) :- 
  Y - InitCol > Columns - 2, 
  Diff is Y - InitCol - Columns + 2, 
  scroll_right(viewport(Rows, Columns, InitRow, InitCol), Diff, Updated).
update_viewport(Viewport, _, Viewport).

% Resize viewport
resize_viewport(viewport(_, _, IR, IC), NewRows, NewCols, viewport(NewRows, NewCols, IR, IC)).

% Scroll
scroll_up(viewport(R, C, IR, IC), N, viewport(R, C, NewIR, IC)) :- NewIR is IR - N.
scroll_down(viewport(R, C, IR, IC), N, viewport(R, C, NewIR, IC)) :- NewIR is IR + N.
scroll_left(viewport(R, C, IR, IC), N, viewport(R, C, IR, NewIC)) :- NewIC is IC - N.
scroll_right(viewport(R, C, IR, IC), N, viewport(R, C, IR, NewIC)) :- NewIC is IC + N.
