% This buffer is for notes you don't want to save.
% If you want to create a file, visit that file with C-x C-f,
% then enter the text in that file's own buffer.


white(a).
black(b).

% if <cond> then
%    <action_1> else
%    <action_2>

color(X) :- white(X) ->
            write('white'); 
	    write('black').
