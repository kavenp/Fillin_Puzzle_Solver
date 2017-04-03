:- ensure_loaded(library(clpfd)).

% You can use this code to get started with your fillin puzzle solver.

main(PuzzleFile, WordlistFile, SolutionFile) :-
	read_file(PuzzleFile, Puzzle),
	read_file(WordlistFile, Wordlist),
	%convert_puzzle(Puzzle, ConvertedPuzzle),
	valid_puzzle(Puzzle),
	solve_puzzle(Puzzle, Wordlist, Solved),
	print_puzzle(SolutionFile, Solved).

read_file(Filename, Content) :-
	open(Filename, read, Stream),
	read_lines(Stream, Content),
	close(Stream).

read_lines(Stream, Content) :-
	read_line(Stream, Line, Last),
	(   Last = true
	->  (   Line = []
	    ->  Content = []
	    ;   Content = [Line]
	    )
	;  Content = [Line|Content1],
	    read_lines(Stream, Content1)
	).

read_line(Stream, Line, Last) :-
	get_char(Stream, Char),
	(   Char = end_of_file
	->  Line = [],
	    Last = true
	; 	Char = '\n'
	->  Line = [],
	    Last = false
	;	Char = '#'
	->	Line = [solid|Line1],
		read_line(Stream, Line1, Last)
	;	Char = '_'
	->  Line = [space(_)|Line1],
		read_line(Stream, Line1, Last)
	;   Line = [Char|Line1],
	    read_line(Stream, Line1, Last)
	).

print_puzzle(SolutionFile, Puzzle) :-
	open(SolutionFile, write, Stream),
	maplist(print_row(Stream), Puzzle),
	close(Stream).

print_row(Stream, Row) :-
	maplist(put_puzzle_char(Stream), Row),
	nl(Stream).

put_puzzle_char(Stream, Char) :-
	(   var(Char)
	->  put_char(Stream, '_')
	;   put_char(Stream, Char)
	).

valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
	maplist(samelength(Row), Rows).


samelength([], []).
samelength([_|L1], [_|L2]) :-
	same_length(L1, L2).

getHorizontalSlots([[]],[[]]).
getHorizontalSlots([[]|Rows],[[]|Slots]) :-
	getHorizontalSlots(Rows,Slots).
getHorizontalSlots([[X|Xs]|Rows],[Y|Slots]) :-
	(	X = solid
	->	getHorizontalSlots([Xs|Rows],[Y|Slots])
	;	getHorizontalSlots([Xs|Rows],[Y1|Slots]),
		append([X],Y1,Y)
	).


getSlot([],[]).
getSlot([X|Xs],Slot) :-
	(	X = solid
	->	getSlot([],Slot)
	;	append([X],TempSlot,Slot),
		getSlot(Xs,TempSlot)
	)
.

getAllSlots([[]],[[]]).
getAllSlots(Puzzle,AllSlots) :-
	getHorizontalSlots(Puzzle,XSlots),
	transpose(Puzzle,TransposedPuzzle),
	getHorizontalSlots(TransposedPuzzle,YSlots),
	append(XSlots,YSlots,AllSlots).

convertSpace(space(A),B) :- B = A.

/*getMatches(Slot,Words,Matches) :-
	Slot
*/
% solve_puzzle(Puzzle0, WordList, Puzzle)
% should hold when Puzzle is a solved version of Puzzle0, with the
% empty slots filled in with words from WordList.  Puzzle0 and Puzzle
% should be lists of lists of characters (single-character atoms), one
% list per puzzle row.  WordList is also a list of lists of
% characters, one list per word.
%
% This code is obviously wrong: it just gives back the unfilled puzzle
% as result.  You'll need to replace this with a working
% implementation.

solve_puzzle(Puzzle, _, Puzzle).
