:- ensure_loaded(library(clpfd)).

%COMP90048 Declarative Programming Project 2
%Author : Kaven Peng
%Student ID : 696573

%This program is used to solve a fillin puzzle
%Run main with the arguments puzzle filename,
%words filename and the intended solved puzzle
%output filename to solve a fillin puzzle.

%Main function that takes in a puzzle filename
%Word filename and a filename where the solution
%will be written to.
main(PuzzleFile, WordlistFile, SolutionFile) :-
	read_file(PuzzleFile, Puzzle),
	read_file(WordlistFile, Wordlist),
	valid_puzzle(Puzzle),
	solve_puzzle(Puzzle, Wordlist, Solved),
	print_puzzle(SolutionFile, Solved).

%A function that reads a file given a filename,
%and outputs it in the form of a 2D list where
%each row element list is a line in the file
%and each character in the line is an element of
%the row.
read_file(Filename, Content) :-
	open(Filename, read, Stream),
	read_lines(Stream, Content),
	close(Stream).

%A function that reads lines from a Stream
%and outputs the content in a list
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

%A function that reads a single line and
%outputs the content in a list,
%while also converting some characters into
%easier to manage variables i.e. '#' -> solid,
% '_' -> space(_).
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
	;   Line = [space(Char)|Line1],
	    read_line(Stream, Line1, Last)
	).

%A function that prints a puzzle 2D list
%and writes it to the given SolutionFile
print_puzzle(SolutionFile, Puzzle) :-
	open(SolutionFile, write, Stream),
	maplist(print_row(Stream), Puzzle),
	close(Stream).

%A function that prints a row list
%to the Stream
print_row(Stream, Row) :-
	maplist(put_puzzle_char(Stream), Row),
	nl(Stream).

%A function that puts a character into
%the output Stream
put_puzzle_char(Stream, Char) :-
	(   var(Char)
	->  put_char(Stream, '_')
	;   put_char(Stream, Char)
	).

%A function that checks if a puzzle is valid
valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
	maplist(samelength(Row), Rows).

%A function that checks if 2 lists are of the
%same length
samelength([], []).
samelength([_|L1], [_|L2]) :-
	same_length(L1, L2).

%A function that gets all the horizontal slots
%from a given puzzle, and outputs a list of
%the slots
getHorizontalSlots([],[]).
getHorizontalSlots([Row|Rows],Slots) :-
	getSlotsFromRow(Row,RowSlots,false),
	append(RowSlots,TempSlots,Slots),
	getHorizontalSlots(Rows,TempSlots).

%A function that gets all the slots in a puzzle row
getSlotsFromRow([],[[]],_).
getSlotsFromRow([X|Xs],[Y|Slots],Open) :-
	(	(X = solid, Open = true)
		%End of a previously open slot
		%Close the previous slot Y and let Open = false
		%to prepare for the next slot
	->	Open1 = false,
		getSlotsFromRow(Xs,Slots,Open1)
	;	(X = solid, Open = false)
		%solids in a row, no need to Open,
		%continue recursing through the row
	->	getSlotsFromRow(Xs,[Y|Slots],Open)
	;	(X \= solid, Open = true)
		%part of the current open slot
		%keep skipping until we find a solid
	->	getSlotsFromRow(Xs,[Y|Slots],Open)
	;	(X \= solid, Open = false)
		%new slot, open the slot and store
		%the new slot in Slots
	->	Open1 = true,
		getSlot([X|Xs],Slot),
		Y = Slot,
		getSlotsFromRow(Xs,Slots,Open1)
	).

%A function that gets a slot by
%storing everything in the given row in Slot
%only stopping once it finds a solid
%this works because getSlotsFromRow already detects the
%first solid of an open slot before calling this function
getSlot([],[]).
getSlot([X|Xs],Slot) :-
	(	X = solid
		%End of slot,use [] to get to base case
	->	getSlot([],Slot)
	;	append([X],TempSlot,Slot),
		getSlot(Xs,TempSlot)	
	)
.

%A general predicate used to filter out all
%undesirable elements from the derived slot lists
%such as unbounded variables, empty lists and slots
%of length = 1
slotFilter(E) :-
	E \= [],
	\+ var(E),
	length(E,L),
	L > 1.

%Gets all slots from the puzzle,
%horizontally and vertically by using transpose
%on the puzzle and getting all the horizontal slots
%of both and appending them together.
%Also filtering the end result with slotFilter
%to keep AllSlots clean.
getAllSlots([[]],[[]]).
getAllSlots(Puzzle,AllSlots) :-
	getHorizontalSlots(Puzzle,XSlots),
	transpose(Puzzle,TransposedPuzzle),
	getHorizontalSlots(TransposedPuzzle,YSlots),
	append(XSlots,YSlots,Slots),
	sublist(slotFilter,Slots,AllSlots).

%Pattern matching to convert spaces back to characters
convertSpace(space(A),B) :- B = A.
%Pattern matching to convert solids back to characters
convertSolid(solid,B) :- B = '#'.

%Converts a list into a printable list (Chars)
%which allows print_puzzle to be used on
%the solved puzzle.
convertPrintable([],[]).
convertPrintable([X|Xs],[Y|Ys]) :-
	(	X = solid
	->	convertSolid(X,Y),
		convertPrintable(Xs,Ys)
	;	convertSpace(X,Y),
		convertPrintable(Xs,Ys)
	).

%Unifies a list with another
%In this program a general case would be
%unifying [space(_)] with [space('G')] for example.
unify([],[]).
unify([X|Xs],[Y|Ys]) :-
	X = Y,
	unify(Xs,Ys).

%Gets all matching words for a slot.
getMatches(Slot,Words,Matches) :-
	setof(Slot,member(Slot,Words),Matches)
.

%Gets the Slot with the least amount of matching words and
%the matching words.
getLeastMatches([MinSlot],Words,MinMatches,MinSlot,1) :-
	%The base case is when the [MinSlot] is a singleton
	%Hence the last argument NumSlots here is equal to 1
	getMatches(MinSlot,Words,MinMatches)
	%Here we store the matching words for the minimum slot.
.
getLeastMatches([Slot1,Slot2|Slots],Words,MinMatches,MinSlot,NumSlots) :-
	getMatches(Slot1,Words,Matches1),
	getMatches(Slot2,Words,Matches2),
	%Fast fail if any one of the matching word lists are empty
	Matches1 \= [],
	Matches2 \= [],
	%Get length of matches for comparison
	length(Matches1,Count1),
	length(Matches2,Count2),
	%NumSlots only used once to get the original length of Slots
	length([Slot1,Slot2|Slots],NumSlots),
	(	Count1 =< Count2
		%In this case keep Slot1
	->	NSlots is NumSlots - 1,
		%This is to keep track for the base case
		getLeastMatches([Slot1|Slots],Words,MinMatches,MinSlot,NSlots)
	;	NSlots is NumSlots - 1,
		%In this case keep Slot2
		getLeastMatches([Slot2|Slots],Words,MinMatches,MinSlot,NSlots)
	).

%Unifies all the slots in the puzzle recursively.
unifyAll([],[]).
unifyAll(Slots,Words) :-
	%Find the slot we want to unify first
	getLeastMatches(Slots,Words,Matches,MinSlot,_),
	%Remove that slot from the Slots list since it's
	%going to be unified first
	select(MinSlot,Slots,NewSlots),
	%Now we unify the slot with the Selected word
	unifySlot(MinSlot,Matches,Selected),
	%After slot has been unified with Selected word
	%we now remove the Selected word from the Words list
	select(Selected,Words,NewWords),
	%Recurse over the new Slots and Words list now
	%until we have unified all slots
	unifyAll(NewSlots,NewWords)
.

%Unifies a slot given a list of matching words
%Selected is the word selected for unification
%so we can remove the selected word from the Words
%list in unifyAll
unifySlot(Slot,[M|Matches],Selected) :-
	%Unify first match
	unify(Slot,M),
	%Store selected word
	Selected = M;
	%Create choice point here so if
	%the previously chosen word were to
	%fail somewhere later we will choose the
	%next word to unify with this slot
	unifySlot(Slot,Matches,_)
.


%Solves the given Puzzle with given Words
%and outputs it as a SolvedPuzzle
%this SolvedPuzzle is also converted so
%that it is already in printable form (Chars)
solve_puzzle(Puzzle, Words, SolvedPuzzle) :-
	%Get all the slots from the puzzle
	getAllSlots(Puzzle,Slots),
	%Unify/Solve all the slots
	unifyAll(Slots,Words),
	%Convert the now solved puzzle into printable format
	maplist(convertPrintable,Puzzle,SolvedPuzzle)
.
