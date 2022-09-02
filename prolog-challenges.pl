%RESHAPE

%trivial implemantation of reshape function
%recursively traversing all the sublists of the given list
%ith element of the checked sublists is stored in the ith sublist
%of the final list
list_of_empty_lists(0, []).

%producing the empty final list that will be filled
%though recursion
list_of_empty_lists(X, [[]|Rest_List]):-
  Y is X - 1,
  list_of_empty_lists(Y, Rest_List).

%traversed the whole list
%producing the basis for the final list
%list of empty lists
transfer([], X, Base_Output):-
  list_of_empty_lists(X, Base_Output).

transfer(Input, X, Output) :-
  append([First_list], Rest_Lists, Input),
  transfer(Rest_Lists, X, Lesser_Output),
  %transfering the elements of current sublist to the partial output
  transfer_list(First_list, Lesser_Output, Output), !.

transfer_list([], X, X).

%adding each element of current sublist to the sublists of the final list
transfer_list([First_Item|Rest_Items], [First_Lesser_Output_List|Rest_Lesser_Output_Lists], [First_Output_List|Rest_Output_Lists]):-
  append([First_Item], First_Lesser_Output_List, First_Output_List),
  transfer_list(Rest_Items, Rest_Lesser_Output_Lists, Rest_Output_Lists).

reshape(Input, Output) :-
  append([First_list], Rest_Lists, Input),
  length(First_list, X), %we need the lenght of sublists
  transfer(Input, X, Output).

%NEWNUM

%Traversing the number's list from right to left
%Searching for the first digit that has a bigger digit on the right
%We get the minimum right digit bigger than the current one
%and swap it with the current one
%This is not enough as we have to make sure that the right part of the digit
%is the smallest one possible in order to get the next bigger number
%We can do that by sorting the right sublist of the number

%single digit number
numToList(Number, [Number]):-
  Number < 10.

numToList(Number, ListNumber):-
  Number >= 10,
  NewNumber is Number // 10,
  NewDigit is Number mod 10,
  numToList(NewNumber, RestNumberList),
  append(RestNumberList, [NewDigit], ListNumber), !.

%case when exponent is equal to 10^0 = 1
%single digit list
listToNumber([Digit], Digit, 0).

listToNumber([FirstDigit|RestList], Number, Exponent):-
  listToNumber(RestList, Rest_Number, PreviousExponent),
  Exponent is PreviousExponent + 1,
  Number is Rest_Number + FirstDigit * (10 ^ Exponent).

%found the bigger digit that we want to swap with the current one
replaceDigit(NumberAsList, DigitToSwap, ReplacingNumber, ModifiedNumberAsList):-
  append([DigitToSwap], RestNumberAsList, NumberAsList),
  append([ReplacingNumber], RestNumberAsList, ModifiedNumberAsList), !.

replaceDigit(NumberAsList, DigitToSwap, ReplacingNumber, ModifiedNumberAsList):-
  append([FirstDigit], RestNumberAsList, NumberAsList),
  replaceDigit(RestNumberAsList, DigitToSwap, ReplacingNumber, RestModifiedNumberAsList),
  append([FirstDigit], RestModifiedNumberAsList, ModifiedNumberAsList).


minGreaterDigit(FirstDigitOfNumber, RestNumberAsList, GreaterDigitAsList, ModifiedRestNumberAsList):-
  member(Candidate, RestNumberAsList),
  %looking for a digit bigger than the current one on the right part of the number
  Candidate > FirstDigitOfNumber,
  %our digit must be smaller than any other right digit bigger than the checked digit
  not((member(OtherCandidate, RestNumberAsList),
       OtherCandidate < Candidate,
       OtherCandidate > FirstDigitOfNumber)),
  replaceDigit(RestNumberAsList, Candidate, FirstDigitOfNumber, ModifiedRestNumberAsList),
  %found the digit we want to swap with the current one
  GreaterDigitAsList = Candidate.

%found the index of the swapped digit
firstDifferenceIndex([X|_], [Y|_], 1):-
  not((X is Y)).

%looking for the index of the updated digit
%we need it in order to sort in increasing order the digits on the right
firstDifferenceIndex([X|NumberAsList], [X|SwappedNumberAsList], Index):-
  firstDifferenceIndex(NumberAsList, SwappedNumberAsList, LowerIndex),
  Index is LowerIndex + 1.

%general recursion method to get the solution of subswap
swap(NumberAsList, SwappedNumberAsList):-
  append([FirstDigitOfNumber], RestNumberAsList, NumberAsList),
  swap(RestNumberAsList, RestSwappedNumberAsList),
  append([FirstDigitOfNumber], RestSwappedNumberAsList, SwappedNumberAsList).

%got to the end of the number
%now traversing from end to head
%trying to find the minimum greated digit on the right of the current one
swap(NumberAsList, SwappedNumberAsList):-
  append([FirstDigitOfNumber], RestNumberAsList, NumberAsList),
  %searching for bigger digit on the right of the current one
  %swapping those two digits producing ModifiedRestNumberAsList
  minGreaterDigit(FirstDigitOfNumber, RestNumberAsList, GreaterDigit, ModifiedRestNumberAsList),
  append([GreaterDigit], ModifiedRestNumberAsList, SwappedNumberAsList).

%given number gives the same number if reversed
%cannot produce a bigger number with
newnum(Number, 0):-
  numToList(Number, NumberAsList),
  reverse(NumberAsList, NumberAsList), !.

newnum(Number, 0):-
  numToList(Number, NumberAsList),
  reverse(NumberAsList, ReversedNumberAsList),
  sort(ReversedNumberAsList, ReversedNumberAsList), !.

newnum(Number, NewNumber):-
  %producing a list of sublists containing number's digits
  numToList(Number, NumberAsList),
  swap(NumberAsList, SwappedNumberAsList),
  %checking the index of the digit that was updated
  firstDifferenceIndex(NumberAsList, SwappedNumberAsList, Index),
  %prefix of the number list that will remain intact
  length(PrefixList, Index),
  append(PrefixList, RestList, SwappedNumberAsList),
  %sorting digits on the right of the swapped one
  %in order to get the smallest bigger number possible
  sort(RestList, SortedRestList),
  append(PrefixList, SortedRestList, NewNumberAsList),
  %changing our number list to number equivalent
  listToNumber(NewNumberAsList, NewNumber, _), !. %cut needed in order to avoid extra equivalent checks of listToNumber

%LEXICOGRAPHIC

%The final letters' sequence should be the lexicographically smallest one.
%This leads us to the realization that the first subtext should be the
%lexicographically smallest prefix of the initial text.
%We can find it by getting the positions of the lexicographically smallest letter
%in the list and getting the lexicographically smallest prefix from these positions
%by simply traversing the respective sublists from right to left.
%In this way we have the first subtext of our final text.
%We repeat the step for the part of text that we are left with, getting the
%second subtext. Getting the last part is trivial, as it is equal to simply reversing
%the remaining text.

%prolog default nth function seems malfunctioning
%wrote this one instead
nth(1, [E|_], E).

%getting the index under which the E element is contained
nth(N, [X|List], E):-
  nth(NN, List, E),
  N is NN + 1.

phraseAsNumberList([],[]).

%changing our character list to the ASCII number equivalent List
%for easier comparisons of elements
phraseAsNumberList([FirstChar|RestInputPhrase], [FirstNumber|RestInputPhraseAsNumberList]):-
  char_code(FirstChar, FirstNumber),
  phraseAsNumberList(RestInputPhrase, RestInputPhraseAsNumberList).

%Is will contain the indices under which the smallest character
%of the string is contained
indices(List, E, Is) :-
    findall(N, nth(N, List, E), Is).

%getting the prefix of the phrase up to the index element
prefix(Index, Phrase, Prefix):-
  length(Prefix, Index),
  append(Prefix, _, Phrase).

%getting all the prefixes that up to the indexes contained within appearances list
getAllPrefixes(InputPhrase, Appearances, Prefixes):-
  findall(Prefix, (member(Index, Appearances), prefix(Index, InputPhrase, Prefix)), Prefixes).

%first string has smaller length, is smaller lexicographically
stringComparison([],[_]).

%first list contains a lexicographically smaller character at an index
stringComparison([FirstStringHead|_], [SecondStringHead|_]):-
  FirstStringHead < SecondStringHead.

%returns true if first element smaller lexicographic than second
stringComparison([FirstStringHead|FirstStringTail], [SecondStringHead|SecondStringTail]):-
  FirstStringHead is SecondStringHead,
  stringComparison(FirstStringTail, SecondStringTail).

%returns one of the three parts of the final solution
getPartialOutput(NumberList, PartialSolution):-
  %getting the smallest character (represented with number) in current list
  min_list(NumberList, MinCode),
  %getting all the indices in which it is contained
  indices(NumberList, MinCode, Appearances),
  %getting all the prefixes ending with indices that we found
  getAllPrefixes(NumberList, Appearances, Prefixes),
  %getting the lexicographically smallest prefix
  %this will be our partial solution
  member(Prefix, Prefixes),
  not((member(OtherPrefix, Prefixes), stringComparison(OtherPrefix, Prefix))),
  length(Prefix, PartialSolution).

lexicographic(InputPhrase, OutputPhrase):-
  %changing input character list into equivalent number List
  %in order to be able to compare characters
  phraseAsNumberList(InputPhrase, InputPhraseAsNumberList),
  %getting the first substring
  getPartialOutput(InputPhraseAsNumberList, LengthOfFirstPart),
  length(FirstPartSolutionUnreversed, LengthOfFirstPart),
  append(FirstPartSolutionUnreversed, LeftPhrasePart, InputPhraseAsNumberList),
  %reversing the first substring
  reverse(FirstPartSolutionUnreversed, FirstPartSolution),
  %same story for the second one
  getPartialOutput(LeftPhrasePart, LengthOfSecondPart),
  length(SecondPartSolutionUnreversed, LengthOfSecondPart),
  append(SecondPartSolutionUnreversed, ThirdPartUnreversed, LeftPhrasePart),
  reverse(SecondPartSolutionUnreversed, SecondPartSolution),
  %no need to call function for third partial solution
  %just reversing the part of the input string we are left with
  reverse(ThirdPartUnreversed, ThirdPartSolution),
  append(FirstPartSolution, SecondPartSolution, FirstAndSecondPartSolution),
  append(FirstAndSecondPartSolution, ThirdPartSolution, OutputPhraseAsNumberList),
  %changing number list back into characters list
  phraseAsNumberList(OutputPhrase, OutputPhraseAsNumberList), !.

%PHOTOTORTURE

%INITIAL THOUGHT EXPERIMENT
%A single child can change position by getting in front of the group
%(which can be done only one time) or by filling the gap of another child
%that missbehaved. This means it can change positions in every single photo,
%so the only valid information we can get are his left neighbours
%(we don't care about the riht ones, as they can be easily derived from the left ones).

%The topology of two children can be changed only by one of them missbehaving, which means
%that their relative positioning in the sequence remains the same in 3 out of the 5 photos.

%ALGORITHM
%In the spirit of the last sentence, we realise that we can derive the needed information, by constructing
%all the possible triplets (10 in total) and getting the instersection of the left neighbours for each child
%in each triplet of photos. This partial solution, that indicates the initial left neighbours that we
%can derive from one triplet, can be generalized into the initial sequence of neighbours by getting
%the union of partial solutions. The subsets for each child that we found, contain all their left neighbours,
%thus their lenght implies their index in the initial sequence. We sort the children, according to that lenght.

%empty list produces empty set
listToSet([],[]).
%sorting list and removing all duplicates
listToSet(MyList, MySet):-
  %sort removes duplicates
  sort(MyList, MySet).

%empty lists make an empty union
listUnion([],[],[]).
%producing a list of elements of both lists without duplicates
listUnion(FirstList, SecondList, UnionList):-
  append(FirstList, SecondList, UnionWithDuplicates),
  listToSet(UnionWithDuplicates, UnionList).

getCommonElements([],[]).
%last item has no common element in other list
getCommonElements([_],[]).

getCommonElements([CurrentElement|RestList], IntersectionList):-
  append([NextElement], _, RestList),
  not(CurrentElement is NextElement),
  getCommonElements(RestList, IntersectionList).

getCommonElements([CurrentElement|RestList], IntersectionList):-
  append([NextElement], SublistToCheck, RestList),
  CurrentElement is NextElement,
  getCommonElements(SublistToCheck, RestIntersectionList),
  append([CurrentElement], RestIntersectionList, IntersectionList).

%producing list containing common elements of both lists without duplicates
listIntersection(FirstList, SecondList, IntersectionList):-
  append(FirstList, SecondList, TempList),
  sort(0, @=<, TempList, SortedTempList),
  getCommonElements(SortedTempList, IntersectionList), !.

%getting all possible triplets out of 5 photos, 10 in total
%algorithm will terminate for any amount of children with 5 Photos
%in order to avoid complex iteration, we simply hardcode all
%possible triplets
getAllTriplets([Photo1, Photo2, Photo3, Photo4, Photo5], AllPossibleTriplets):-
  append([Photo1], [Photo2, Photo3], Part1),
  append([Part1], [[Photo1, Photo2, Photo4]], Part2),
  append(Part2, [[Photo1, Photo2, Photo5]], Part3),
  append(Part3, [[Photo1, Photo3, Photo4]], Part4),
  append(Part4, [[Photo1, Photo3, Photo5]], Part5),
  append(Part5, [[Photo1, Photo4, Photo5]], Part6),
  append(Part6, [[Photo2, Photo3, Photo4]], Part7),
  append(Part7, [[Photo2, Photo3, Photo5]], Part8),
  append(Part8, [[Photo2, Photo4, Photo5]], Part9),
  append(Part9, [[Photo3, Photo4, Photo5]], AllPossibleTriplets).

%traversed each of the three photos
buildIntersectSets([Photo1, _, _], [], Iteration):-
  length(Photo1, MaxIterations),
  Iteration is MaxIterations + 1.

%Building a list containing a list with tuples of ([KIDS ON THE LEFT], CHILD)
buildIntersectSets([Photo1, Photo2, Photo3], IntersectSetList, Iteration):-
  not(Iteration is 1),
  %getting all the kids up to the currently checked one
  length(CheckedKids1, Iteration),
  length(CheckedKids2, Iteration),
  length(CheckedKids3, Iteration),
  append(CheckedKids1, _, Photo1),
  append(CheckedKids2, _, Photo2),
  append(CheckedKids3, _, Photo3),
  %getting the left kids sets for each child
  append(KidsOnLeft1, [CurrentKid1], CheckedKids1),
  append(KidsOnLeft2, [CurrentKid2], CheckedKids2),
  append(KidsOnLeft3, [CurrentKid3], CheckedKids3),
  NewIteration is Iteration + 1,
  buildIntersectSets([Photo1, Photo2, Photo3], UpdatedIntersectList, NewIteration),
  append([(KidsOnLeft1, CurrentKid1),(KidsOnLeft2, CurrentKid2),(KidsOnLeft3, CurrentKid3)], UpdatedIntersectList, IntersectSetList), !.

%last kid is being checked for each photo, no kids on the left
buildIntersectSets([Photo1, Photo2, Photo3], IntersectSetList, Iteration):-
  Iteration is 1,
  append([FirstChildPhoto1], _, Photo1),
  append([FirstChildPhoto2], _, Photo2),
  append([FirstChildPhoto3], _, Photo3),
  NewIteration is Iteration + 1,
  buildIntersectSets([Photo1, Photo2, Photo3], UpdatedIntersectList, NewIteration),
  append([([], FirstChildPhoto1),([], FirstChildPhoto2),([], FirstChildPhoto3)], UpdatedIntersectList, IntersectSetList), !.


getLeftNeighboursList([], []).

getLeftNeighboursList([(KidsOnLeft1, X), (KidsOnLeft2, X), (KidsOnLeft3, X)|RestIntersectList], LeftNeighboursList):-
    listIntersection(KidsOnLeft1, KidsOnLeft2, TempLeftKidsIntersection),
    listIntersection(TempLeftKidsIntersection, KidsOnLeft3, KidsOnLeftIntersection),
    getLeftNeighboursList(RestIntersectList, UpdatedLeftNeighboursList),
    append([(KidsOnLeftIntersection, X)], UpdatedLeftNeighboursList, LeftNeighboursList).

%getting a list consisting of the intersection of kids left to each kid in the triplet of photos
getIntersectSets(PhotosTriplet, LeftNeighboursList):-
  buildIntersectSets(PhotosTriplet, IntersectSetList, 1),
  sort(2, @=<, IntersectSetList, SortedIntersectSetList),
  getLeftNeighboursList(SortedIntersectSetList, LeftNeighboursList).

getAllNeighbours([], []).

%adding current triplet's left neighbours intersection list into the union list
getAllNeighbours([[Photo1, Photo2, Photo3]|RestTriplets], ListOfNeighbours):-
  getIntersectSets([Photo1, Photo2, Photo3], TripletNeighboursList),
  getAllNeighbours(RestTriplets, UpdatedListOfNeighbours),
  append(TripletNeighboursList, UpdatedListOfNeighbours,  ListOfNeighbours).

getIntersectionsUnion([],[]).

getIntersectionsUnion([(CurrentLeftNeighbours, _)|OtherLeftNeighbours], IntersectionUnion):-
  getIntersectionsUnion(OtherLeftNeighbours, OtherIntersectionUnion),
  listUnion(CurrentLeftNeighbours, OtherIntersectionUnion, IntersectionUnion).


getListWithIndexes([], [], _).
%List form [(Index, Kid), ...]
getListWithIndexes(ListOfNeighbours, ListWithIndexes, Kid):-
  length(IntersectionsForCurrentKid, 10),
  append(IntersectionsForCurrentKid, RestListOfNeighbours, ListOfNeighbours),
  getIntersectionsUnion(IntersectionsForCurrentKid, IntersectionUnion),
  length(IntersectionUnion, FinalKidIndex),
  %getting the index of the next child
  NextKid is Kid + 1,
  getListWithIndexes(RestListOfNeighbours, UpdatedListWithIndexes, NextKid),
  %adding the index of the current child to the Index List
  append([(FinalKidIndex, Kid)], UpdatedListWithIndexes, ListWithIndexes).

getProperPositions([],[]).
%just traversing the index list and storing kids in new list InitialState
getProperPositions([(_, Kid)|RestIndexTuples], InitialState):-
  getProperPositions(RestIndexTuples, UpdatedInitialState),
  append([Kid], UpdatedInitialState, InitialState).

%no kids, no initial state
getInitialState([], []).

getInitialState(Triplets, InitialState):-
  getAllNeighbours(Triplets, ListOfNeighbours),
  sort(2, @=<, ListOfNeighbours, SortedIntersectSetList),
  getListWithIndexes(SortedIntersectSetList, ListWithIndexes, 1),
  %sorting kids in regard to their index
  sort(0, @=<, ListWithIndexes, SortedListWithIndexes),
  %simply traversing the ListWithIndexes and building the list with initial kids positions
  getProperPositions(SortedListWithIndexes, InitialState), !.

phototorture(Photos, InitialState):-
  getAllTriplets(Photos, Triplets),
  getInitialState(Triplets, InitialState).

%MINCOL

%The solution follows a greedy approach, that we come about, looking at the
%form of the final solution. In order for a column to be contained in the
%final matrix, it has to contain the same element. This implies, that each row has
%to contain this element in the first place. Solution can be found, by simply iterating
%the elements of the first row [1,N], checking if the current element appears in
%the other two rows. If not, we simply delete the Ith column with the number I
%in the first row. Repeating this greedy move, we get the solution we were
%looking for.

getIthElement([H|_],1,H) :-
    !.
getIthElement([_|T],N,H) :-
    N > 0, %add for loop prevention
    N1 is N-1,
    getIthElement(T,N1,H).

cutListColumn([], [], _).

cutListColumn(UncutList, CutList, Index):-
  length(ListWithElement, Index),
  append(ListWithElement, RestUncutList, UncutList),
  append(ListWithoutElement, [ElementToRemove], ListWithElement),
  append(ListWithoutElement, RestUncutList, CutList), !.

cutMatrixColumn([],[],_).

%delete the Index element of each row of matrix
cutMatrixColumn([FirstList|RestMatrix], [CutFirstList|RestCutList], Index):-
  cutListColumn(FirstList, CutFirstList, Index),
  cutMatrixColumn(RestMatrix, RestCutList, Index).

getMatrixRows([], [], [], []).

%getting the three matrix rows
getMatrixRows(InputMatrix, FirstRow, SecondRow, ThirdRow):-
  getIthElement(InputMatrix, 1, FirstRow),
  getIthElement(InputMatrix, 2, SecondRow),
  getIthElement(InputMatrix, 3, ThirdRow).

getCuts(InputMatrix, OutputMatrix, NumberOfCuts, Index):-
  getMatrixRows(InputMatrix, FirstRow, SecondRow, ThirdRow),
  getIthElement(FirstRow, Index, NumberToCheck),
  (member(NumberToCheck, SecondRow), member(NumberToCheck, ThirdRow)),
  NextIndex is Index + 1,
  getCuts(InputMatrix, OutputMatrix, NextNumberOfCuts, NextIndex),
  NumberOfCuts is NextNumberOfCuts, !.

getCuts(InputMatrix, OutputMatrix, NumberOfCuts, Index):-
  getMatrixRows(InputMatrix, FirstRow, SecondRow, ThirdRow),
  getIthElement(FirstRow, Index, NumberToCheck),
  not((member(NumberToCheck, SecondRow), member(NumberToCheck, ThirdRow))),
  cutMatrixColumn(InputMatrix, UpdatedMatrix, Index),
  getCuts(UpdatedMatrix, OutputMatrix, NextNumberOfCuts, Index),
  NumberOfCuts is NextNumberOfCuts + 1, !.

%second and third row contain the searched number of first row
%no cut is needed
getCuts(InputMatrix, InputMatrix, NumberOfCuts, Index):-
  getMatrixRows(InputMatrix, FirstRow, SecondRow, ThirdRow),
  length(FirstRow, RowLenght),
  RowLenght = Index,
  getIthElement(FirstRow, Index, NumberToCheck),
  (member(NumberToCheck, SecondRow), member(NumberToCheck, ThirdRow)),
  NumberOfCuts is 0, !.

getCuts(InputMatrix, OutputMatrix, NumberOfCuts, Index):-
  getMatrixRows(InputMatrix, FirstRow, SecondRow, ThirdRow),
  length(FirstRow, RowLenght),
  RowLenght = Index,
  getIthElement(FirstRow, Index, NumberToCheck),
  not((member(NumberToCheck, SecondRow), member(NumberToCheck, ThirdRow))),
  cutMatrixColumn(InputMatrix, OutputMatrix, Index),
  NumberOfCuts is 1.

getMinCuts(InputMatrix, NumberOfCuts):-
  getCuts(InputMatrix, OutputMatrix, NextNumberOfCuts, 1),
  NextNumberOfCuts is 0,
  NumberOfCuts is NextNumberOfCuts, !.

getMinCuts(InputMatrix, BestNumberOfCuts):-
  getCuts(InputMatrix, CutMatrix, NumberOfCuts, 1),
  getMinCuts(CutMatrix, RestCuts),
  BestNumberOfCuts is NumberOfCuts + RestCuts.

mincol(InputMatrix, NumberOfCuts):-
  getMinCuts(InputMatrix, NumberOfCuts).

%SWAPSOCKS

%The expected number of swaps can be translated into the sum of
%swaps needed to match all socks' pairs. Following, this simple idea,
%we simply traverse the list of socks, looking for the other pair of
%our head sock. Then we calculate their distance
%(number of swaps needed to put the second socks on the right of our head).
%This approach doesn't affect the number of swaps needed for the subsequent
%matching of the elements contained within the [head, ..., head_partner].
%Following this greedy approach, for each element, updating the right part
%of the list, we get a simple but effective solution.

removeIthElement([], [], _).

removeIthElement(UncutList, CutList, Index):-
  length(ListWithElement, Index),
  append(ListWithElement, RestUncutList, UncutList),
  append(ListWithoutElement, [ElementToRemove], ListWithElement),
  append(ListWithoutElement, RestUncutList, CutList), !.

findSock([SearchedSock|_], SearchedSock, 1).

findSock([CurrentSock|RestSocks], SearchedSock, Index):-
  not(CurrentSock = SearchedSock),
  findSock(RestSocks, SearchedSock, NextIndex),
  Index is NextIndex + 1.

findMinSwaps([], 0).

findMinSwaps([FirstSock, SecondSock|RestSockList], MinSwaps):-
  not(FirstSock = SecondSock),
  findSock(RestSockList, FirstSock, Index),
  removeIthElement(RestSockList, CutRestSockList, Index),
  append([SecondSock], CutRestSockList, UpdatedRestList),
  findMinSwaps(UpdatedRestList, RestSwaps),
  MinSwaps is RestSwaps + Index, !.

findMinSwaps([PairSock, PairSock|RestSockList], MinSwaps):-
  findMinSwaps(RestSockList, RestSwaps),
  MinSwaps is RestSwaps.

swapsocks(InputList, MinSwaps):-
  findMinSwaps(InputList, MinSwaps).
