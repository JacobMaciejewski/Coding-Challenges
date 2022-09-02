import Data.List
import Data.Maybe
import Data.Bool
import Data.Set

-- ALLPAIRS

-- simply producing all consecutive points contained
-- within lines connecting (num, 0) -> (0, num)
-- where num is every single positive integer
-- in this way we get the expected sequence of tuples

getSetOfTuples :: Int -> [(Int, Int)]
getSetOfTuples 0 = [(0,0)]
getSetOfTuples y = [(x_current, y - x_current)| x_current <-[y,y-1..0]]


getAllTuples :: [Int] -> [(Int, Int)]
getAllTuples (y_current:ys) = getSetOfTuples y_current ++ getAllTuples ys

allpairs :: [(Int, Int)]
allpairs =  getAllTuples [0..]


-- NEWNUM
-- Following the same approach as in the previous project
-- with some discernible changes. We are not recursively traversing
-- the number to its end and then searching for the digit to swap.
-- With our new appraoch, we simply reverse the number and search
-- for the first digit that is smaller than the previous one and
-- swap it with the smallest digit on the right that is bigger than the found digit
-- In the end, we simply sort in descending order the right end of the number
-- and swap the whole sequence of digits.

listToNumber:: [Int] -> Int
listToNumber numberAsList =
  let firstDigit = head numberAsList
      restDigits = tail numberAsList
      in if Data.List.null restDigits
         then firstDigit
         else listToNumber restDigits + firstDigit * 10^ length restDigits

numToList :: Integral a => a -> [a]
numToList 0 = [0]

numToList num =
  let division = num `div` 10
      moduloAsList = [num `mod` 10]
    in if num < 10
    then [num]
    else numToList division ++ moduloAsList

swapElementsAt :: Int -> Int -> [Int] -> [Int]
swapElementsAt i j xs = let elemI = xs !! i
                            elemJ = xs !! j
                            left = take i xs
                            middle = take (j - i - 1) (drop (i + 1) xs)
                            right = drop (j + 1) xs
                    in  left ++ [elemJ] ++ middle ++ [elemI] ++ right

isDescending :: [Int]->Bool
isDescending givenList =
  let listTail = tail givenList
  in case givenList of
  (x:y:xs) -> if x >= y
            then isDescending listTail
            else False
  [x] -> True
-- function returnes a tuple
getFirstBiggerNumber :: [Int]-> Int -> (Int,Int)
getNumBigger [x] = (0,0)

getFirstBiggerNumber numberAsList index =
  let numberAsListTail = tail numberAsList
      nextIndex = index + 1
  in case numberAsList of
  x:y:xs -> if x > y
            then (x,index)
            else getFirstBiggerNumber numberAsListTail nextIndex
  [x] -> (0,index)

getBiggerNums :: Int->[Int]->[Int]
getBiggerNums numberToSwap [] = []
getBiggerNums numberToSwap (x:xs) =
  if x > numberToSwap
  then x : getBiggerNums numberToSwap xs
  else getBiggerNums numberToSwap xs

newnum :: Int->Int

newnum inputNumber =
  let numberAsList = numToList inputNumber
  in if isDescending numberAsList
  then 0
  else coreNewNum numberAsList

coreNewNum :: [Int]->Int

coreNewNum numberAsList =
  let -- we reverse the list as we have to start the traversal from the end
      reversedList = reverse numberAsList
      (firstDigit, previousDigitIndex) = getFirstBiggerNumber reversedList 0
      -- index of the digit that will be swapped
      -- previousDigitIndex = fromJust (elemIndex firstDigit reversedList)
      firstDigitIndex = previousDigitIndex + 1
      -- getting the index of the second digit that we want to swap
      secondDigitIndex = getSecondDigitIndex firstDigitIndex reversedList
      -- producing a list with the two digits swapped
      listWithSwappedElements = swapElementsAt secondDigitIndex firstDigitIndex reversedList
      -- gettinf the final list of digits that make up for the next bigger number
      finalDigitsList = getFinalNumberList firstDigitIndex listWithSwappedElements
  in  listToNumber finalDigitsList

getSecondDigitIndex :: Int -> [Int] -> Int
getSecondDigitIndex firstDigitIndex givenList =
  let
    -- digit that we want to swap
    firstDigit = givenList!!firstDigitIndex
    -- all digits to the right of the first swapped digit
    subListToCheck = take firstDigitIndex givenList
    -- getting all possible candidates for swapping
    biggerThanFirstDigits = getBiggerNums firstDigit subListToCheck
    -- finding the second candidate for swap
    secondDigit = minimum biggerThanFirstDigits
    -- getting the index of the second digit that we want to swap
    secondDigitIndex = elemIndex secondDigit subListToCheck
    -- changing Maybe Int -> Int
    secondDigitIndexAsInteger = fromJust secondDigitIndex
  in secondDigitIndexAsInteger

getFinalNumberList :: Int -> [Int] ->[Int]
getFinalNumberList secondNumberToSwapIndex listWithSwappedElements =
  let -- getting the part of the number to the right of the swapped digit
      subListToSort = take secondNumberToSwapIndex listWithSwappedElements
      -- getting the part of digit list that remained intact
      endOfList = stripPrefix subListToSort listWithSwappedElements
      -- sorting in descending order all digits right to the swapped one
      sortedSublist = sortOn negate subListToSort
      -- connecting right part of the number with the intact one
      finalList = sortedSublist++fromJust endOfList
      -- re-reversing the number so we get the final list
  in reverse finalList

-- LEXICOGRAPHIC
-- Realising, we have to produce at most 3 prefixes that being reversed, will
-- add up to the lexicographically smallest expression, we follow a new approach.
-- We simply produce all the possible triplets (some of them may be empty),
-- which in total are less than O(n^2). N being the length of the given string.
-- This implies that brute force approach is pretty effective.

lexicographic :: String -> String

lexicographic inputString =
  let stringLength = length inputString
      stringReversed = reverse inputString in
    -- inserted one character
    if stringLength <= 1
      then inputString
    -- only two cases when given string has lenght 2
    else if stringLength == 2
      then if inputString < stringReversed
        then inputString
        else stringReversed
    else findBestCut inputString stringReversed

findBestCut :: String -> String -> String

findBestCut givenString givenStringReversed =
  let stringLength = length givenString
      sectionsInTuples = getSections stringLength
      bestString = getBestCut givenString sectionsInTuples
  in if bestString < givenStringReversed
     then bestString
     else givenStringReversed

getBestCut :: String -> [(Int, Int)] -> String

getBestCut initialString sections =
  let (firstPrefixLength, secondPrefixLenght) = head sections
      restSections = tail sections
      firstPrefix = reverse (take firstPrefixLength initialString)
      lesserString = drop firstPrefixLength initialString
      secondPrefix = reverse (take secondPrefixLenght lesserString)
      leftString = drop secondPrefixLenght lesserString
      newString = firstPrefix ++ secondPrefix ++ reverse leftString
   -- no more tuples left, recurse back
   in if Data.List.null restSections
      then newString
      -- comparing new string with the next one
      else let nextString = getBestCut initialString restSections
      in if newString < nextString
          then newString
         else nextString

getSections :: Int -> [(Int, Int)]
-- getting all tuples representing the length of the first and second prefix
-- third one easily conducted from the first two
getSections strLen =
  [(x,y)| x <-[strLen-1, strLen-2..1], y <- [1..strLen - x]]

-- PREDICTZOO
-- Producing the whole sequence of zoos, may be pretty ineffective
-- even impossible, when we take into consideration strings with
-- length bigger than some million characters. We simply simulate the construction
-- of the sequence following a pattern described below. First of all, we check
-- if the index of the searched character is contained within current s(i), if so
-- it can only be contained in the middle part (that connects two instanced of s(i-1))
-- or in the second part of the s(i) that is equal to s(i-1). If not, we simply decrement
-- the index of the searched character and we simulate the construction of s(i+1).
-- The pattern in described in the comments following the main function of our program.

-- main function that simply decides which method we will follow
predictzoo :: Int -> Char

predictzoo position =
  if position <= 3 then
  initialSequenceElement position
  -- starting recursion from s(1) where we have
  -- where the previous string was zoo (size 3)
  -- current string is zoozooozoo (size 10)
  -- current middle string is zooo (size 4)
  else let previousStringSize = 3
           middleStringSize = 4
           currentStringSize = 10
  in searchInNextString position previousStringSize middleStringSize currentStringSize

-- called only when we want to get one of the first 3 characters
-- base case for all searches
initialSequenceElement :: Int -> Char
initialSequenceElement position =
  if position == 1 then 'z'
  else 'o'

inCurrentString :: Int -> Int -> Bool
inCurrentString position stringLenght =
  -- character's position contained within current string boundaries
  if position <= stringLenght
  then True
  else False


inSecondPart :: Int -> Int -> Bool
inSecondPart position lengthUpToSecondPart =
  if position > lengthUpToSecondPart
    then True
  else False

searchInNextString position previousStringSize middleStringSize currentStringSize =
  if position <= 3
    then initialSequenceElement position
    -- position in current string
  else if inCurrentString position currentStringSize
        -- character contained after the middle string
        then if inSecondPart position (previousStringSize + middleStringSize)
              then let positionInSubstring = position - (previousStringSize + middleStringSize)
                   -- following the pattern we will always come across s(1) string
                   in searchInNextString positionInSubstring 3 4 10
              -- character contained in the middle string
             else let positionInMiddleString = position - previousStringSize in
                    if positionInMiddleString == 1
                    -- first character of middle string is always 'z'
                    then 'z'
                    else 'o'
  -- character is not contained in current string, simulating S(i+1)
  -- checking it from middle string and later (first part has already been checked)
  else let nextMiddleStringSize = middleStringSize + 1
           nextStringSize = 2 * currentStringSize + nextMiddleStringSize
       -- position remains the same, checking for the updated string
       in searchInNextString position currentStringSize nextMiddleStringSize nextStringSize

-- PHOTOCOST
-- Being unable to find a set formula for group production, we decide to
-- follow a greedy approach, where we initially produce an acceptable group
-- containing representatives of all teams and then we systematically update it
-- in order to produce the cheapest one. In more detail, we sort the tuples in
-- in regard to the axis at which the children are placed. In this way we can
-- proceed with our greedy approach, following the x axis. In each step, we check
-- whether the first child of our group has a teammate from the same team within
-- the group, implying it can be removed from it. In the other case, we check whether
-- there is a similar teammate in the children that we haven't scanned yet. If so,
-- we check if the replacement of current head child with the new one, produces a cheaper
-- final result. In this way, our group will be always updated if a better children choice
-- is found and will end if we run out of children to check.


-- main function gets a list of tuples (x_axis, team)
-- and returns the cost of the smallest group containing
-- representatives of all teams
photocost :: [(Int, Int)] -> Int
photocost [] = 0

photocost childrenList =
 -- it is easier to traverse the tuples list as an x axis
 let childrenListSorted = sort childrenList
     -- constructing the first group containing representatives of all groups
     firstAcceptableGroup = getFirstAcceptableGroup childrenListSorted
 in getMinimumCost firstAcceptableGroup childrenListSorted

getMinimumCost :: ((Int, Int), (Int, Int)) -> [(Int, Int)] -> Int

getMinimumCost currentPhoto childrenToScan =
 -- getting the data of the first and last child of current photo
 let (firstChildData, lastChildData) = currentPhoto
     -- firstChildPos lastChildPosition
     costOfCurrentPhoto = getCost firstChildData lastChildData
     -- (scannedChildPos, scannedChild) = head childrenToScan
     currChildData = head childrenToScan
     leftChildren = tail childrenToScan in
 -- first child already represented in current photo
 if alreadyInPhoto currentPhoto leftChildren
   -- checking the cost of the updated photo without it
 then getMinimumCost (currChildData, lastChildData) leftChildren
 -- no child of the same team within photo, looking in the left children
 else let sameTeamChild = getOtherChild firstChildData lastChildData leftChildren
   -- no child of the same team left
   -- we found the lowest cost team
   in if isNothing sameTeamChild
     then costOfCurrentPhoto
     -- checking if the photo with the new representative is cheaper
     -- returning the lowest of the two values
     else let newLastChild = fromJust sameTeamChild
              otherPhotoCost = getMinimumCost (currChildData, newLastChild) leftChildren
     in min otherPhotoCost costOfCurrentPhoto

getOtherChild :: (Int, Int) -> (Int, Int) -> [(Int, Int)] -> Maybe (Int, Int)

getOtherChild firstChildData lastChildData leftChildren =
 let (firstChildPos, firstChildTeam) = firstChildData
     (lastChildPos, lastChildTeam) = lastChildData
     childrenAfterPhoto = getUnCheckedChildren lastChildPos leftChildren
 -- searching for other representative in the list
 -- of the children following our photo
 in getChildOfTeam firstChildTeam childrenAfterPhoto

getChildOfTeam :: Int -> [(Int, Int)] -> Maybe (Int, Int)
-- traversed the whole list, no other representative
getChildOfTeam team [] = Nothing

getChildOfTeam team childrenToCheck =
 let (currChildPosition, currChildTeam) = head childrenToCheck
     restChildren = tail childrenToCheck
 -- found another child from the same team, following our photo
 in if currChildTeam == team then
   Just (currChildPosition, currChildTeam)
   else getChildOfTeam team restChildren


getUnCheckedChildren :: Int -> [(Int, Int)] -> [(Int, Int)]
-- got to the end of the children list, no second representative found
getUnCheckedChildren _ [] = []

getUnCheckedChildren lastChildPos listOfChildren =
 let (currChildPos, currChildTeam) = head listOfChildren
     restChildren = tail listOfChildren
 -- found the last child of the photo, getting the following children
 in if lastChildPos == currChildPos then restChildren
 else getUnCheckedChildren lastChildPos restChildren


alreadyInPhoto :: ((Int, Int),(Int, Int)) -> [(Int, Int)] -> Bool
-- traversed the whole photo, no player from the same team
alreadyInPhoto currentPhoto [] = False

alreadyInPhoto currentPhoto childrenToScan =
 let ((firstChildPos, firstChildTeam),(lastChildPosition, lastChildTeam)) = currentPhoto
     (currChildPos, currChildTeam) = head childrenToScan
     restChildrenToScan = tail childrenToScan in
 -- got to the end of the photo, last check
 if endOfPhoto lastChildPosition currChildPos
 then firstChildTeam == currChildTeam
 -- continue traversing through photo
 else if firstChildTeam == currChildTeam then True
      else alreadyInPhoto currentPhoto restChildrenToScan

endOfPhoto :: Int -> Int -> Bool
-- position of current child, equal to the last child of the photo
endOfPhoto lastChildPosition currentChildPosition =
 lastChildPosition == currentChildPosition


getCost :: (Int, Int) -> (Int, Int) -> Int
-- cost is equal to distance between two end children of the photo
getCost firstChildData lastChildData =
 let (firstChildPosition, firstChildTeam) = firstChildData
     (lastChildPosition, lastChildTeam) = lastChildData
 in lastChildPosition - firstChildPosition

-- function returns all football teams in a form of set
-- for easier update through following recursions
getAllTeams :: [(Int, Int)] -> Set Int
getAllTeams tuplesList =
 let teamsAsList = getTeamsList tuplesList []
 in Data.Set.fromList teamsAsList

-- help function, constructing a list with all teams
getTeamsList :: [(Int, Int)] -> [Int] -> [Int]
-- got to the end of the list
getTeamsList [] _ = []
getTeamsList tuplesList teamsFoundList =
 let (currentX, currentTeam) = head tuplesList
     restOfTuples = tail tuplesList
 in if currentTeam `elem` teamsFoundList
 then teamsFoundList ++ getTeamsList restOfTuples teamsFoundList
 else let newTeamsList = currentTeam : teamsFoundList
 in newTeamsList ++ getTeamsList restOfTuples newTeamsList

-- producing the first full group containing representatives
-- of all possible teams
getFirstAcceptableGroup :: [(Int, Int)] -> ((Int, Int), (Int, Int))
getFirstAcceptableGroup listOfChildren =
 let firstChildOfGroup = head listOfChildren
     allPossibleTeams = getAllTeams listOfChildren
     finalChildOfGroup = getLastChildOfGroup allPossibleTeams listOfChildren
 in (firstChildOfGroup, finalChildOfGroup)

-- traversing all children, until we get representatives from all teams
getLastChildOfGroup :: Set Int -> [(Int, Int)] -> (Int, Int)

getLastChildOfGroup footballTeams listOfChildren =
 -- getting the data from the current child
 let (currentChildPosition, currentChildTeam) = head listOfChildren
     restChildren = tail listOfChildren in
 -- found a representative of a team
 if member currentChildTeam footballTeams
 then let updatedTeamSet = Data.Set.delete currentChildTeam footballTeams in
 -- found the representative of the last missing team
   if Data.Set.null updatedTeamSet then
     (currentChildPosition, currentChildTeam)
   else getLastChildOfGroup updatedTeamSet restChildren
 else getLastChildOfGroup footballTeams restChildren

-- SEPARATE
-- Each cell of the given array, is represented with a x,y coordinates tuple.
-- We simply traverse the whole array, at each step checking the sets of groups, to which
-- the 8 coordinates of current cell's neighbours belong to. This obviously happens only in the case
-- when the current cell contains a 1. If we have only one neighbour contained in an existing
-- set, we simply add the current coordinate into that set. If we have more than one neighbour belonging
-- to a different group, this implies the current cell would connect them, so they are being merged,
-- previous sets removed from the list, which is then updated with the merged set.
-- In the case when we have no neighbours within an existing group of sets, we produce a new group
-- with the current coordinate as its only element. This approach, guarantees that in the end of
-- the traversal, we will be left with two sets, representing the coordinates of the two 1s groups.
-- We choose the bigger one and update the coordinates in the initial array that are contained within
-- the bigger set.

-- functions takes as input an array of 0s and 1s
-- replaces the biggest 1s group with 2s
separate :: [[Int]] -> [[Int]]

separate xss =
 -- constructing two sets, that will contain
 -- coordinates of cells contained in the two 1s groups
 let arraySets = getArraySets (1,1) xss [] in
     -- both groups empty, returning array as it is
     if length arraySets == 0 then
       xss
    -- there is only group of 1s, other is empty
     else if length arraySets == 1 then
       let set1 = arraySets!!0
           coorList = elems set1
           -- have to sort the coordinates in respect to y_axis first
           coordinatesToUpdate = [(a,b)|(foo,a,b)<-sort[(abs b,a,b)|(a,b)<-coorList]]
           in constructUpdatedArray (1,1) xss coordinatesToUpdate
     else let set1 = arraySets!!0
              set2 = arraySets!!1
              -- bigger 1s group is contained in the first set
              in if size set1 > size set2 then
                  let coorList = elems set1
                      -- have to sort the coordinates in respect to y_axis first
                      coordinatesToUpdate = [(a,b)|(foo,a,b)<-sort[(abs b,a,b)|(a,b)<-coorList]]
                  in constructUpdatedArray (1,1) xss coordinatesToUpdate
                 else let coorList = elems set2
                          coordinatesToUpdate = [(a,b)|(foo,a,b)<-sort[(abs b,a,b)|(a,b)<-coorList]]
                          in constructUpdatedArray (1,1) xss coordinatesToUpdate

constructUpdatedArray :: (Int, Int) -> [[Int]] -> [(Int, Int)] -> [[Int]]
-- got to the end of the array
constructUpdatedArray _ [] _ = []

constructUpdatedArray (x_axis, y_axis) givenArray coordinatesToUpdate =
 let currentRow = head givenArray
     restRows = tail givenArray
     numOfRowCoordinates = getNumOfCoordinates y_axis coordinatesToUpdate
     -- spliting the list of coordinates to the ones contained within current row and next ones
     (rowCoordinates, restCoordinates) = splitAt numOfRowCoordinates coordinatesToUpdate
     updatedCurrentRow = constructUpdatedRow (x_axis, y_axis) currentRow rowCoordinates
     -- constructing following row
     in [updatedCurrentRow] ++ constructUpdatedArray (x_axis, y_axis + 1) restRows restCoordinates

-- checking for coordinates within 2s group and updating them, keeping other elements intact
constructUpdatedRow :: (Int, Int) -> [Int] -> [(Int, Int)] -> [Int]
-- got to the end of the row
constructUpdatedRow _ [] _ = []

constructUpdatedRow (x_axis, y_axis) givenRow rowCoordinates =
 let currentElement = head givenRow
     otherElements = tail givenRow in
     -- no need to update if no more coordinates left of 2s team or coordinate contains 0
     if Data.List.null rowCoordinates || currentElement == 0
       then currentElement : constructUpdatedRow (x_axis + 1, y_axis) otherElements rowCoordinates
     else let coordinateToUpdate = head rowCoordinates
              restCoordinates = tail rowCoordinates
          -- found member of the 2s group
          in if (x_axis, y_axis) == coordinateToUpdate
            then 2 : constructUpdatedRow (x_axis + 1, y_axis) otherElements restCoordinates
            -- found a 1 but not a member of the 2s group
            else 1 : constructUpdatedRow (x_axis + 1, y_axis) otherElements rowCoordinates

-- returning the number of coordinates that are members of the 2s group
getNumOfCoordinates :: Int -> [(Int, Int)] -> Int

getNumOfCoordinates y_axis [] = 0

getNumOfCoordinates y_axis coordinates =
 let (curr_x, curr_y) = head coordinates
     restCoordinates = tail coordinates in
     if curr_y == y_axis
       then 1 + getNumOfCoordinates y_axis restCoordinates
     else 0

neighBoursInSet :: [(Int,Int)] -> Set (Int, Int) -> Bool

-- traversed all of the neighbours, none in given set
neighBoursInSet [] givenSet =
 False

neighBoursInSet neighbours givenSet =
 let currentNeighbour = head neighbours
     restNeighbours = tail neighbours in
     -- current neighbour is in the set, condition satisfied
     if member currentNeighbour givenSet
       then True
     else neighBoursInSet restNeighbours givenSet

getArraySets :: (Int, Int) -> [[Int]] -> [Set (Int, Int)] -> [Set (Int, Int)]

-- traversed the whole array, returning list with two groups' sets
getArraySets _ [] finalSetList =
 finalSetList

getArraySets (x_axis, y_axis) arrayToScan currentSetList =
 let rowToScan = head arrayToScan
     restRowsToScan = tail arrayToScan
     -- getting the updated list from current row
     updatedSetList = getRowSets (x_axis, y_axis) rowToScan currentSetList
     -- repeating for the rest of the array
     in getArraySets (x_axis, y_axis + 1) restRowsToScan updatedSetList

getRowSets :: (Int, Int) -> [Int] -> [Set (Int, Int)] -> [Set (Int, Int)]
-- got to the end of the row, returning updated set list
getRowSets _ [] finalRowSetList =
 finalRowSetList

getRowSets (x_axis, y_axis) rowToScan rowSetList =
 let currentElement = head rowToScan
     restElements = tail rowToScan
     neighbours = getNeighbours (x_axis, y_axis)
     -- finding the sets in which the neighbours are contained in
     neighboursSets = getNeighboursSets neighbours rowSetList in
     -- current element is just a zero, we continue leaving sets intact
     if currentElement == 0 then
       getRowSets (x_axis + 1, y_axis) restElements rowSetList
     -- no neighbour belongs to an existing group set
     -- constructing a new group set
     else if Data.List.null neighboursSets then
       let newGroupSet = fromList [(x_axis, y_axis)]
           updatedRowSetList = newGroupSet : rowSetList in
             getRowSets (x_axis + 1, y_axis) restElements updatedRowSetList
     -- only one neighbour belongs in an existing group set
     -- just updating the proper set
     else if length neighboursSets == 1 then
           let setToUpdate = head neighboursSets
               updatedSet = Data.Set.insert (x_axis, y_axis) setToUpdate
               updatedRowSetList = replaceSet rowSetList setToUpdate updatedSet in
               --added current coordinate into neighbour's set, can continue
                 getRowSets (x_axis + 1, y_axis) restElements updatedRowSetList
     -- current coordinate surrounded by more than one groups, mergine them
     else let mergedSet = Data.Set.unions neighboursSets
              newSet = Data.Set.insert (x_axis, y_axis) mergedSet
              -- removing old groups of neighbours
              newRowSetList = removeGroupSets neighboursSets rowSetList
              -- new list contains the merged groups of neighbours with current coordinate
              updatedRowSetList = newSet : newRowSetList in
                getRowSets (x_axis + 1, y_axis) restElements updatedRowSetList

removeGroupSets :: [Set (Int, Int)] -> [Set (Int, Int)] -> [Set (Int, Int)]
-- got to the end of the list
removeGroupSets _ [] = []

-- got rid of all neighbours groups, just get the rest of list intact
removeGroupSets [] setList =
 setList

removeGroupSets toRemoveSets setsList =
 let toRemoveSet = head toRemoveSets
     restToRemoveSets = tail toRemoveSets
     currentSet = head setsList
     restSets = tail setsList in
     -- found a set to remove
     if currentSet == toRemoveSet
       then removeGroupSets restToRemoveSets restSets
     -- current set should not be removed
     else currentSet : removeGroupSets toRemoveSets restSets

-- replaces replacedSet with newSet in the listOfSets
replaceSet :: [Set (Int, Int)] -> Set (Int, Int) -> Set (Int, Int) -> [Set (Int, Int)]

replaceSet [] _ _ = []

replaceSet listOfSets replacedSet newSet =
 let currentSet = head listOfSets
     restSets = tail listOfSets in
     -- found the set to update
     if currentSet == replacedSet then
       newSet : restSets
     else
       currentSet : replaceSet restSets replacedSet newSet

getNeighboursSets :: [(Int, Int)] -> [Set (Int, Int)] -> [Set (Int, Int)]
-- checked all existing sets of groups
getNeighboursSets neighbours [] = []

getNeighboursSets neighbours rowSetList =
 let currentSet = head rowSetList
     restSets = tail rowSetList
     foundNeighbouringSet = neighBoursInSet neighbours currentSet
 -- current set contains at least one neighbour
 in if foundNeighbouringSet
     then currentSet : getNeighboursSets neighbours restSets
    -- there is no neighbour in current set
    else getNeighboursSets neighbours restSets

--producing 8 tuples, representing the neighbours of a coordinate
getNeighbours :: (Int, Int) -> [(Int, Int)]

getNeighbours (x_axis, y_axis) =
  let wholeGroup = [(x_axis + offset1,y_axis + offset2)|offset1<-[-1..1], offset2<-[-1..1]]
  -- group consists of neighbours and current coordinate
  in Data.List.delete (x_axis, y_axis) wholeGroup
