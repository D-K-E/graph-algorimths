{-
Module      :  Simple Utility Functions
Description :
Copyright   :  Kaan Eraslan
License     :  Mit

Maintainer  :  Kaan
Stability   :  unstable
Portability :  don't know
-}

copyList :: [a] -> [a]
switchItemLocationsInList :: [a] -> Int -> Int -> [a]

append2List :: [a] -> i -> [a]
append2List lst item = lst:item

prepend2List :: [a] -> i -> [a]
prepend2List lst item = item:lst

copyUntilPosition :: [a] -> Int -> [a]
copyUntilPosition lst position
  | position == 0 = []
  | position == length lst - 1 = lst
  | null lst = []
  | otherwise = copyWithAccumulator 0 lst position
  where copyWithAccumulator acc (x:xs) pos = if acc == pos
                                             then x:xs
                                             else x : copyWithAccumulator (acc+1) xs pos

copyFromPosition :: [a] -> Int -> [a]
copyFromPosition lst position
  | position == 0 = lst
  | position == length lst - 1 = []
  | null lst = []
  | otherwise = let lstlen = length lst
                in copyWithAccumulator lstlen lst position
                   where copyWithAccumulator accu (x:xs) pos = if accu == pos
                                                               then x:xs
                                                               else x : copyWithAccumulator accu xs (pos+1)

insert2List :: [a] -> i -> Int -> [a]
insert2List (x:xs) item position
  | position >= length (x:xs) = error "position greater than index of list"
  | position < 0 = error "position smaller than 0"
  | null (x:xs) = [item]
  | otherwise = let lstUptoPos = copyUntilPosition (x:xs) (position-1)
                    lstFromPos = copyFromPosition (x:xs) (position+1)
                    posItem = (x:xs) !! position
                in lstUptoPos:item:posItem:lstFromPos

removeItemFromList :: [a] -> i -> [a]
removeItemFromList (x:xs) item
  | null (x:xs) = []
  | otherwise = if x == item
                then removeItemFromList xs item
                else x : removeItemFromList xs item

popItemFromListByIndex :: [a] -> Int -> [a]
popItemFromListByIndex lst pos
  | null lst = []
  | pos >= length lst = error "position not covered in the index range of list"
  | pos < 0 = error "position smaller than 0"
  | otherwise = iterateList 0 lst pos
  where iterateList acc (x:xs) p = if acc == p
                                   then iterateList (acc+1) xs p
                                   else x : iterateList (acc+1) xs p

findItemPositionInList :: [a] -> i -> Int
findItemPositionInList (l:ls) item 
  | null (l:ls) = error "list is empty"
  | otherwise = iterateList 0 (l:ls) item
  where iterateList acc (x:xs) item
    | null (x:xs) = -1 
    | otherwise = if x == item
                  then acc
                  else iterateList (acc+1) xs item

findItemPositionsInList :: [a] -> i -> [Int]
  | null (l:ls) = error "list is empty"
  | otherwise = iterateList 0 (l:ls) item
  where iterateList acc (x:xs) item
    | null (x:xs) = -1 
    | otherwise = if x == item
                  then acc : iterateList (acc+1) xs item
                  else iterateList (acc+1) xs item

