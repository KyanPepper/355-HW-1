{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

dropList :: Int -> [a] -> [a]
dropList x [] = []  --On empty list return empty list
dropList 0 xs = xs  --On n = 0 return remaining list
dropList n (x:xs) = dropList (n - 1) xs -- Recursively call drop where n = n -1 (will hit 0 case)



splitAtIndex :: Int -> [a] -> ([a], [a])
splitAtIndex _ [] = ([], [])  --On empty list return tuple of empty lists
splitAtIndex 0 xs = ([], xs)  --On index 0 return single list
splitAtIndex n (x:xs)
  | n > 0 = let (left, right) = splitAtIndex (n - 1) xs --Append head to left list and let base case n - 0 return right list
            in (x : left, right)


concatLists :: [a] -> [a] -> [a]
concatLists [] ys = ys  --If 1 list is empty return non empty list
concatLists (x:xs) ys = x : concatLists xs ys -- xs and ys are lists, each element x is appended the list ys until xs is nothing (base case)


interleaveLists :: [a] -> [a] -> [a]
interleaveLists [] ys = ys  --If 1 list is empty return non empty list
interleaveLists xs [] = xs -- If 1 list is empty return non empty list
interleaveLists (x:xs) (y:ys) = x : y : interleaveLists xs ys  -- Takes head of each list an appends to a new list until both lists are empty


mergeAscending :: Ord a => [a] -> [a] -> [a]
mergeAscending [] ys = ys  -- If 1 list is empty return non empty list
mergeAscending xs [] = xs  -- If 1 list is empty return non empty list
mergeAscending (x:xs) (y:ys) -- if(x <=  y) append head of xs (x) to new list then recurse with rest of xs and y,ys. if not x<=y vice versa with ys
  | x <= y    = x : mergeAscending xs (y:ys)  -- (<=) because if x == y, x is appended first then y is appended which shouldnt make a difference given same value
  | otherwise = y : mergeAscending (x:xs) ys 


mergeDescending :: Ord a => [a] -> [a] -> [a]
mergeDescending [] ys = ys  -- If 1 list is empty return non empty list
mergeDescending xs [] = xs  -- If 1 list is empty return non empty list
mergeDescending (x:xs) (y:ys) -- if(x >=  y) append head of xs (x) to new list then recurse with rest of xs and y,ys. if not x>=y vice versa with ys
  | x >= y    = x : mergeDescending xs (y:ys)   -- (>=) because if x == y, x is appended first then y is appended which shouldnt make a difference given same value
  | otherwise = y : mergeDescending (x:xs) ys  


mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []  -- If list is empty return empty list
mergeSort [x] = [x]  -- If list is one element  its sorted thus do nothing and return
mergeSort xs = 
  let mid = length xs `div` 2  -- Find the middle index
      (left, right) = splitAtIndex mid xs  -- Split the list at the middle index into two lists see (splitAtIndex) for implementation
  in mergeAscending (mergeSort left) (mergeSort right)  -- Recursively call mergeAscending  which will merge the two elements in ascending order see (mergeAscending) for implementation


-- Insert an element into its correct position in a sorted list
insertHelper :: Ord a => a -> [a] -> [a]
insertHelper x [] = [x] -- If list ((Y;ys)) is empty return list with x
insertHelper x (y:ys) -- Inserts new element x into correct position in sorted list
  | x <= y    = x : y : ys  -- Append before the first larger element
  | otherwise = y : insertHelper x ys  -- Recursively find index where x is less then y (head)



insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []  -- if list is empty return empty list
insertionSort (x:xs) = insertHelper x (insertionSort xs) --break down list element by element recursively till sorted see insertHelper for implementation 





