import qualified System.Random
import qualified Data.List

--Problem 1
-- Find the last element of a list
myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs

--Problem 2
-- Find the second to last element of a list
secondToLast :: [a] -> a
secondToLast [x,_] = x
secondToLast (_:xs) = secondToLast xs

--Problem 3
-- Find the K'th element of a list. The first element in the list is number 1.
elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt [] _    = error "Index out of bounds"
elementAt (_:xs) k
	| k < 1 	  = error "Index out of bounds"
	| otherwise   = elementAt xs (k - 1)
	
--Problem 4
-- Find the number of elements of a list.
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

--Problem 5
-- Reverse a list.
myReverse :: [a] -> [a]
myReverse (x:xs) = myReverse xs ++ [x]
myReverse [] = []

--Problem 6
--Find out whether a list is a palindrome.
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome []  = True
isPalindrome [x] = True
isPalindrome (x:xs)
	| x == last xs = isPalindrome $ init xs
	| otherwise = False
	
--Problem 7
-- Flatten a nested list structure
-- [[1,2,3],[4,5,6],[7,8,9]] => [1,2,3,4,5,6,7,8,9]
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem a) 	  = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List []) 	  = []

--Problem 8
-- Eliminate consecutive duplicates of list elements
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs)
	| xs == [] = [x]
	| x == head xs = compress xs
	| otherwise = [x] ++ compress xs

--Problem 9
-- Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements, it should be packed in separate lists.
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x: takeWhile (==x) xs) : pack (dropWhile (==x) xs)

--Problem 10
-- Run-length encoding of a list. Conserve duplicates of elements
-- are encoded as lists (N, E) where N is the number of duplicates of element E.
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode (x:xs) = (length $ x : takeWhile (==x) xs, x) : encode (dropWhile (==x) xs)

--Problem 11
-- Modify the results of problem 10 in such a way that if an element has no
-- duplicates it is simply copied into the list.
data ListItem a = Single a | Multiple Int a 
	deriving (Show)
encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified = map encodeHelper . encode
	where
		encodeHelper (1,x) = Single x
		encodeHelper (n,x) = Multiple n x
--Problem 12
--Decode a run-length encoded list generated in problem 11.
decodeModified :: (Eq a) => [ListItem a] -> [a]
decodeModified = concatMap decodeHelper
	where
		decodeHelper (Single x) = [x]
		decodeHelper (Multiple n x) = replicate n x
		
--Problem 14
-- Duplicate the elements of a list.
dupe :: [a] -> [a] 
dupe [] = []
dupe (x:xs) = replicate 2 x ++ dupe xs

--Problem 15
-- Replicate the elements of a list a given number of times.
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) k = replicate k x ++ repli xs k

--Problem 16
-- Drop every N'th element of a list.
dropEvery :: [a] -> Int -> [a]
dropEvery (x:xs) n = dropEveryHelper (x:xs) n 1
	where
		dropEveryHelper (x:xs) n k = (if (mod k n == 0) then 
			[] else
			[x])
			++ (dropEveryHelper xs n (k+1))
		dropEveryHelper [] _ _ = []

--Problem 17
-- Split a list into two parts; the length of the first part is given
-- Do not use any predefined predicates.
split :: [a] -> Int -> ([a],[a])
split (x:xs) n | n > 0 = let (f,l) = split xs (n-1) in (x : f, l)
split xs _ 			   = ([], xs)

--Problem 18
-- Implement python's slicing
slice :: [a] -> Int -> Int -> [a]
slice xs i k | i > 0 = take (k-i + 1) $ drop (i-1) xs

--Problem 19
-- Rotate a list N places to the left.
rotate :: [a] -> Int -> [a]
rotate xs k 
	| (k > 0) = (drop k xs) ++ (take k xs)
	| (k < 0) = (drop (length xs + k) xs) ++ (take (length xs + k) xs)
	| otherwise = error "Cannot rotate by 0"
	
--Problem 20
-- Remove the K'th element from a list.
removeAt :: Int -> [a] -> [a]
removeAt k xs | k > 0 = let (ys, zs) = splitAt k xs in ys ++ (tail zs)

--Problem 21
-- Insert an element at a given position into a list.
insertAt :: a -> [a] -> Int -> [a]
insertAt ys xs n | n > 0 = let (first, last) = splitAt n xs 
						   in first ++ [ys] ++ last
--Problem 22
-- Create a list containing all integers within a given range.
range x y = [x..y]

--Problem 23
-- Extract a given number of randomly selected elements from a list.
rnd_select xs n = do
	gen <- System.Random.getStdGen
	return $ take n [xs !! x | x <- System.Random.randomRs (0 , (length xs) -1) gen]

--Problem 24
-- Lotto: Draw N different random numbers from the set 1..M
diff_select :: Int -> Int -> System.Random.StdGen -> [Int]
diff_select n m = take n . Data.List.nub . System.Random.randomRs (1,m)

--Problem 25
-- Generate a random permutation of the elements of a list
rnd_permu :: [a] -> IO [a]
rnd_permu []	 = return []
rnd_permu (x:xs) = do
	rand <- System.Random.randomRIO (0, (length xs))
	rest <- rnd_permu xs
	return $ let (ys, zs) = splitAt rand rest
			 in ys ++ (x:zs)
--Problem 26
-- Generate the combinations of K distinct objects chosen from the N elements
-- of a list.
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = return []
combinations n xs = do 
			y:xs' <- Data.List.tails xs
			ys <- combinations (n-1) xs'
			return (y:ys)

--Problem 27
-- Group the elements of a set into disjoint subsets.

--Problem 28
-- Sorting a list of lists according to the length of sublists

--Problem 31
--Determine whether a given integer is prime.
isPrime :: Integral a => a -> Bool
isPrime n = isPrime' n 2
	where
		isPrime' n i
			| (n < 2) = error "isPrime cannot be called with n < 2."
			| (n == 2) = True
			| (mod n i == 0) = False
			| (i * i == n) = False
			| (i * i > n) = True
			| (i * i < n) = isPrime' n (i+1)
--Problem 32
-- Determine the greatest common divisor of two positive integer numbers.
-- Use the Euclidean algorithm.
myGCD :: Integer -> Integer -> Integer
myGCD a b
	|(b == 0) = a
	| otherwise = gcd b (mod a b)
	
--Problem 33
-- Determine whether two positive integer numbers are coprime. Two numbers
-- are coprime if their greatest common divisor equals 1.
coprime :: Integer -> Integer -> Bool
coprime a b 
	| ((myGCD a b) == 1) = True
	| otherwise = False

--Problem 34
-- Calculate Euler's totient function
totient :: Integer -> Integer
totient m = totient' m (m-1)
	where
		totient' m n
			| (n == 1) = 1
			|(coprime m n) = 1 + totient' m (n-1)
			| otherwise = 0 + totient' m (n -1)

--Problem 35
-- Determine the prime factors of a given positive integer.
-- Construct a flat list containing the prime factors in ascending order.
primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors n = let divisors = dropWhile ( (/=0) . mod n) [2 .. ceiling $ sqrt $ fromIntegral n]
			     in let prime = if null divisors then n else head divisors
			        in (prime : ) $ primeFactors $ div n prime
					
--Problem 36
-- Determine the prime factors of a given positive integer.
-- Construct a list containing the prime factors and their multiplicity.
prime_factors_mult :: Integer -> [(Integer, Int)]
prime_factors_mult n = encode' $ primeFactors n
	where 
		encode' [] = []
		encode' (x:xs) = (x, length $ x:takeWhile (==x) xs) : encode' (dropWhile (==x) xs)