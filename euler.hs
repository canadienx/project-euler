-- Utility Functions

--- Naive primes
primes :: [Integer]
primes = sieve [2..]
    where
      sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]

--- Prime factors
primeFactors :: Integer -> [Integer]
primeFactors n = factor n primes
    where factor n (p:ps) | p * p > n = [n]
                          | n `mod` p /= 0 = factor n ps
                          | otherwise = p : factor (n `div` p) (p:ps)

--- Factorial
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = product [1..n]              
                                        
--- Palindrome
isPalindrome :: String -> Bool
isPalindrome s
    | length s == 1 = True
    | length s == 2 = head s == last s
    | otherwise     = (head s == last s) &&  (isPalindrome $ drop 1 $ init s)

isNumPalindrome :: Integer -> Bool
isNumPalindrome n = isPalindrome $ show n

-- Problem 1
euler1 = sum [n | n <- [1..1000-1], n `mod` 3 == 0 || n `mod` 5 == 0]

-- Problem 2
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

euler2 :: Integer -> Integer
euler2 max = sum (takeWhile (<= max) (filter even fibs))

-- Problem 3
euler3 :: Integer -> Integer
euler3 n = maximum $ primeFactors n

-- Problem 4
euler4 :: Integer
euler4 = maximum $ filter (isNumPalindrome) [n*m | n <- [100..999], m <- [100..999]]

-- Problem 5
euler5 :: Integer -> Integer
euler5 n = foldr1 (lcm) [1..20]

-- Problem 6
euler6 :: Integer -> Integer
euler6 n = (m * m) - (sum [r*r | r <- [1..n]])
    where m = sum [1..n]