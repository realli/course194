-- convert interger number to a list of digit
-- so toDigits 199 = [1,9,9]
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

-- convert interger number to a list of digits (revers)
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0 = []
    | otherwise = let remaider = n `mod` 10
                      divs = n `div` 10
                  in remaider : toDigitsRev divs

-- double every other one of list of digits
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther ls = doubleIt' ls False
    where doubleIt' [] _ = []
          doubleIt' (x:xs) True = x*2 : doubleIt' xs False
          doubleIt' (x:xs) False = x : doubleIt' xs True

-- sum the digits
-- sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)

-- validate function of credit card
validate :: Integer -> Bool
validate n = if sumValues `mod` 10 == 0 then True else False
    where sumValues = sumDigits . doubleEveryOther . toDigitsRev $ n
