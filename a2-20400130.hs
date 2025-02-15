-- CISC 360 a2, Winter 2025
-- Jana Dunfield (Q2) and December Stuart (Q1)

-- See a2.pdf for instructions

module A2 where
import Data.List

-- Rename this file to include your student ID: a2-studentid.hs
-- Also, add your student ID number after the "=":
student_id :: Integer
student_id = 20400130

{- If you are working in a group of 2, uncomment the line below and add the second student's ID number: -}
-- second_student_id =
{- If you are working in a group of 2, uncomment the line above. -}


-- THIS FILE WILL NOT COMPILE UNTIL YOU ADD YOUR STUDENT ID ABOVE
check_that_you_added_your_student_ID_above = ()



{- Checklist

   I have read the "Read this first" section,
     including "Reminder", "Try to start early", "Late policy",
     and "Your lowest assignment mark is dropped": 

   I have read the "Version control" section:

   I have read the "IMPORTANT: Your file must compile" section:

   I have read the "Document your code" section:

   I have read the "Strive for simplicity" section:

   I have read the "Be careful with library functions" section:

   I have read the "Test cases" section:

   I have added my student ID above: 

   I am:
      working by myself: 
   OR
      working in a group of 2,
      and I have read the "If you choose to work in a group of 2" section: 
-}


{-
    Q1: Subitize
-}

{- 
  Q1a. Fill in the definition of space_by_thousands below.
  See a2.pdf for instructions.
-}
space_by_thousands :: String -> String
space_by_thousands inputList =
  let firstGroupSize = length inputList `mod` 3
      (firstGroup, leftover) = splitAt (if firstGroupSize == 0 then 3 else firstGroupSize) inputList
  in firstGroup ++ formatGroups leftover
  where
    formatGroups [] = []
    formatGroups remaining =
      let (grouping, leftover) = splitAt 3 remaining
      in " " ++ grouping ++ formatGroups leftover
{- 
  Q1b. Helper functions:
       You are welcome to use these in your definition of subitize_string,
       but you are also welcome to approach the problem in a different way.
-}

{-
  Given a list  l  and an integer  k , splits the list in two after the 
  first  k  elements.
  returns a tuple of:
    ([first k elements of l], [rest of l])
  if k == 0, does not split the list at all and returns (l, [])
-}
first_k_of :: Int -> [a] -> ([a], [a])
first_k_of _ []   = ([], [])
first_k_of 0 rest = (rest, [])
first_k_of k list = splitAt k list

-- Thousands Separator Format spacing function:
-- given the length of the string, returns when the next space should be
tsf :: Int -> Int
tsf n
  | n <= 3         = 0
  | n `mod` 3 == 0 = 3
  | otherwise      = n `mod` 3

-- Indian Separator Format spacing function:
-- given the length of the string, returns when the next space should be
isf :: Int -> Int
isf n 
  | n <= 3    = 0
  | otherwise = (n `mod` 2) + 1 


{-
  Q1b. Fill in the definition of subitize_string below.
  See a2.pdf for instructions.
-}
subitize_string :: (Int -> Int) -> Char -> String -> String
subitize_string f delimiter s = insertSpaces s
  where
    insertSpaces [] = []
    insertSpaces remainingDigits = 
      let (firstGroup, leftover) = first_k_of (f (length remainingDigits)) remainingDigits
      in firstGroup ++ 
         if length leftover == 0 
         then [] 
         else [delimiter] ++ insertSpaces leftover
{-
  Q1c. Write the type and definition of the subitize function below.
  See a2.pdf for instructions.
-}
-- Don't forget to replace "..." with a type, and uncomment the type declaration on the next line:
--  subitize :: ...
subitize :: (Int -> Int) -> a -> [a] -> [a]
subitize f delimiter xs = insertDelimiters xs
  where
    insertDelimiters [] = []
    insertDelimiters remaining =
      let (firstGroup, leftover) = first_k_of (f (length remaining)) remaining
      in firstGroup ++ 
         if length leftover == 0 
         then [] 
         else 
           [delimiter] ++ insertDelimiters leftover
  

{-
  Q1d. Define test cases for your subitize function below.
  Name your tests test_subitize2, test_subitize2, and so on, and add
  them to the list test_all_subitize in order as you do so.
  See a2.pdf for instructions.
-}
test_subitize1 = subitize tsf ' ' "123456" == "123 456"

test_subitize2 = subitize isf '.' "100000000" == "10.00.00.000"

test_subitize3 = subitize tsf ' ' "abc" == "abc"

test_subitize4 = subitize tsf 0 [1,1,1,1,1,1] == [1,1,1,0,1,1,1]

test_subitize5 = subitize tsf '*' "abcdefghij" == "a*bcd*efg*hij"

test_subitize6 = subitize tsf 0 [1,1,1,1,1,1,1] == [1,0,1,1,1,0,1,1,1]

test_subitize7 = subitize tsf "+" ["+","1","b","1","1","1","1"]== ["+", "+", "1", "b", "1", "+", "1", "1", "1"]

test_subitize8 = subitize tsf "" []== []


test_all_subitize = [
  test_subitize1,
  test_subitize2,
  test_subitize3,
  test_subitize4,
  test_subitize5,
  test_subitize6,
  test_subitize7,
  test_subitize8
  ]

-- print these to display the results of the tests you have defined
conjoin :: [Bool] -> Bool
conjoin [] = True
conjoin (q : qs) = q && conjoin qs

passed_all_tests = conjoin test_all_subitize
test_results     = zip (map (\x -> "test" ++ show x) [1..100]) test_all_subitize



{-
  Q2. Songs
-}

data Song = Overlay Song Song
          | Note String
          deriving (Show, Eq)    -- writing Eq here lets us use == to compare Songs
          
{-
  Q2a. sing: See a2.pdf for complete instructions.

  Hint: You may be able to simplify your code by writing a helper function.
-}

sing :: Song -> Song

sing (Overlay (Overlay (Note x) right) _)
  | not (null x) && head x == '1' = right


sing (Overlay (Overlay (Overlay (Note y) s1) s2) s3)
  | not (null y) && head y == 'S' = Overlay (Overlay s1 s3) (Overlay s2 s3)

sing (Overlay left right) = Overlay (sing left) (sing right)

sing other = other


ascend = Overlay (Overlay (Note "S0") (Note "11")) (Note "12")

test_sing1 = (sing (Overlay ascend (Note "13")))
              == Overlay (Overlay (Note "11") (Note "13"))
                         (Overlay (Note "12") (Note "13"))
                         
test_sing2 = sing (sing (Overlay (Overlay (Note "11") (Note "1"))
                                 (Overlay (Note "12") (Note "1"))))
              == Note "1"

test_sing3 = (sing ascend) == ascend

test_sing4 = sing (Overlay (Note "S?") (Overlay ascend (Note "10")))
              == Overlay
                   (Note "S?")
                   (Overlay (Overlay (Note "11") (Note "10"))
                            (Overlay (Note "12") (Note "10")))
test_sing = test_sing1 && test_sing2 && test_sing3 && test_sing4

{-
  Q2b. repeat_sing: See a2.pdf for instructions.
-}

repeat_sing :: Song -> Song
repeat_sing song =
  let simplifiedSong = sing song
  in if simplifiedSong == song 
     then song
     else
       repeat_sing simplifiedSong

test_repeat1 = repeat_sing (Overlay ascend (Note "Z")) == Note "Z"



{-
  Q2c.  BONUS: See a2.tex.
  This question might not have an answer, and is not worth very many marks,
  so don't attempt it unless you really want to.
  
  Your solution must not be infinite, that is, it must not be self-referential
  or recursive.
-}
diverging_song :: Song
diverging_song = undefined
