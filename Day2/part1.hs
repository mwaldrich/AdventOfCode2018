import Data.List

main :: IO ()
main = do
  fileContents <- readFile "input.txt"
  let ids = lines fileContents
  let checker = (\appearances -> (length (filter (dup appearances) ids)))
  putStrLn (show ((checker 2) * (checker 3)))

-- Does this id contain any letters that are repeated `repeats' times?
dup :: Int -> String -> Bool
dup repeats id =
  (not . null)
  (filter
    (\uniqueLetter -> repeats== (length (filter (uniqueLetter==) id)))
    (nub id))
