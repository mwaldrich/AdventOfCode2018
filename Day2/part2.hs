main :: IO ()
main = do
  fileContents <- readFile "input.txt"
  let ids = lines fileContents
  let matchedPair = (head (filter
                            (\pair -> (1==) (diff (fst pair) (snd pair)))
                            (pairs ids)))
  putStrLn (common (fst matchedPair) (snd matchedPair))

-- The common characters in a string.
common :: String -> String -> String
common [] [] = []
common (a:as) (b:bs) =
  if (a == b)
  then [a] ++ (common as bs)
  else (common as bs)
  
-- How many characters differ between the two strings?
-- The two strings should be the same size.
diff :: String -> String -> Int
diff a b = (length a) - (length (common a b))

-- All tuples composed of the given elements.
pairs :: [a] -> [(a, a)]
pairs a = (foldr (++) [] (map (makePair a) a))

-- All tuples composed of the given x and elements.
makePair :: [a] -> a -> [(a, a)]
makePair elems x = (zip elems (replicate (length elems) x))

