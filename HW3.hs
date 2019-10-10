module HW3 where


-- | Convert a regular list into a run-length list.
--
--   >>> compress [1,1,1,2,3,3,3,1,2,2,2,2]
--   [(3,1),(1,2),(3,3),(1,1),(4,2)]
-- 
--   >>> compress "Mississippi"
--   [(1,'M'),(1,'i'),(2,'s'),(1,'i'),(2,'s'),(1,'i'),(2,'p'),(1,'i')]
--
compress :: Eq a => [a] -> [(Int,a)]
-- compress xs = compressor (reverse xs) []
--     where
--         compressor :: Eq a => [a] -> [(Int, a)] -> [(Int, a)]
--         compressor [] ts = ts
--         compressor (x:xs) [] = compressor xs [(1,x)]
--         compressor (x:xs) ((ti, tx):ts) = if tx == x 
--             then compressor xs ((1 + ti, tx):ts) 
--             else compressor xs ((1,x) : (ti,tx) : ts)
compress = map entry . chunk
    where
        entry :: [a] -> (Int, a)
        entry ys = (length ys, head ys)

        chunk :: Eq a => [a] -> [[a]]
        chunk [] = []
        chunk (x:xs) = let (match, rest) = all x xs in match : chunk rest

        all :: Eq a => a -> [a] -> ([a], [a])
        all x [] = ([x], [])
        all x (y:ys) = if x == y then let (match, rest) = all x ys in (y : match, rest) else ([x], y:ys)


-- | Convert a run-length list back into a regular list.
--
--   >>> decompress [(5,'a'),(3,'b'),(4,'c'),(1,'a'),(2,'b')]
--   "aaaaabbbccccabb"
--  
decompress :: [(Int,a)] -> [a]
decompress [] = []
decompress ((x,y):rest) = (replicate x y) ++ decompress rest