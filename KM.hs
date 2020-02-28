module KM where

-- find distance from mean
--l2 :: Vectorized -> Vectorized -> Float
l2' = ((sqrt . sum . map (**2)) . ) . zipWith (-) 

l2 = const
