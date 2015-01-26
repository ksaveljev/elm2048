module Utils where

-- Note that this is a little sloppy — we’d like to be able to throw
-- errors for situations when for example an index is out of bounds,
-- but this is beyond the scope of this tutorial. Fortunately, this
-- implementation will satisfy the needs of our project.
infixl 9 !
(!) : [a] -> Int -> a -- the nth element of a list
l ! n = case (l, n) of
          (l, 0) -> head l
          (x::xs, n) -> xs ! (n - 1)

-- Again, this implementation ignores a lot of potentially error prone
-- situations, but it will suffice for our needs.
transpose : [[a]] -> [[a]] -- transposes a list of lists
transpose ll = case ll of
                 ((x::xs)::xss) -> (x :: (map head xss)) :: transpose (xs :: (map tail xss))
                 otherwise -> []
