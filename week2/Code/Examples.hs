Induction Example 1
-------------------

We want to prove that the sum of the first `n` odd numbers
is the same as `n^2`.

More formally: for any natural number `n`,
the function
```
f 0 = 0
f n = (2*n - 1) + f (n-1)
```
satisfies the equation `f n = n^2`.


We want to prove that the result holds for any natural `n`.
We argue by induction on `n`.
There are two cases.

Case 1: `n` is zero.
Goal: `f 0 = 0^2`.
f 0 = 0                -- first eqn for f
    = 0^2.

Case 2: `n` has the form `m+1` for another natural `m`,
and we know by inductive hypothesis that
`f m = m^2`.
Goal: `f (m+1) = (m+1)^2`

f (m+1) =                        -- by eqn 1 for f
2 * (m+1) - 1 + f ((m+1) - 1) =  -- arithmetic
2 * (m+1) - 1 + f m           =  -- arithmetic
2 * m + 2 - 1 + f m           =  -- arithmetic
2 * m + 1 + f m               =  -- ind hypothesis
2 * m + 1 + m^2               =  -- arithmetic
m^2 + 2*m + 1                 =  -- high school algebra
(m+1)^2



Induction Example 2
-------------------
The input list of a `map` function always has the same length as 
the output list.

We want to prove that the `map` function does not
change the length of its input list.

More formally: we must prove that for
  any input list `xs :: [t]` and
  any function `f :: t -> s`,
the equality
  `length (map f xs) == length xs`
holds.

First, remind ourselves of the definitions involved:
{-
length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs
-}

We want to prove that the result holds for any list `xs`.
We argue by induction on the structure of `xs`.
There are two cases.

Case 1: `xs` has the form [].
Goal: `length (map f []) = length []`.

length (map f []) =   -- by eqn 1 of map
length [].

Case 2: `xs` has the form `(y:ys)` for some `y :: t` and `ys :: [t]`,
where we know by inductive hypothesis that
`length (map f ys) = length ys`.
Goal: `length (map f (y:ys)) = length (y:ys)`.

length (map f (y:ys))   =    -- by eqn 2 of map
length (f y : map f ys) =    -- by eqn 2 of length
1 + length (map f ys)   =    -- ind hypothesis
1 + length ys           =    -- by eqn 2 of length backwards
length (y:ys).



