import Prelude (
    (==), (<), (>), (<=), (>=),
    (+), (-), (*), div, negate,
    (.), Int, Bool, Eq, Ord,
    show)

-- Host Language
eq = \x -> \y -> if x == y then true else false
lt, gt :: Ord a => a -> a -> (b -> b -> b)
lt = \x -> \y -> if x < y then true else false
gt = \x -> \y -> if x > y then true else false

add, mul :: Int -> Int -> Int
add = (+)
mul = (*)
min = negate

-- Basic
id = \x -> x
sub = \x -> \y -> add x (min y)
inc = \x -> add x 1
dec = \x -> sub x 1

-- Church Numerals
zero = \x -> x
one = \x -> succ (zero x)
succ = \n -> inc n
pred = \n -> dec n
isZero = \f -> \x -> eq x (f x)

evi = \f -> f (0 :: Int)

-- Boolean Logic
true = \x -> \y -> x
false = \x -> \y -> y
if' = id
then' = id
else' = id

and = \f -> \g -> if' f g false
or = \f -> \g -> if' f true g
not = \f -> if' f false true
nand = \f -> \g -> not (and f g)
nor = \f -> \g -> not (or f g)
xor = \f -> \g -> if' f (not g) g

evb = \f -> f 1 0

-- Lists, tuples
empty = id
cons = \x -> \xs -> \n ->
  if' (eq n 0) x (xs (dec n))
len = \xs ->
  sub (min 1) (xs (min 1))

mapCons = \k -> \v -> \xs -> \i ->
  if' (eq i k) v (xs i)

foldr = \f -> \acc -> \xs -> foldr' f acc xs 0
  where
    foldr' = \f -> \acc -> \xs -> \n ->
      if' (eq n (len xs))
        acc (f (xs n)
        (foldr' f acc xs (inc n)))

map = \f -> \xs ->
  foldr (\x -> \acc -> cons (f x) acc) empty xs

sum = foldr add 0
product = foldr mul 1

take = \n -> \xs ->
  if' (eq n (len xs))
    (empty)
    (cons (xs n) (take (dec n) xs))

drop = \n -> \xs ->
  if' (eq n (len xs))
    (empty)
    (cons (xs n) (drop (dec n) xs))

head = take 1
tail = drop 1
init = \xs -> take (dec (len xs)) xs
last = \xs -> drop (dec (len xs)) xs
