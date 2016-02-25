

------------------------- Exercise 1
{-
approxR :: Integer -> (Float -> Float) -> Float
approxR n f | n <= 0    = approxR (n - 1) f
            | otherwise = f (n)
-}
f0 :: Float -> Float
f0 x = 1 + sqrt x

f1 :: Float -> Float
f1 x = 2 / (x + 1)

f2 :: Float -> Float
f2 x = (2 / 3) + (x / 4) + ((x * x) / 10)
{-
approximations :: (Float -> Float) -> [Float]
approximations f =  
                 where
                  f x | x = x + 1
                      | otherwise = f x
  -}                 

------------------------- Fixed-point combinator

fix :: (a -> a) -> a
fix f = f (fix f)

{-
     fix f
  == f (fix f)
  == f (f (fix f))
  == f (f (f (fix f)))
  ...
-}


------------------------- Exercise 2

as :: [Integer]
as = 0 : as

f3 :: [Integer] -> [Integer]
f3 xs = 0 : xs

{-
     fix f3
  == f3 (fix f3)      
  == 0 : fix f3
  == f3 (f3 (0:fix f3))
  == 0 : 0 : fix f3
  == f3 ( f3 (f3 (0 : 0 : fix f3)))
  == 0 : 0 : 0 : fix f3
-}

addOne :: [Integer] -> [Integer]
addOne x =  

f4 :: [Integer] -> [Integer]
f4 ys = 0 : addOne ys

{-
     fix f4
  == f4 (fix f4)
  == 0 : addOne (fix f4)
  ...
-}

bs :: [Integer]
bs = undefined

approxL :: Integer -> ([a] -> [a]) -> [a]
approxL n f = undefined

f5 :: [Integer] -> [Integer]
f5 xs = 1:2:3:xs


------------------------- Exercise 3

f6 :: (Integer -> Integer) -> (Integer -> Integer)
f6 g n = undefined

{-
     (fix f6) 3
  == f6 (fix f6) 3
  == 3 * (fix f6) 2
  ...
-}


------------------------- Exercise 4

diverge :: a
diverge = diverge

approx :: Integer -> (a -> a) -> a
approx n f = undefined


------------------------- State

type Variable = String

type State = Variable -> Integer

empty :: State
empty _ = 0

set :: Variable -> Integer -> State -> State
set x n s = t  where  t y | x == y    = n
                          | otherwise = s y

------------------------- Arithmetic expressions

data Aexp = Num Integer
          | Var Variable
          | Aexp :+: Aexp
          | Aexp :-: Aexp
          | Aexp :*: Aexp

evalA :: Aexp -> State -> Integer
evalA (Num n)   _ = n
evalA (Var v)   s = s v
evalA (a :+: b) s = evalA a s + evalA b s
evalA (a :*: b) s = evalA a s * evalA b s
evalA (a :-: b) s = evalA a s - evalA b s


------------------------- Boolean expressions

data Bexp = Boolean Bool
          | Aexp :==: Aexp
          | Aexp :<=: Aexp
          | Neg Bexp
          | Bexp :&: Bexp
          | Bexp :|: Bexp

evalB :: Bexp -> State -> Bool
evalB (Boolean b) _ = b
evalB (a :==: b)  s = evalA a s == evalA b s
evalB (a :<=: b)  s = evalA a s <= evalA b s
evalB (Neg b)     s = not (evalB b s)
evalB (a :&: b)   s = evalB a s && evalB b s
evalB (a :|: b)   s = evalB a s || evalB b s


------------------------- Commands

data Comm = Skip
          | Variable :=: Aexp
          | Comm :>: Comm
          | If Bexp Comm Comm
          | While Bexp Comm
          | Print Aexp

evalC :: Comm -> State -> State
evalC Skip        s = s
evalC (v :=: a)   s = set v (evalA a s) s
evalC (c :>: d)   s = evalC d (evalC c s)
evalC (If b c d)  s | evalB b s = evalC c s
                    | otherwise = evalC d s
evalC (While b c) s = approx 10 (wstep b c) s


------------------------- Exercise 5

wstep :: Bexp -> Comm -> (State -> State) -> (State -> State)
wstep b c f s = undefined


------------------------- Programs

-- Factorial

factorial :: Comm
factorial = ("y" :=: Num 1) :>: 
            While (Num 1 :<=: Var "x") (
              ("y" :=: (Var "x" :*: Var "y")) :>:
              ("x" :=: (Var "x" :-: Num 1))
            )

runFactorial :: Integer -> Integer
runFactorial n = t "y"
  where 
    s = set "x" n empty
    t = evalC factorial s


-- Euclid's algorithm

euclid :: Comm
euclid = While (Neg (Var "x" :==: Var "y")) (
           If (Var "x" :<=: Var "y")
              ("y" :=: (Var "y" :-: Var "x"))
              ("x" :=: (Var "x" :-: Var "y"))
         )

runEuclid :: Integer -> Integer -> Integer
runEuclid n m = t "x"
  where
    s = set "x" n (set "y" m empty)
    t = evalC euclid s