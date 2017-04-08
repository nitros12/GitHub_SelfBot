module Befunge () where

data BefungeCmd = Add
                | Sub
                | Mul
                | IDiv
                | Mod
                | Not
                | Geq
                | PCR
                | PCL
                | PCU
                | PCD
                | RPC
                | Hif
                | Vif
                | TSM
                | Dup
                | Swp
                | Pop
                | PrInt
                | PrChr
                | Brd
                | Get
                | Put
                | Halt
                | Num Int deriving (Show)

type BefungeArray = [[BefungeCmd]]

type Vector = (Int, Int)

data BFState = BFState { array    :: [[BefungeCmd]]
                       , arrayPos :: Vector
                       , spDir    :: Vector
                       , stack    :: [Int]
                       }
type BFUpdate = BFState -> BFState

getLeft :: Int -> [BefungeCmd] -> [BefungeCmd]
getLeft s arr = case splitAt s arr of
  (a, _) -> a
  (_, _) -> []

getMiddle :: Int -> [BefungeCmd] -> BefungeCmd
getMiddle s arr = case maybeSplitAt s arr of
  (_, a:_) -> a
  (_, [])  -> Halt -- Just get Halt if nothing


getRight :: Int -> [BefungeCmd] -> [BefungeCmd]
getRight s arr = case maybeSplitAt s arr of
   (_, _:a) -> a
   (_, [])  -> Halt

setPosition :: BefungeArray -> (Int, Int) -> Char -> BefungeArray -- (x, y)
setPosition b (x, y) s = getLeft y b ++ [(getLeft x $ getMiddle y b) ++ [s] ++ (getRight x $ getMiddle y b)] ++ getRight y b

getPosition :: BefungeArray -> (Int, Int) -> Char
getPosition b (x, y) = b !! y !! x


opstack :: (Int -> Int -> Int) -> BFUpdate
opstack op state@BFState{stack=x:y:xs} = state {stack = op y x: xs}

add :: BFUpdate
add = opstack (+)

sub :: BFUpdate
sub = opstack (-)

mul :: BFUpdate
mul = opstack (*)

div' :: BFUpdate
div' = opstack div

mod' :: BFUpdate
mod' = opstack mod

not' :: BFUpdate
not' state@BFState{stack=x:xs} = state {stack = (if x == 0 then 1 else 0):xs}

geq :: BFUpdate
geq state@BFState{stack=x:y:xs} = state {stack = (if y > x then 1 else 0):xs}
