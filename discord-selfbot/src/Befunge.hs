module Befunge (runBefunge) where

import           Data.Char (digitToInt)
import           Data.List
import           Data.Ord

type BefungeArray = [[Char]]

type Vector = (Int, Int)

data BFState = BFState { array      :: [[Char]]
                       , arrayPos   :: Vector
                       , spDir      :: Vector
                       , stack      :: [Int]
                       , stringMode :: Bool
                       , output     :: String
                       }
type BFUpdate = BFState -> BFState

addvec :: Vector -> Vector -> Vector
addvec (xa, ya) (xb, yb) = (xa+xb, ya+yb)

getLeft :: Int -> [a] -> [a]
getLeft s arr = a
  where (a, _) = splitAt s arr

getMiddle :: Int -> [a] -> a -> a
getMiddle s arr default' = case splitAt s arr of
  (_, a:_) -> a
  (_, [])  -> default' -- Just get Halt if nothing


getRight :: Int -> [a] -> [a]
getRight s arr = case splitAt s arr of
   (_, _:a) -> a
   (_, [])  -> []

setPosition :: [[Char]] -> (Int, Int) -> Char -> [[Char]] -- (x, y)
setPosition b (x, y) s = getLeft y b ++ (getLeft x (getMiddle y b ([])) ++ s : getRight x (getMiddle y b ([]))) : getRight y b

getPosition :: [[Char]] -> (Int, Int) -> Char
getPosition b (x, y) = b !! y !! x

advance :: BFUpdate
advance state@BFState{arrayPos=pos, spDir=dir} = state {arrayPos=addvec pos dir}

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

pcr :: BFUpdate
pcr state = state {spDir = (1, 0)}

pcl :: BFUpdate
pcl state = state {spDir = (-1, 0)}

pcu :: BFUpdate
pcu state = state {spDir = (0, 1)}

pcd :: BFUpdate
pcd state = state {spDir = (0, -1)}

hif :: BFUpdate
hif state@BFState{stack = x:xs} = state {stack = xs, spDir = if x == 0 then (1, 0) else (-1, 0)}

vif :: BFUpdate
vif state@BFState{stack = x:xs} = state {stack = xs, spDir = if x == 0 then (0, -1) else (0, 1)}

tsm :: BFUpdate
tsm state@BFState{stringMode = m} = state {stringMode = not m}

dup :: BFUpdate
dup state@BFState{stack = x:xs} = state {stack = x:x:xs}

swp :: BFUpdate
swp state@BFState{stack = x:y:xs} = state {stack = y:x:xs}

pop :: BFUpdate
pop state@BFState{stack = x:xs} = state {stack = xs}

prInt :: BFUpdate
prInt state@BFState{output = o, stack = x:xs} = state {output = show x ++ o, stack = xs}

prChr :: BFUpdate
prChr state@BFState{output = o, stack = x:xs} = state {output = (toEnum x :: Char) : o, stack = xs}

get :: BFUpdate
get state@BFState{stack = y:x:xs, array=arr} = state {stack = (fromEnum $ getPosition arr (x, y)):xs}

put :: BFUpdate
put state@BFState{stack = y:x:xs, array=arr} = state {stack = xs, array = setPosition arr (x, y) $ getPosition arr (x, y)}

pushstk :: Int -> BFUpdate
pushstk a state@BFState{stack=xs} = state {stack = a:xs}

maxLength :: [String] -> Int
maxLength = length . maximumBy (comparing length)

rfill :: Int -> a -> [a] -> [a]
rfill l c str = str ++ replicate (max 0 (l - length str)) c

fixarr :: [String] -> [String]
fixarr str = map (rfill l ' ') str
  where l = maxLength str + 1

newState :: BefungeArray -> BFState
newState array = BFState {array = fixarr array
                         ,arrayPos = (0, 0)
                         ,spDir = (1, 0)
                         ,stack = repeat 0::[Int]
                         ,stringMode = False
                         ,output = ""}





runInstance :: Char -> BFUpdate
runInstance c state = case c of
    '+'  -> add state
    '-'  -> sub state
    '*'  -> mul state
    '/'  -> div' state
    '%'  -> mod' state
    '!'  -> not' state
    '`'  -> geq state
    '>'  -> pcr state
    '<'  -> pcl state
    '^'  -> pcu state
    'v'  -> pcd state
    '_'  -> hif state
    '|'  -> vif state
    ':'  -> dup state
    '\\' -> swp state
    '$'  -> pop state
    '.'  -> prInt state
    ','  -> prChr state
    'g'  -> get state
    'p'  -> put state
    '#'  -> advance state
    _    -> state


runBF :: BFUpdate
runBF state
  | s == '"' = runBF . advance . tsm $ state
  | stringMode state = runBF . advance $ pushstk  (fromEnum s) state
  | s == '@' = state
  | s `elem` ['0'..'9'] = runBF (advance (pushstk (digitToInt s) state))
  | otherwise = runBF . advance $ runInstance s state
  where s = getPosition (array state) (arrayPos state)

runBefunge :: String -> String
runBefunge inp = reverse $ output (runBF . newState . lines $ inp)
