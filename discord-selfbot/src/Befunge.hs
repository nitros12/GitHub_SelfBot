module Befunge () where

data BefungeCmd = Add
                | Sub
                | Mul
                | Div
                | Mod
                | Not
                | Geq
                | Pcr
                | Pcl
                | Pcu
                | Pcd
                | Hif
                | Vif
                | Tsm
                | Dup
                | Swp
                | Pop
                | PrInt
                | PrChr
                | Halt
                | Brd
                | Get
                | Put
                | Num' Int
                | None deriving (Show)

type BefungeArray = [[BefungeCmd]]

type Vector = (Int, Int)

data BFState = BFState { array      :: [[BefungeCmd]]
                       , arrayPos   :: Vector
                       , spDir      :: Vector
                       , stack      :: [Int]
                       , stringMode :: Bool
                       , output     :: String
                       }
type BFUpdate = BFState -> BFState

addvec :: Vector -> Vector -> Vector
addvec (xa, ya) (xb, yb) = (xa+xb, ya+yb)

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
pcr state = state {spDir = (-1, 0)}

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
prInt state@BFState{output = o, stack = x:_} = state {output = show x:o}

prChr :: BFUpdate
prChr state@BFState{output = o, stack = x:_} = state {output = (toEnum x :: Char) : o}

get :: BFUpdate
get state@BFState{stack = y:x:xs, array=arr} = state {stack = getPosition arr (x, y):xs }

put :: BFUpdate
put state@BFState{stack = y:x:xs, array=arr} = state {stack = xs, array = setPosition arr (x, y)}

pushsk :: Int -> BFUpdate
pushsk a state@BFState{stack=xs} = state {stack = a:xs}

newState :: BefungeArray -> BFState
newState array = BFState {array = array
                         ,arraypos = (0, 0)
                         ,spDir = (1, 0)
                         ,stack = []::[Int]
                         ,stringMode = False
                         ,output = ""}

getOP :: Char -> BFCmd
getOP chr
  | chr `elem` ['0'..'9'] = Num' (read chr :: Int)
  | otherwise = case chr of
    '+'       -> Add
    '-'       -> Sub
    '*'       -> Mul
    '/'       -> Div
    '%'       -> Mod
    '!'       -> Not
    '`'       -> Geq
    '>'       -> Pcr
    '<'       -> Pcl
    '^'       -> Pcu
    'v'       -> Pcd
    '_'       -> Hif
    '|'       -> Vif
    '"'       -> Tsm
    ':'       -> Dup
    '\\'      -> Swp
    '$'       -> Pop
    '.'       -> PrInt
    ','       -> PrChr
    '#'       -> Brd
    'g'       -> Get
    'p'       -> Put
    '@'       -> Halt
    otherwise -> None

parseBF :: [String] -> BefungeArray
parseBF = map (map getOp)

runBF :: BFUpdate
runBF state = case getPosition (array state) (arraypos state) of
  Halt -> state
  c -> runBF . advance $ case c of
    Add    -> add state
    Sub    -> sub state
    Mul    -> mul state
    Div    -> div' state
    Mod    -> mod' state
    Not    -> not' state
    Geq    -> geq state
    Pcr    -> pcr state
    Pcl    -> pcl state
    Pcu    -> pcu state
    Pcd    -> pcd state
    Hif    -> hif state
    Vif    -> vif state
    Tsm    -> tsm state
    Dup    -> dup state
    Swp    -> swp state
    Pop    -> pop state
    PrInt  -> prInt state
    PrChr  -> prChr state
    Get    -> get state
    Put    -> put state
    Brd    -> advance state
    Num' a -> pushstk a state
    None   -> state

processBF :: [String] -> String
processBF inp = reverse $ output (runBF . newState . parseBF $ inp)
