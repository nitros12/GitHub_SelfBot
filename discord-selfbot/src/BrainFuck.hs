module BrainFuck
    ( processBF
    ) where

data Tape a = Tape [a] a [a] deriving (Show)

data BFCmd = Movr
           | Movl
           | Add
           | Sub
           | Put
           | Push
           | Ret
           | Null
           | Halt deriving (Show, Eq)

data BFState = BFState { instructions :: Tape BFCmd
                        , tape        :: Tape Int
                        , output      :: String
                        } deriving (Show)

type BFUpdate = BFState -> BFState


goBack :: Tape BFCmd -> Int -> Tape BFCmd
goBack (Tape (l:rl) m r) c
  | m == Ret = goBack (Tape rl l (m:r)) (succ c)
  | m == Push = case c of
    1 -> Tape rl l (m:r)
    _ -> goBack (Tape rl l (m:r)) (pred c)
  | otherwise = goBack (Tape rl l (m:r)) c
goBack (Tape [] m r) _ = Tape [Halt] m r

-- jump forward, move forward until counter is 0, if [-Push increment counter, if ]-Ret decrement

goForward :: Tape BFCmd -> Int -> Tape BFCmd
goForward (Tape l m (r:rm)) c
  | m == Push = goForward (Tape (m:l) r rm) (succ c)
  | m == Ret = case c of
    1 -> Tape l m (r:rm)
    _ -> goForward (Tape (m:l) r rm) (pred c)
  | otherwise = goForward (Tape (m:l) r rm) c
goForward (Tape l m []) _ = Tape l m [Halt]


getOp :: Char -> BFCmd
getOp chr = case chr of
  '>' -> Movr
  '<' -> Movl
  '+' -> Add
  '-' -> Sub
  '.' -> Put
  '[' -> Push
  ']' -> Ret
  _   -> Null

parseBF :: String -> [BFCmd]
parseBF = filter (Null /=) . map getOp

movr :: BFUpdate
movr state@BFState{tape=Tape (h:lr) m r} = state {tape = Tape lr h (m:r)}
movr state@BFState{tape=Tape [] m r}     = state {tape = Tape [0] 0 (m:r)}

movl :: BFUpdate
movl state@BFState{tape=Tape l m (h:lr)} = state {tape = Tape (m:l) h lr}
movl state@BFState{tape=Tape l m []}     = state {tape = Tape (m:l) 0 [0]}

add :: BFUpdate
add state@BFState{tape=Tape l m r} = state {tape = Tape l (succ m) r}

sub :: BFUpdate
sub state@BFState{tape=Tape l m r} = state {tape = Tape l (pred m) r}

put :: BFUpdate
put state@BFState{tape=Tape _ m _, output = o} = state {output = (toEnum m :: Char) : o }

push :: BFUpdate
push state@BFState{tape = Tape _ 0 _, instructions = t} = state {instructions = goForward t 0}
push state = state

ret :: BFUpdate
ret state@BFState{instructions = i} = state {instructions = goBack i 0}

newState :: [BFCmd] -> BFState
newState (x:xs) = BFState {instructions = Tape [Halt] x (xs++[Halt])
                   ,tape = newTape 0
                   ,output = "" }
newState _ = BFState {instructions = Tape [Halt] Halt [Halt]
                   ,tape = newTape 0
                   ,output = "" }

newTape :: a -> Tape a
newTape a = Tape (repeat a) a (repeat a)

advance :: BFUpdate
advance state@BFState{instructions=Tape l m (x:xs)} = state {instructions = Tape (m:l) x xs}
advance state@BFState{instructions=Tape l m []} = state {instructions = Tape (m:l) Halt [Halt]}

runBF :: BFUpdate
runBF state@BFState{instructions=Tape _ Halt _} = state
runBF state@BFState{instructions=(Tape _ cur _), output = o} = runBF . advance $ case cur of
  Movr -> movr state
  Movl -> movl state
  Add  -> add state
  Sub  -> sub state
  Put  -> put state
  Push -> push state
  Ret  -> ret state
  _    -> state

processBF :: String -> String
processBF inp = reverse $ output (runBF . newState . parseBF $ inp)
