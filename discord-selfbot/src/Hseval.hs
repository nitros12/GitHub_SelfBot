import           Language.Haskell.Interpreter
import           System.Environment
import           System.Timeout

main :: IO ()
main = do
  [block] <- getArgs
  Just (Right s) <- timeout 2500000 . runInterpreter $ do
    setImports ["Prelude"]
    eval block
  let p = if length s >= 1900 then take 1893 s ++ "..." else s
  putStr p
