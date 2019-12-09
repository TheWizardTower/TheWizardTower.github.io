import           Control.Concurrent (threadDelay)
import           Control.Retry      (retryPolicyDefault, retrying)
import           Data.Maybe         (isNothing)
import           System.Random      (randomRIO)
import           System.Timeout     (timeout)

myLongFunc :: IO Int
myLongFunc = do
  -- roll 1d10
  num <- randomRIO (1, 10) :: IO Integer
  case num of
    10        -> threadDelay (2  * 10 ^ 6)    -- sometimes the DB answers really fast!
    otherwise -> threadDelay (10 * 10 ^ 6)  -- ...but mostly not so much.
  return 100

myActualAction :: Int -> IO ()
myActualAction = print -- log it, or something.

myComposedAction :: IO ()
myComposedAction = print "Trying!" >> myLongFunc >>= myActualAction

main :: IO ()
main = do
    _ <- retrying
      retryPolicyDefault
      (const $ return . isNothing)
      (\_ -> timeout (5 * 10 ^ 6) myComposedAction)
    return ()
