module Main (main) where

import Control.Concurrent
import Control.Monad
import Data.Time
import Control.Concurrent.Async
import Network.HTTP
    ( getRequest, getResponseBody, simpleHTTP, Response )
import qualified Network.HTTP.Stream as Network.Stream
-- To use threadDelay for simulating network latency
import Control.Concurrent (threadDelay) 
import Control.Monad (void)

n :: Int
n = 100

performNetwork :: IO (Network.Stream.Result (Response String))
performNetwork =  simpleHTTP (getRequest "http://hackage.haskell.org/")

printMeasure :: (Traversable t, Show a) => IO (t (IO a)) -> IO ()
printMeasure cb = do
  start <- getCurrentTime
  writes <- cb
  k <- sequence writes
  mapM_ print k
  stop <- getCurrentTime 
  print $ diffUTCTime stop start

seqNet :: IO ()
seqNet = 
  printMeasure $ do 
      x <- replicateM n performNetwork  
      let writes = fmap (take 15) . getResponseBody <$> x
      return writes

perThread:: IO [Char]
perThread = do
    x <- performNetwork  
    (fmap (take 15) . getResponseBody) x

concNet :: IO ()
concNet =
  -- make N concurrent network requests
  printMeasure $ do
        spawned <- replicateM n $ async perThread -- async :: IO a -> IO Async a ;; when we bind, we get back an Async (a handle to a concurrent result)
        let writes = fmap wait spawned  -- waiting on a handle provides another IO action which we can bind to get the result
        return writes 

-- some notes
-- https://bitbashing.io/haskell-concurrency.html
-- concurrency is about not wasting time waiting (for IO) and maximizing hardware utilization (many cores)
-- in Haskell concurrency is closely related to the IO monad.
-- user-space concurrency is nearly always cooperative (pre-emptive requires hardware managed by OS)
  -- and for parallelism, we leverage OS threads
-- for user-space concurrency we can do either stackless or stackful coroutines
  -- stackful is like re-building OS threads in user-space but we can have tiny stacks
      -- Golang does fine-grained cooperation by yielding at any IO/channel op or even at function call
      -- forkIO in Haskell spawns a stackful coroutine 
  -- stackless is like running code, then saving the exact state in the heap and where to continue;;  notice, we require the stop points to be known beforehand for an efficient implementation
    -- a good mental model is a closure - save the state and the code is the rest of the program -- Python, Rust (Future state machine), c++. interesting ideas around lifetimes... (i.e. Pin)
    -- the cooperation is at the same "yield" points, after which we have to run a new closure.
  -- results (types)
    -- we need to know when the concurrent task is done - if we are completely separate then we just need the result (Future) or a callback to be called else we need primitives like channels, 
    -- just stick with a Future as the mechanism of getting a concurrent result. a future can be "wait'd", so we poll until we get the result.
  -- syntax
    -- we use the async type to label a function as returning a Future, then we can await on the Future to get the result
    -- Future is a MONAD
    -- Haskell chooses Async to mean an actively running function, so it's not a monad. We use the IO monad to compose actions, then we use Async to create a promise. Haskell only uses 'wait' not 'await'
    -- a stackless coroutine needs to await async things in order to yield to other coroutines. this is where the many closures come in.
        -- so say we are awaiting a network request or a timer; we have there a primitive function that can be awaited and returns when we get an interrupt.
-- methods of scheduling user-space concurrency:
  -- to utilize parallelism, we multiplex coroutines on many OS threads (M coroutines runs on 1 OS thread at a time, with typically N OS threads when a system has N virtual cores)


-- the fact that IO is a monad and async works inside IO means we can compose Futures. 
-- (Async T) is Future<T>


-- await == bind
-- async == return

-- async function sequentialComposition() {
--     const user = await findUser(1);
--     const posts = await getPosts(user);
--     const comments = await getComments(posts);
--     const endTime = Date.now();
-- }

-- 1. Functions return IO T (The Future Monad)
findUser :: Integer -> IO String
findUser = const $ do
  threadDelay 1000000 -- Simulate 1 second delay
  print "findUser"
  return "Sid"

getPosts :: String -> IO String
getPosts = const $ do
  threadDelay 1000000 -- Simulate 1 second delay
  print "getPosts"
  return "Post1"

getComments :: String -> IO String
getComments = const $ do
  threadDelay 1000000 -- Simulate 1 second delay
  print "getComments"
  return "Comment1"

backgroundWork :: Integer -> IO String
backgroundWork n = do 
  if n == 0 then
    return "done"
  else do
    print n
    threadDelay 10000
    backgroundWork (n+1)

-- 2. The Monadic Composition
haskellAsyncMonad :: IO ()
haskellAsyncMonad = do
  let bgWork = backgroundWork 1
      fgWork = do 
        user <- findUser 1
        postsAsync <- async (getPosts user)
        posts <- wait postsAsync
        commentsAsync <- async (getComments posts)
        wait commentsAsync
  res <- race bgWork fgWork
  print $ either id id res

main :: IO ()
main = do
  seqNet
  concNet
  haskellAsyncMonad