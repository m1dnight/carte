module Main where
import           Base36
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Char
import           Data.List
import           Data.Time.Clock.POSIX
import           Network
import           System.Directory
import           System.Environment
import           System.FilePath.Posix
import           System.IO
import           System.Random
import           Text.Printf

data Data = D { rnd :: StdGen, opts :: Opts }

main :: IO ()
main = do t    <- currentTimeSeconds
          opts <- liftM buildOpts getArgs
          putStrLn $ printf "Config: %s" (show opts)
          withSocketsDo $
            do s <- listenOn (PortNumber $ fromInteger . p . port $ opts)
               evalStateT (loop s) (D (mkStdGen t) opts)
          return ()

----------------------
-- Argument Parsing --
----------------------

data Opts = Opts { port :: Arguments, host :: Arguments, files :: Arguments }
            deriving (Show, Eq)

data Arguments
 = Port     { p :: Integer }
 | HostName { h :: String  }
 | FileDir  { dir :: String  }
 deriving (Show, Eq)

parseArg as arg constr process  deflt =
  let args = take 2 . dropWhile (/= arg) $ as
  in
    case args of
      [arg, opts] -> constr (process opts)
      _           -> constr deflt

parsePort as    = parseArg as "-p" Port read 5001
parseHost as    = parseArg as "-h" HostName id "localhost"
parseFileDir as = parseArg as "-d" FileDir id "."

buildOpts args = Opts { port  = parsePort args,
                        host  = parseHost args,
                        files = parseFileDir args }

buildFileUrl url id = if last url == '/'
                         then url ++ id
                         else url ++ "/" ++ id

---------------------
-- Socket handling --
---------------------

loop :: Socket -> StateT Data IO ()
loop s =
  do (h,_,_) <- lift $ accept s
     id      <- randId
     conf    <- liftM opts get
     lift . forkIO $ process h id conf
     loop s

process :: Handle -> String -> Opts -> IO ()
process hdl id opts = do d <- eatData hdl []
                         let filepath = combine (dir . files $ opts) id
                         saveToDisk filepath d
                         hPutStrLn hdl $ buildFileUrl (h . host $ opts) id
                         hFlush hdl
                         hClose hdl


eatData :: Handle -> [String] -> IO [String]
eatData handle ls =
  do l <- hGetLine handle
     end <- hIsEOF handle
     if end
       then return $ l:ls
       else eatData handle $ l:ls

saveToDisk :: FilePath -> [String] -> IO ()
saveToDisk fp ls = do path <- canonicalizePath fp
                      putStrLn $ printf "Writing file to %s" path
                      writeFile path (intercalate "\n" ls)

--------------------------
-- Random ID generation --
--------------------------

randId :: StateT Data IO String
randId = do d <- get
            let (str, gen') = random (rnd d)
            put (d { rnd = gen' })
            return $ base36 str

currentTimeSeconds :: IO Int
currentTimeSeconds = round `fmap` getPOSIXTime
