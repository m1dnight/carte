-- Author:  Christophe De Troyer
-- Email:   christophe.detroyer@gmail.com
-- License: GPLv3
-- Date:    November 10, 2016

module Main where
import           Base36
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Trans.State
import           Data.Time.Clock.POSIX
import           Data.Tuple.Select
import           Network
import           Options.Applicative
import           System.FilePath.Posix
import           System.IO
import           System.Random
import           Data.Semigroup ((<>))

 --      ___           ___           ___                         ___
 --     /  /\         /  /\         /  /\          ___          /  /\
 --    /  /::\       /  /::\       /  /::\        /__/\        /  /::\
 --   /  /:/\:\     /  /:/\:\     /  /:/\:\       \  \:\      /  /:/\:\
 --  /  /:/  \:\   /  /::\ \:\   /  /::\ \:\       \__\:\    /  /::\ \:\
 -- /__/:/ \  \:\ /__/:/\:\_\:\ /__/:/\:\_\:\      /  /::\  /__/:/\:\ \:\
 -- \  \:\  \__\/ \__\/  \:\/:/ \__\/~|::\/:/     /  /:/\:\ \  \:\ \:\_\/
 --  \  \:\            \__\::/     |  |:|::/     /  /:/__\/  \  \:\ \:\
 --   \  \:\           /  /:/      |  |:|\/     /__/:/        \  \:\_\/
 --    \  \:\         /__/:/       |__|:|~      \__\/          \  \:\
 --     \__\/         \__\/         \__\|                       \__\/

--------------------------------------------------------------------------------
--- ARGUMENT PARSING -----------------------------------------------------------
--------------------------------------------------------------------------------

type URL = String

-- | `Configuration` is the data type that holds all the runtime
-- information. It is populated by arguments entered by the user.
data Configuration =
  Configuration
    { port  :: Integer
    , url   :: URL
    , files :: String
    } deriving (Show)

portParser :: Parser Integer
portParser =
    option auto        -- an option which reads its argument with Read
      ( short 'p'      -- a short name i.e. "-p"
     <> long "port"    -- a long name i.e. "--port"
     <> metavar "PORT" -- a symbolic name in the help text
     <> value 5001     -- a default value
     <> help "The port on which to listen for connections" )

urlParser :: Parser String
urlParser =
    strOption
      ( short 'u'
     <> long "url"
     <> metavar "URL"
     <> value "localhost"
     <> help "The root url for building links to files" )

filesParser :: Parser String
filesParser =
    strOption
      ( short 'd'
     <> long "dir"
     <> metavar "DIRECTORY"
     <> value "."
     <> help "The DIRECTORY in which to store pastes" )

configuration :: Parser Configuration
configuration =
    Configuration
      <$> portParser
      <*> urlParser
      <*> filesParser

--------------------------------------------------------------------------------
--- MAIN -----------------------------------------------------------------------
--------------------------------------------------------------------------------

main :: IO ()
main = do cfg <- execParser prog
          socketloop cfg
  where
    prog = info
      (helper <*> configuration)
      ( fullDesc
     <> progDesc "Start a Carte paste server"
     <> header "Carte - a socket based paste server" )

--------------------------------------------------------------------------------
--- SOCKET LOOP ----------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Starts the `loop` function which keeps on listening for client
-- connections. The current time in seconds is used as the starting
-- seed for the random generator.
socketloop :: Configuration -> IO ()
socketloop cfg = withSocketsDo $ do
                   seed <- fmap mkStdGen currentTimeSeconds
                   s    <- listenOn (PortNumber . fromInteger . port $ cfg)
                   runReaderT (evalStateT (loop s) seed) cfg

-- | Fires a new thread for each client connection, handles the rest
-- of the upload.
loop :: Socket -> StateT StdGen (ReaderT Configuration IO) ()
loop s = liftIO (accept s) >>= handleUploader . sel1  >> loop s

--------------------------------------------------------------------------------
--- UPLOADER HANDLING ----------------------------------------------------------
--------------------------------------------------------------------------------

-- | Takes in a `Handle` (socket), reads the input from the socket,
-- writes it to disk, notifies the user and closes the socket.
handleUploader :: Handle -> StateT StdGen (ReaderT Configuration IO) ()
handleUploader source = do
  key     <- newKey
  baseUrl <- asks url
  fileDir <- asks files
  void . liftIO $ do
    receiveFile source (fileDir </> key)
    notify source (baseUrl </> key)
    hClose source

-- | Writes data from first `Handle` to second `Handle` until EOF
-- is encountered. The `Handle`s are not closed.
pipe :: Handle -> Handle -> IO ()
pipe src sink = do
  l   <- hGetLine src
  hPutStrLn sink l
  end <- hIsEOF src
  unless end $ pipe src sink

-- | Takes in a `Handle` and writes all the data to the given file at
-- FilePath. Uses `WriteMode` so overwrites any contents.
receiveFile :: Handle -> FilePath -> IO ()
receiveFile src filepath = withBinaryFile filepath WriteMode (pipe src)

-- | Prints the web URL on the socket (to the user).
notify :: Handle -> URL -> IO ()
notify = hPutStrLn

-- | Returns the current time in seconds. Used as a seed for the random generator.
currentTimeSeconds :: IO Int
currentTimeSeconds = round <$> getPOSIXTime

-- | Generates a new random ID for the snippet.
newKey :: (Monad m) => StateT StdGen m String
newKey = do
  (key, gen') <- gets random
  put gen'
  return $ base36 key
