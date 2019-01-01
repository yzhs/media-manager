import Control.Monad
import Data.List
import System.Directory
import System.Environment
import System.Exit

data Entry a b c d = Entry {
    typ :: a,
    title :: b,
    ident :: c,
    comment :: d
  } deriving (Eq, Ord, Show)

-- | Reads a file describing which media items you have already watched or
-- listened to.
readOldList listFile = readFile listFile >>= return . map parseLine . lines

-- | Read a line describing a single item you already watched or listened to.
parseLine line = Entry cat name num (unwords desc)
  where (cat:name:num:desc) = wordsTabs line

-- | Split a string into chunks at tabs.   Like words, just ignoring other
-- whitespace characters.
wordsTabs :: String -> [String]
wordsTabs str = helper str "" []
  where
    helper "" "" lst = reverse lst
    helper "" tmp lst = reverse (reverse tmp:lst)
    helper ('\t':xs) "" lst = helper xs "" lst
    helper ('\t':xs) tmp lst = helper xs "" (reverse tmp:lst)
    helper (x:xs) tmp lst = helper xs (x:tmp) lst

-- | Search the database of old items for one matching a given item exactly.
search db lst = filter (\(Entry c n n' d) -> isPrefixOf lst [c,n,n',d]) db

-- | Add a new entry to the in-memory database.
addEntry db cat name num desc = insert (Entry cat name num desc) db

toString :: Entry String String String String -> String
toString (Entry a b c d) = unwords [a, b, c, d]

-- | Get list of files 
listFiles :: [String] -> IO [FilePath]
listFiles lst = getHomeDirectory >>= \home -> helper (dir lst home) (cond lst)
  where helper dirs cond = do
          files <- (fmap concat $ mapM getDirectoryContents dirs)
          return $ filter (\f -> (not $ isPrefixOf "." f) && cond f) files
        dir [cat] home = [home ++ "/" ++ cat ++ "s/**"] -- TODO get the files from all subdirectories
        dir (cat:name:_) home = [home ++ "/" ++ cat ++ "s/" ++ name]
        cond (_:_:num:_) = isInfixOf num
        cond _ = const True

-- | Filter for files that are not listed in the database of old
-- videos/podcasts.
findNew :: Eq c => [Entry a b [c] d] -> [[c]] -> [[c]]
findNew db = filter (\f -> all (\(Entry _ _ n _) -> not $ isInfixOf n f) db)

-- | Filter for files that are in the database of old videos/podcasts.
findOld :: Eq c => [Entry a b [c] d] -> [[c]] -> [[c]]
findOld db = filter (\f -> any (\(Entry _ _ n _) -> isInfixOf n f) db)

main = do
  progName <- getProgName
  args     <- getArgs >>= parse progName
  home     <- getHomeDirectory
  db       <- readOldList (home ++ "/done.txt")
  files    <- listFiles args
  mapM_ putStrLn $ sort $ (if progName == "new" then findNew else findOld) (search db args) files


-- | Parse the command line arguments.
parse :: String -> [String] -> IO [String]
parse str []     = usage   str >> exit
parse str ["-h"] = usage   str >> exit
parse str ["-v"] = version str >> exit
parse str [cat]  = return [cat]
parse str [cat, name] = return [cat, name]
parse str (cat:name:num:desc) = return (cat:name:num:(if null desc then [] else [unwords desc]))

usage str   = putStrLn ("Usage: " ++ str ++ " [-vh] category [name [number [description]]]")
version str = putStrLn (str ++ " 0.1")

exit = exitWith ExitSuccess
die  = exitWith (ExitFailure 1)
