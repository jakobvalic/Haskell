import Dict

-- a phone book maps names to numbers. But phone numbers can be messy, so we
-- use [String] to represent them.
type Phonebook = Dict String String

-- we make some basic operations on phone books available to the user
data Operation = Add | Search | Remove | PrintAll

-- a command tells us to either perform an operation on a phone book or to stop
data Command = Op Operation | UnknownCmd String | Quit

-- [readName] asks the user to input a name and reads it from the standard input
readName :: IO String
readName = do
  putStr "Enter name: "
  getLine

-- [readNumber] asks the user to input a phone number and reads it from the standard input
readNumber :: IO String
readNumber = do
  putStr "Enter number: "
  getLine

-- [readCommand] asks the user to input one of the available commads and reads it
readCommand :: IO Command
readCommand = do
  putStr "Choose command (add,search,remove,print,quit) : "
-- getLine >>= \ cmd ->
  cmd <- getLine
  case cmd of
    "quit" -> return Quit
    "add" -> return (Op Add)
    "search" -> return (Op Search)
    "remove" -> return (Op Remove)
    "print" -> return (Op PrintAll)
    _ -> return (UnknownCmd cmd)


-- [runOperation pb op] runs the operation [op] on the phone book [pb]
runOperation :: Phonebook -> Operation -> IO Phonebook
runOperation pb Add = do
    -- read a name, read a number, return the pb w/ whose added
    name <- readName
    number <- readNumber
    return $ add name number pb
runOperation pb Search = do
    name <- readName
    (case search name pb of
       Nothing -> putStrLn $ "name " ++ name ++ " not found"
       Just nb -> putStrLn $ "Found number: " ++ nb) :: IO ()
     retrun nb --ker moramo vrniti IO Phonebook  

-- [interactionLoop pb] reads a [Command] and performs the corresponding
-- action: an operation is executed and an unknown command is signalled to the
-- user by writing a warning. Until a [Quit] is encountered, at which point the
-- function returns.
interactionLoop :: Phonebook -> IO ()
interactionLoop pb = do
    -- read a command
    cmd <- readCommand
    -- depending on what command we got, either
    case cmd of
      -- * execute an operation and restart the loop 
      Op operation -> do
        new_phonebook <- runOperation pb operation
        interactionLoop new_phonebook
      -- * return a unit value  
      Quit -> return ()
      UnknownCmd some_cmd -> do
        -- * signal a warning
        putStrLn $ "The " ++ some_cmd ++ "is unknown command."
        -- * and carry on
        interactionLoop pb
       
   -- * execute an operation and restart the loop 
   -- * signal a warning
   -- * return a unit name

main :: IO ()
main = interactionLoop empty
