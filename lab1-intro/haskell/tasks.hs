-- The following function definition contains a syntax error:
-- (To enable the code, delete contents of lines 10 and 3.)
{-
sayHello :: IO ()
sayHello =
  do
  let name = "Alice"
   putStrLn "hello"
  putStrLn name
-}
-- Answer:
sayHello :: IO ()
sayHello =
    do
    let name = "Alice"
    putStrLn "hello" -- the error was a space on this line
    putStrLn name

main :: IO()
main = 
    do
    let add a b = a + b
    let myfun = add 5
    print (myfun 10)
-- What does myfun 10 return?
-- It returns 15

ask :: String -> IO ()
ask prompt =
  do
  putStrLn prompt
  line <- getLine
  if line == ""
    then ask prompt
    else putStrLn ("you said: " ++ reverse line)

main :: IO ()
main =
  do
  -- sayHello
  ask "please say something"

-- put both of these in the compiler
-- cii.)
-- Answer:
ask :: String -> IO ()
ask prompt =
  do
  putStrLn prompt
  line <- getLine
  if line /= "quit"
    then ask prompt
    else putStrLn ("quitting...")

main :: IO ()
main =
  do
  ask "please say quit"

-- ciii.)
-- Answer:
ask :: String -> IO ()
ask prompt =
  do
  putStrLn prompt
  line <- getLine
  if line == ""
    then ask prompt
    else putStrLn ("quitting...")

main :: IO ()
main =
  do
  ask "!"
  -- find a way to add += "!" onto it each time