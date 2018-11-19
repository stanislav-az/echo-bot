import WebIO

main :: IO ()
main = do
    putStrLn "Running tests..."
    --assert (isPalindrome "racecar") "passed 'racecar'" "FAIL: 'racecar'"
    putStrLn "done!"

assert :: Bool -> String -> String -> IO ()
assert test passStatement failStatement = if test
                                          then putStrLn passStatement
                                          else putStrLn failStatement

