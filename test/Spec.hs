import TelegramTests
--import Test.Hspec -- HPFFP p. 529
--import Test.QuickCheck

main :: IO ()
main = do 
    putStrLn "Running telegram tests..."
    runTelegramTests
    putStrLn "Telegram tests done!"