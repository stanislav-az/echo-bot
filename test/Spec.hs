import TelegramTests
import SlackTests

main :: IO ()
main = do 
    putStrLn "Running Telegram tests..."
    runTelegramTests
    putStrLn "Telegram tests done!"
    putStrLn "Running Slack tests..."
    runSlackTests
    putStrLn "Slack tests done!"
    