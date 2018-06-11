module Main where

main :: IO ()
main = do
  let initialList = []
  interactWithUser initialList

type Item = String
type Items = [Item]

data Command = Quit
             | DisplayItems
             | AddItem String
             | RemoveItem Int
             | Help

type Error = String

parseCommand :: String -> Either Error Command
parseCommand line = case words line of
  ["quit"] -> Right Quit
  ["items"] -> Right DisplayItems
  "add" : item -> Right $ AddItem $ unwords item
  "remove" : i -> Right $ RemoveItem $ read $ unwords i
  ["help"] -> Right Help
  _ -> Left "Unknown command"

addItem :: Item -> Items -> Items
addItem = (:)

displayItems :: Items -> String
displayItems = unlines . map ("- " ++)

removeItem :: Int -> Items -> Either Error Items
removeItem i items
  | i < 0 || i >= length items = Left "Out of range"
  | otherwise = Right result
  where (front, back) = splitAt (i + 1) items
        result = init front ++ back

interactWithUser :: Items -> IO ()
interactWithUser items = do
  line <- getLine
  case parseCommand line of
    Right DisplayItems -> do
      putStrLn $ displayItems items
      interactWithUser items

    Right (AddItem item) -> do
      let newItems = addItem item items
      putStrLn "Added"
      interactWithUser newItems

    Right (RemoveItem i) ->
      case removeItem i items of
        Right newItems -> do
          putStrLn $ "Removed " ++ items !! i
          interactWithUser newItems
        Left err -> do
          putStrLn err
          interactWithUser items


    Right Quit -> return ()

    Right Help -> do
      putStrLn "Commands:"
      putStrLn "help"
      putStrLn "items"
      putStrLn "add"
      putStrLn "quit"
      interactWithUser items

    Left err -> do
      putStrLn $ "Error: " ++ err
      interactWithUser items
