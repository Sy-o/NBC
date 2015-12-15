import NBC;
import CSVParse;

getData sourceData s = sCSV (getMaybeData sourceData s ::Maybe [([Double],String)])
   where sCSV (Nothing) = error "Wrong CSV File Format"
         sCSV (Just a) = a


main = do
  -- let x =  Map.fromList [("a",[(1,0),(8,0)]),
  --                       ("b",[(2,0),(3,0)]),
  --                       ("c",[(4,0),(5,0)]),
  --                       ("d",[(6,0),(7,0)])]
  --   -- in print $ addObjectToClass "c" [2,4] x
  print $ (/5) 8
  --sourceData <- readFile "source1.txt" `catch` handlerForFileRoutine "source1.txt"
  --let 
  --  x = getData sourceData
