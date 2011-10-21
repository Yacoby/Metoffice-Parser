module Print where

printTable :: [String] -> [[String]] -> IO ()
printTable headings table = do
    width <- return $ map (+2) $ widths headings table
    printRow width headings
    putStr "\n"
    putStrLn (take (sum $ width) $ repeat '-')
    mapM (printRow width) table
    return ()

    where
        printRow width row = do 
            mapM (putStr . (uncurry padCenter)) $ zip width row
            putStr "\n"
    

padCenterList width list = map (uncurry padCenter) (zip width list)

padCenter :: Int -> String -> String
padCenter num str = (take padL $ repeat ' ') ++ str ++ (take padR $ repeat ' ')
    where
        enum = if even num then num else num - 1
        padL = (enum -(length str)) `div` 2
        padR =  if even num then padL else padL + 1

widths :: [String] -> [[String]] -> [Int]
widths headings table = foldr columnWidths headingWidths table
    where
        headingWidths = map length headings

        columnWidths :: [String] -> [Int] -> [Int]
        columnWidths str cur = map ( \(x,y) -> max x (length y) ) $ zip cur str

