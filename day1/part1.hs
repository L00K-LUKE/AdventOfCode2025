extractInstruction :: String -> Int
extractInstruction (direction : amount) =
    if direction == 'R'
        then read amount
        else (-1 * read amount)

calcPassword :: [String] -> Int -> Int -> Int
calcPassword [] _ count = count
calcPassword ( x : xs ) currentPosition count =
    let 
        newLocation = (currentPosition + extractInstruction x) `mod` 100
    in
        case newLocation of
            0 -> calcPassword xs newLocation (count+1)
            _ -> calcPassword xs newLocation count


main :: IO ()
main = do
    contents <- readFile "input.txt"
    let instructions = words contents
    print (calcPassword instructions 50 0)
    

