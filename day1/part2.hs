extractInstruction :: String -> Int
extractInstruction (direction : amount) =
    if direction == 'R'
        then read amount
        else (-1 * read amount)

zeroPasses :: Char -> Int -> Int -> Int -> (Int,Int) 
zeroPasses _ currentPosition 0 zerosEncountered =
    case currentPosition of 
        0 -> (currentPosition, zerosEncountered+1)
        _ -> (currentPosition, zerosEncountered)
zeroPasses direction 0 clicksToGo zerosEncountered = 
    case direction of 
        'R' -> zeroPasses 'R' 1 (clicksToGo-1) (zerosEncountered+1)
        'L' -> zeroPasses 'L' 99 (clicksToGo-1) (zerosEncountered+1)
zeroPasses direction currentPosition clicksToGo zerosEncountered =
    case direction of
        'R' -> zeroPasses 'R' ((currentPosition+1) `mod` 100) (clicksToGo-1) zerosEncountered
        'L' -> zeroPasses 'L' ((currentPosition-1)`mod` 100) (clicksToGo-1) zerosEncountered

calcPassword :: [String] -> Int -> Int -> Int
calcPassword [] _ count = count
calcPassword ( x : xs ) currentPosition count =
    case newPosition of
        0 -> calcPassword xs newPosition (newCount-1)
        _ -> calcPassword xs newPosition newCount
    where
        (newPosition, newCount) =
            let instruction = extractInstruction x in
                if instruction > 0
                then zeroPasses 'R' currentPosition instruction count
                else zeroPasses 'L' currentPosition ((-1)*instruction) count

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let instructions = words contents
    print (calcPassword instructions 50 0)