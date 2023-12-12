import Data.Char
import Data.List
import Data.Maybe

cut p list =
  let (hd, tl) = break p list
  in (hd, snd (fromJust (uncons tl)))

split p list =
  let (hd, tl) = break p list
  in case tl of
    [] -> [hd]
    _:tl -> hd:split p tl

parseInput lines =
  let (time_line, tail) = fromJust (uncons lines)
      time_str = drop (length "Time:") time_line
      (distance_line, _tail) = fromJust (uncons tail)
      distance_str = drop (length "Distance:") distance_line
  in (time_str, distance_str)

numberOfWays (time, distance) =
  -- `Float` won't work!
  let time_f :: Double = fromIntegral time
      distance_f :: Double = fromIntegral distance
      delta = time_f ** 2 - 4 * distance_f
      alpha = (time_f - sqrt delta) / 2
      beta = (time_f + sqrt delta) / 2
      max_holding_time =
        if fromIntegral (floor beta) == beta then
          floor beta - 1
        else
          floor beta
      min_holding_time =
        if fromIntegral (ceiling alpha) == alpha then
          ceiling alpha + 1
        else
          ceiling alpha
  in max_holding_time - min_holding_time + 1

main = do
  input <- getContents
  let (time_str, distance_str) = parseInput (lines input)
  let times = map read (filter (/= "") (split (== ' ') time_str))
  let distances = map read (filter (/= "") (split (== ' ') distance_str))
  let result_part1 = product (map numberOfWays (zip times distances))
  putStrLn ("Part 1: " ++ show result_part1)
  let time = read (filter (/= ' ') time_str)
  let distance = read (filter (/= ' ') distance_str)
  let result_part2 = numberOfWays (time, distance)
  putStrLn ("Part 2: " ++ show result_part2)
