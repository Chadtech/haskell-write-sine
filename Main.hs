module Main where

import Data.ByteString (writeFile, concat)

import Data.List (map, foldr)
import Data.ByteString.Char8 (pack)
import Data.Int (Int16)
import Data.Binary (encode)
import Flow
import Prelude hiding (map, foldr, writeFile, concat)
import Data.ByteString.Lazy (toStrict)


main :: IO ()
main = do
  print' "What should we name this file?"
  fileName <- getUserInput
  print' "What frequency should the sine wave have?"
  frequency <- getUserInput
  print' "What duration should the audio have?"
  duration <- getUserInput

  let frequency' = (read frequency) :: Float
  let duration'  = (read duration) :: Integer
  let audioData  = sineWave frequency' duration'

  logAudio fileName audioData
  writeAudio fileName audioData

print' :: String -> IO ()
print' =
  putStrLn

getUserInput :: IO String
getUserInput =
  getLine


-- Make Audio Data


sineWave :: Float -> Integer -> [ Int16 ]
sineWave freq duration =
  [ 0 .. (duration - 1) ]
  |>map (getAmplitude freq)

getAmplitude :: Float -> Integer -> Int16
getAmplitude freq index =
  let 
    freq' = freq / 44100
    i     = Prelude.fromInteger index
  in
  sin (i * 2 * pi * freq')
  |>toInt16

toInt16 :: Float -> Int16
toInt16 f =
  fromIntegral (fromEnum (f * 32767))


-- Write it to disk


writeAudio :: String -> [ Int16 ] -> IO ()
writeAudio fn audioData =
  let fileName = fn ++ ".audio" in
  audioData
  |>map (toStrict . encode)
  |>concat
  |>writeFile fileName


-- Write it to disk 
-- in a human readable way


logAudio :: String -> [ Int16 ] -> IO ()
logAudio fn audioData =
  let fileName = fn ++ ".log" in
  audioData
  |>map lineate
  |>foldr (++) ""
  |>pack
  |>writeFile fileName

lineate :: Int16 -> String
lineate i =
  (show i) ++ "\n"


