module Main where

import Network (getGrandmastersFromRegion)

import Domain ( numberOfTerrans
              , numberOfProtoss
              , numberOfZerg
              , getHighestMMRPlayer
              , getPlayerWithHighestWinRate )

main :: IO ()
main = runApp

-- just testing here for now
runApp :: IO ()
runApp = do
  -- na <- getGrandmastersFromRegion "NA"
  eu <- getGrandmastersFromRegion "EU"
  -- kr <- getGrandmastersFromRegion "KR"

  let terranUsers = numberOfTerrans eu
  let protossUsers = numberOfProtoss eu
  let zergUsers = numberOfZerg eu

  let xs = ["Terran: " <> show terranUsers, "Protoss: " <> show protossUsers, "Zerg: " <> show zergUsers]

  let bigboi = getHighestMMRPlayer eu

  mapM_ putStrLn xs
  print $ getPlayerWithHighestWinRate eu
