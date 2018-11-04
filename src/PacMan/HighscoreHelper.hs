{-# LANGUAGE OverloadedStrings #-}

module PacMan.HighscoreHelper where

import Data.Text (Text)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Aeson
import Data.List

data Score = Score {
  playerName :: Text,
  playerScore :: Int
}

instance FromJSON Score where
  parseJSON = withObject "Score" $ \v -> Score <$> v .: "name" <*> v .: "score"

instance ToJSON Score where
  toJSON (Score name score) = object ["name" .= name, "score" .= score]
  toEncoding (Score name score) = pairs ("name" .= name <> "score" .= score)

addScore :: Score -> [Score] -> [Score]
addScore score highscores = sortBy sort' (score : highscores)
  where
    sort' :: Score -> Score -> Ordering
    sort' (Score _ scoreA) (Score _ scoreB) = compare scoreB scoreA

readHighscores :: IO [Score]
readHighscores = do
 jsonData <- readFile "data/highscores.json"
 case decode (pack jsonData) of
   (Just highscores') -> return highscores'
   _                  -> error "could not highscores"

writeHighscore :: [Score] -> IO ()
writeHighscore = writeFile "data/highscores.json" . unpack . encode
