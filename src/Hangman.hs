{-# LANGUAGE NamedFieldPuns #-}
module Hangman
    ( runHangman
    ) where

import qualified Control.Monad.State.Lazy as MTL
import qualified System.Random as R

import Data.List
import Data.List.HT

type Hangman a = MTL.StateT GameState IO a

data GameState = GameState
  { theWord  :: String     -- TODO should be in Reader monad
  , guesses  :: [Char]     -- which characters have been guessed
  , lives    :: Int
  , maxLives :: Int        -- the number of lives at the beginning.
  }

instance Show GameState where
  show GameState {theWord,guesses,lives,maxLives} =
    let wordIndicator = map (replaceWith guesses '_') theWord;
        usedIndicator  = "Guessed: {" ++ guesses ++ "}";
        livesIndicator = "Lives:   [" ++ replicate lives 'I' ++ "]"
        spacesFromWordToUsed  = 15 - length wordIndicator
        spacesFromUsedToLives = 14 - length guesses
        spaces = "      "
    in concat [spaces, wordIndicator,
               replicate spacesFromWordToUsed ' ',
               usedIndicator, replicate spacesFromUsedToLives ' ',
               livesIndicator, "\n\n",
               livesIllustrations !! (maxLives-lives)
         ]
    where
      replaceWith cs char c
        | c `elem` cs = c
        | otherwise   = char

data GameResult = Won | Lost | NotWon

-- Game defaults
defaultLives       = 10
startingGameState word = GameState { theWord = word
                                   , guesses = []
                                   , lives = defaultLives
                                   , maxLives = defaultLives }

                                   --
-- This is the loop that runs with state!
-- 1) Repeatedly show prompt for 1 character only.
-- 2) If this is okay, add it into the list of guesses.
-- 3) If there were matches (elem) then do not decrement lives.
-- 4) Check whether the player has won.
--
mainLoop :: Hangman ()
mainLoop = do

  -- Read state and show the game status
  gs@GameState {theWord,guesses,lives} <- MTL.get
  MTL.liftIO $ print gs

  -- Keep asking the user for a single character.
  inputChar <- MTL.liftIO $ MTL.msum $ repeat retrieveChar
  MTL.liftIO $ putStr "\n\n"

  -- Decrement lives only if guess was not in the word
  let updateLives = if inputChar `elem` theWord
                    && notElem inputChar guesses then id else pred

  -- Put state
  let gs' = gs {guesses=guesses `union` [inputChar],lives=updateLives lives}
  MTL.put gs'

  case gameResult gs' of
    Won  -> MTL.liftIO $ do
      print gs'
      putStr $ wonMessage $ show theWord
    Lost -> MTL.liftIO $ do
      print gs'
      putStr $ lostMessage $ show theWord
    _    -> mainLoop             -- neither won or lost. Continue.

  where
    -- Asks user for one character only.
    retrieveChar = do
      inLine <- getLine
      if length inLine == 1
        then return $ head inLine
        else do
          putStrLn "Please enter a single character only. Try again."
          MTL.mzero -- failure state

    gameResult :: GameState -> GameResult
    gameResult gs@GameState {theWord,guesses,lives}
     | all (`elem` guesses) theWord = Won
     | lives < 1 = Lost
     | otherwise = NotWon

runHangman :: IO ()
runHangman = do
  allWords <- getWords "res/words.txt" -- TODO Add more words
  (randomIndex,_) <- R.randomR (0,length allWords) <$> R.newStdGen 
  let chosenWord = allWords !! randomIndex
  putStrLn introMessage
  putStr "\n\n"
  _ <- MTL.execStateT mainLoop $ startingGameState chosenWord
  return ()
  where
    getWords filePath = concatMap words . lines <$> readFile filePath

introMessage = unlines [
  "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!",
  "!                 Hok's Hangman                   !",
  "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!",
  "Welcome to the gallows...",
  "You'd better get the word right, or else Mr. Stick gets it."
  ]

wonMessage theWord = unlines [
  "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!",
  "Congratulations, YOU GOT IT!! You're so smart!",
  "The word was " ++ theWord ++ " -- HOW DID YOU KNOW?!?"
  ]

lostMessage :: [Char] -> String
lostMessage theWord = unlines [
  "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!",
  "Oh dear, YOU LOST!! Mr. Stick couldn't be saved :(",
  "The word was " ++ theWord ++ ". Better luck next time ;)"
  ]

livesIllustrations = [lives10,lives9,lives8,
                      lives7,lives6,lives5,
                      lives4,lives3,lives2,
                      lives1,theEnd]

lives10 = "             \n" ++
          "             \n" ++
          "             \n" ++
          "             \n" ++
          "             \n" ++
          "             \n" ++
          "             \n"

lives9 =  "             \n" ++
          "             \n" ++
          "             \n" ++
          "             \n" ++
          "             \n" ++
          "             \n" ++
          "-------------\n"

lives8  = "             \n" ++
          " |           \n" ++
          " |           \n" ++
          " |           \n" ++
          " |           \n" ++
          " |           \n" ++
          "-------------\n"

lives7  = "-----------  \n" ++
          " |           \n" ++
          " |           \n" ++
          " |           \n" ++
          " |           \n" ++
          " |           \n" ++
          "-------------\n"

lives6  = "-----------  \n" ++
          " |     |     \n" ++
          " |           \n" ++
          " |           \n" ++
          " |           \n" ++
          " |           \n" ++
          "-------------\n"

lives5  = "-----------  \n" ++
          " |     |     \n" ++
          " |     O     \n" ++
          " |           \n" ++
          " |           \n" ++
          " |           \n" ++
          "-------------\n"

lives4  = "-----------  \n" ++
          " |     |     \n" ++
          " |     O     \n" ++
          " |     |     \n" ++
          " |           \n" ++
          " |           \n" ++
          "-------------\n"

lives3  = "-----------  \n" ++
          " |     |     \n" ++
          " |     O     \n" ++
          " |    /|     \n" ++
          " |           \n" ++
          " |           \n" ++
          "-------------\n"

lives2  = "-----------  \n" ++
          " |     |     \n" ++
          " |     o     \n" ++
          " |    /|\\    \n" ++
          " |           \n" ++
          " |           \n" ++
          "-------------\n"

lives1  = "-----------  \n" ++
          " |     |     \n" ++
          " |     o     \n" ++
          " |    /|\\    \n" ++
          " |      \\    \n" ++
          " |           \n" ++
          "-------------\n"

theEnd  = "-----------  \n" ++
          " |     |     \n" ++
          " |     O     \n" ++
          " |    /|\\    \n" ++
          " |    / \\    \n" ++
          " |           \n" ++
          "-------------\n"