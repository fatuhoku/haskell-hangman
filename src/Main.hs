module Main where

import System.Random

import Control.Monad.Trans
import Control.Monad.State.Lazy
import Data.List
import Data.List.HT

type Hangman a = StateT GameState IO a

-- TODO lookup the definition of the chosen word from an online dictionary,
--      display it at the end of the game, thereby justifying the way that he
--      program tends to call the user a nasty name.
--      LINK: http://services.aonaware.com/DictService/DictService.asmx
--      This looks like this will involve a bit of SOAP. What fun.
--
-- TODO lookup nasty names from the internet to call the user if they lose.

-- Holds a list of true and false values for which the player had already
-- guessed those letters correctly.
--
-- TODO do a total variation of Hangman, which is based on messing around with
--      webservices: find a random word from the database, search and grab an
--      image from Yahoo! images; get the URL and put it into an ASCII-art
--      webservice to convert it into ASCII characters. Present the output to
--      the user to make them recognise what is in the picture.
data GameResult = Won | Lost | NotWon

data GameState = GameState {
  theWord  :: String,          -- should be in Reader
  guesses  :: [Char],          -- which characters have been guessed
  lives    :: Int,
  maxLives :: Int              -- the number of lives at the beginning.
}

instance Show GameState where
  show (GameState {theWord=word,guesses=chrs,lives=ls,maxLives=mls}) =
    let mysteryWord = map (replaceWith chrs '_') word;
        used  = "Guessed: {" ++ chrs ++ "}";
        lives = "Lives:   [" ++ replicate ls 'I' ++ "]"
        wordToUsed  = 15 - length mysteryWord
        usedToLives = 14 - length chrs
        spaces = "      "
    in spaces ++ mysteryWord ++ replicate wordToUsed ' ' ++
        used ++ replicate usedToLives ' ' ++ lives ++ "\n\n" ++
        livesIllustrations !! (mls-ls)
    where
      replaceWith cs char c
        | c `elem` cs = c
        | otherwise   = char

-- We have to replace every letter of the word that appears in chr with _!

-- The player is given 10 lives.

-- This is the loop that runs with state!
-- We can simply output the number of lives left, as a simple way around drawing
-- the whole stickman figure.
--
-- 1) Repeatedly show prompt for 1 character only.
-- 2) If this is okay, add it into the list of guesses.
-- 3) If there were matches (elem) then do not decrement lives.
-- 4) Check whether the player has won.
--
runHangman :: Hangman ()
runHangman = do

  -- Read state and show the game status
  gs@GameState {theWord=word,guesses=chrs,lives=ls} <- get
  liftIO $ print gs

  -- Validate user input for one character only
  -- Once validated, this constitutes a guess.
  inChar <- liftIO $ msum $ repeat retrieveChar
  liftIO $ putStr "\n\n"

  -- Reduce lives if guess was not in the word
  let ls' = if inChar `elem` word && not (inChar `elem` chrs)
              then ls
              else ls - 1

  -- Put state
  let gs' = gs {guesses=chrs `union` [inChar],lives=ls'}
  put gs'

  case gameResult gs' of
    Won  -> liftIO $ do
      print gs'
      putStr "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"
      putStr "Congratulations, you've won!! You're so smart!\n"
      putStr $ "The word was " ++ show word ++ " -- how did you guess?!?\n"
    Lost -> liftIO $ do
      print gs'
      putStr "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"
      putStr "POOR YOU! You've LOST! You're such a dumbass!\n"
      putStr $ "The word was " ++ show word ++ ".\n"
    _    -> runHangman             -- neither won or lost. Continue.

  where
    retrieveChar = do
      inLine <- getLine
      if length inLine == 1
        then return $ head inLine
        else do
          putStr "Please enter a single character only. Try again.\n"
          mzero
    gameResult :: GameState -> GameResult
    gameResult gs@GameState {theWord=word,guesses=chrs,lives=ls} =
      if all (`elem` chrs) word
        then Won
        else if ls < 1 then Lost else NotWon

-- At the beginning of the game, I pick a word randomly from a list of words.
-- I have a game
main :: IO ()
main = do
  wrds <- getWords "res/words.txt"
  (ranIdx,_) <- newStdGen >>= return . randomR (0,length wrds)
  let chosenWord = wrds !! ranIdx
  putStr "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"
  putStr "!                 Hok's Hangman                   !\n"
  putStr "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"
  putStr "Welcome to the gallows..."
  putStr "You'd better get the word right, or else Mr. Stick gets it."
  putStr "\n\n"
  _ <- execStateT runHangman $ defaultGameState chosenWord
  return ()
  where
    getWords filePath  = readFile filePath >>= return . concatMap words . lines
    defaultLives       = 10
    defaultGameState w = GameState w [] defaultLives defaultLives

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

