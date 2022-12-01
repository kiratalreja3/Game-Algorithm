{-|
Module      : AI
Description : AIs for Othello
Copyright   : (c) 2020 Your Name Here
License     : AllRightsReserved
-}
module AI where

import Othello
import Data.Tuple()
import Data.List(sortOn)

-- | Type of AI functions you can choose to write.
data AIFunc
  = NoLookahead (GameState -> Move)
    -- ^ Simple AIs that do not need lookahead.
  | WithLookahead (GameState -> Int -> Move)
    -- ^ AIs that want to look ahead. The assignment framework will
    -- call the function over and over with increasing integer
    -- arguments @1, 2, 3, ...@ until your AI's time limit is up.

-- | The table of all AIs that your assignment provides. The AI named
-- "default" in this table is the one your tutor will dedicate most of
-- his or her attention to marking.
ais :: [(String, AIFunc)]
ais = [ ("firstLegalMove", NoLookahead firstLegalMove),
        ("greedyAI", NoLookahead greedyAI),
        ("default", WithLookahead minimaxAI),
        ("minimaxABprune", WithLookahead minimaxABprune)
      ]

--Greedy Heuristic for NoLookAhead "greedyAI" (Successful attempt)

-- | 'runningScore' computes the difference in 
-- our score & our opponent's score on the board
runningScore :: Maybe GameState -> Int
runningScore st = case st of
  (Just (GameState _ (Turn p) brd)) ->
     currentScore brd p - currentScore brd (otherPlayer p)
  _ -> 0

-- | 'moveScoreTuple' generates a tuple out of 
-- a move & the resulting running score from it
moveScoreTuple :: GameState -> Move -> (Move,Int)
moveScoreTuple st move = (move, runningScore (applyMove st move))

-- | 'moveScoreTuples' maps 'moveScoreTuple' 
-- over a list of moves
moveScoreTuples :: GameState -> [Move] -> [(Move,Int)]
moveScoreTuples st listmoves = map (moveScoreTuple st) listmoves

-- | 'bestMove' sorts a list of 
-- (Move,Int) tuples by the second value (Int)
-- & returns the sorted list of just the first value (Move)
bestMove :: [(Move, Int)] -> [Move]
bestMove tuples = map fst (sortOn snd tuples)

-- | 'greedyAIHeuristic' returns Move with the 
-- highest running score from a list of legal Move values
greedyAIHeuristic :: [Move] -> GameState -> Move
greedyAIHeuristic moves st = case moves of
  [singleMove] -> singleMove
  _ -> last (bestMove (moveScoreTuples st moves))

-- MiniMax with AB Pruning "minimaxABPrune" (Non-successful attempt)

-- | 'minimaxAB' is a non-successful attempt at MiniMax with 
-- AB Pruning. This code returns a non-optimal move. 
minimaxAB :: [Rose (Move,Int)] -> MinMaxPlayer -> Int -> Int -> (Move,Int)
minimaxAB roseTreeList player alpha beta = case player of
  Max -> chooseMoveAB (map (optimalValueAB Min) roseTreeList)  Max
  Min -> chooseMoveAB (map (optimalValueAB Max) roseTreeList) Min
  where
  -- | 'optimalValueAB' chooses an optimal (Move,Int) depending 
  -- on if the player is Min or Max. If there are no children of 
  -- the RoseTree, it returns the node value
  optimalValueAB :: MinMaxPlayer -> Rose (Move,Int) -> (Move,Int)
  optimalValueAB currentPlayer roseTree  = case roseTree of
     (Rose (mv1,scr1) []) -> (mv1,scr1)
     (Rose (mv,_) moves) -> (mv, chooseChildAB moves currentPlayer)
       where
          -- | 'chooseChildAB' is used when 'optimalValueAB' detects
          -- a RoseTree with children. It returns the Int value of the
          -- most optimal child, using AB Pruning. 
          chooseChildAB :: [Rose (Move,Int)] -> MinMaxPlayer  -> Int
          chooseChildAB children childPlayer = case children of
            [] -> 0
            x:xs -> case childPlayer  of
                    Max
                      | max alpha (takeSecondEle (minimaxAB [x]
                        (changePlayer childPlayer)
                        alpha beta)) >= beta -> max alpha -- pruning 
                        (takeSecondEle (minimaxAB [x] (changePlayer childPlayer)
                        alpha beta))
                      | otherwise -> takeSecondEle (chooseMoveAB
                        [minimaxAB [x] childPlayer alpha beta,
                        minimaxAB xs childPlayer alpha beta]
                        childPlayer)
                    Min
                       | min beta (takeSecondEle (minimaxAB [x]
                        (changePlayer childPlayer) alpha beta))
                         <= beta -> min beta (takeSecondEle -- pruning 
                         (minimaxAB [x] (changePlayer childPlayer)
                         alpha beta))
                       | otherwise -> takeSecondEle (chooseMoveAB
                         [minimaxAB [x] childPlayer alpha
                         beta,minimaxAB xs childPlayer
                         alpha beta] childPlayer)

  -- | 'chooseMoveAB' chooses the maximum or minimum (Move,Int)
  -- tuple from a list depending on the current player being
  -- Min or Max
  chooseMoveAB :: [(Move,Int)] -> MinMaxPlayer -> (Move,Int)
  chooseMoveAB list current_player = case list of
    [] -> error "Empty list"
    x:xs -> case current_player of
            Max -> foldl (\(mv1,scr1) (mv2,scr2) ->
               if scr1>scr2 then (mv1,scr1) else (mv2,scr2)) x xs
            Min -> foldl (\(mv1,scr1) (mv2,scr2) -> 
              if scr1>scr2 then (mv2,scr2) else (mv1,scr1)) x xs

  -- | 'changePlayer' changes player
  -- between Min & Max
  changePlayer :: MinMaxPlayer -> MinMaxPlayer
  changePlayer p = case p of
   Min -> Max
   Max -> Min

-- Minimax with Weights, Greedy & Mobility Heuristic
-- "minimaxAI" (Default, Successful attempt)

-- General Helper Functions 

-- | 'getUs' returns the "maximising" player
-- or "us" using the initial GameState
getUs :: GameState -> Player
getUs st = case st of
  (GameState _ (Turn x) _) -> x
  _ -> error "Can't retrieve state"

-- | 'getOpponent' returns the "minimizing" player 
-- or "opponent" using the initial GameState
getOpponent :: GameState -> Player
getOpponent old_state = otherPlayer (getUs old_state)

-- | 'takeFirstEle' return the Move from (Move,Int)
takeFirstEle :: (Move,Int) -> Move
takeFirstEle (m,_) = m

-- | 'takeFirstEle' return the Int from (Move,Int)
takeSecondEle :: (Move,Int) -> Int
takeSecondEle (_,scr) = scr

-- Mobility Heuristic 

-- | 'mobilityTreeHeuristic' calculates the number of 
-- legal Move values possible from a given GameState 
-- It uses 'getUs' to detect if the player is us 
-- or opponent & appropriately negates the value if needed
mobilityTreeHeuristic :: Maybe GameState -> GameState -> Int
mobilityTreeHeuristic newSt ogState = case newSt of
  (Just (GameState bnds (Turn p) brd))
      | p == getUs ogState ->
         length (legalMoves 
         (GameState bnds (Turn p) brd))
      | otherwise ->
         (-length (legalMoves 
         (GameState bnds (Turn p) brd)))
  _ -> 0

-- Weights Heurisitc 

-- | 'weights' contains values for each 
-- position of the Othello Board
-- Positive values indicate advantage, 
-- while negative ones indicate disadvantage 
weights :: [Int]
weights = [10,(-3),4,4,4,4,(-3),10
           ,(-3),4,(-1),(-1),(-1),(-1),(-4),(-3)
           ,4,-1,1,0,0,1,(-1),4
           ,4,(-1),0,1,1,0,(-1),4
           ,4,(-1),0,1,1,0,(-1),4
           ,4,(-1),1,0,0,1,(-1),4
           ,(-3),4,(-1),(-1),(-1),(-1),(-4),(-3),
            10,(-3),4,4,4,4,(-3),10]

-- | 'gameStateToBoard' returns the current 
-- Board value from a GameState
gameStateToBoard :: GameState -> Board
gameStateToBoard (GameState _ _ brd) = brd

-- | 'boardToPlayer' converts the current board positions 
-- into weights. If we own the position, it is assigned 1. 
-- If our opponent has it, it is assigned -1. Otherwise, 0. 
boardToPlayer :: [Maybe Player] -> Maybe Player -> Maybe Player -> [Int]
boardToPlayer weightList us opponent = case weightList of
  [] -> []
  x:xs
   |x==Nothing -> 0 : boardToPlayer xs us opponent
   |x==us -> 1 : boardToPlayer xs us opponent
   |otherwise -> -1 : boardToPlayer xs us opponent

-- | 'sumZippedWeights' sums up the 'weights' & 'boardToPlayer' 
-- lists by multiplying each element of the list with its couterpart
sumZippedWeights :: [Int] -> [Int] -> Int
sumZippedWeights boardList weightList = sum (zipWith (*) boardList weightList)

-- | 'weightsTreeHeuristic' is the main weights heuristic function 
-- which gives the weighted sum of any GameState
weightsTreeHeuristic :: Maybe GameState -> GameState -> Int
weightsTreeHeuristic state ogState = case state of
  Nothing -> 0
  (Just st) -> sumZippedWeights (boardToPlayer 
   (concat (gameStateToBoard st)) (Just (getUs ogState))
   (Just (getOpponent ogState))) weights

-- Greedy Heurisitc 

-- | 'treeGreedyHeurisitic' returns the running score difference 
-- of us & our opponent, and appropriately negates it 
treeGreedyHeurisitc :: Maybe GameState -> GameState -> Int
treeGreedyHeurisitc state ogState = case (state,ogState) of
  (Just (GameState _ (Turn opponent) _),GameState _ (Turn us) _)
    | opponent==us -> treeGreedyHelper state
    | otherwise -> (-(treeGreedyHelper state))
    -- if we win in a GameState, a large value is returned.
    -- otherwise an equal negated value is returned
  (Just (GameState _ (GameOver (Winner opponent)) _),GameState _ (Turn us) _)
    | opponent==us -> 10000
    | otherwise -> (-10000)
  -- if the game is a draw for our opponent, it is slighly 
  -- less disadvantageous, so a barely lesser value is returned
  -- when compared to a lost game
  (Just (GameState _ (GameOver Draw) _), _) -> (-9999)
  (Just (GameState _ (GameOver (Winner _)) _),GameState _ (GameOver _) _) -> (-10000)
  (Just (GameState _ (Turn _) _),GameState _ (GameOver (Winner _)) _) -> 10000
  _ -> 0

-- | 'treeGreedyHelper' calculates the running score 
-- for 'treeGreedyHeuristic' 
treeGreedyHelper :: Maybe GameState -> Int
treeGreedyHelper st = case st of
  (Just (GameState _ (GameOver _) _)) -> 10000
  (Just (GameState _ (Turn p) brd)) -> 
    currentScore brd p - currentScore brd (otherPlayer p)
  Nothing -> 0

-- Boss Heuristic (default heuristic)

-- | 'bossHeuristic' combines the power of 'treeGreedyHeuristic',
-- 'mobilityTreeHeuristic' & 'weightsTreeHeuristic' to generate the 
-- most optimal heuristic score possible. Appropriate Weightage has been 
-- assigned to each heuristic after testing
bossHeuristic :: Maybe GameState -> GameState -> Int
bossHeuristic state ogState = (10 * weightsTreeHeuristic state ogState) 
        + (1 * treeGreedyHeurisitc state ogState)
        + (3 * mobilityTreeHeuristic state ogState)

-- Game Tree of any depth 

data Rose a = Rose a [Rose a]
                  deriving (Eq, Show)

-- | 'buildGameTree' generates a list of RoseTree (Move,Int)
-- from all possible moves upto the input depth 
buildGameTree :: [Move] -> GameState -> GameState -> Int -> [Rose (Move,Int)]
buildGameTree moves st ogState depth = case moves of
  [] -> []
  x:xs
   | depth == 1 -> Rose (x,bossHeuristic (applyMove st x) ogState) []
     : buildGameTree xs st ogState depth
   | otherwise ->  Rose (x,bossHeuristic (applyMove st x) ogState) 
     (buildGameTree (maybeLegalMoves (applyMove st x)) 
     (newGameState (applyMove st x) ogState) ogState (depth-1))
     : buildGameTree xs st ogState depth

-- | newGameState returns the new GameState after 
-- a Move is applied using 'applyMove'
newGameState :: Maybe GameState -> GameState -> GameState
newGameState newSt ogSt = case newSt of
        (Just changed) -> changed
        Nothing -> ogSt

-- | maybeLegalMoves generates a list of legal moves 
-- if the GameState exists
maybeLegalMoves :: Maybe GameState -> [Move]
maybeLegalMoves st = case st of
     Just state -> legalMoves state
     _ -> []

-- MiniMax 

-- 'MinMaxPlayer' holds a value of either Max or Min
-- & makes the 'minimax' function easier to interpret
data MinMaxPlayer = Max | Min
  deriving (Show, Eq)

-- | 'minimax' gives the most optimal move tuple from 
-- a list of RoseTrees, depending on if the player is 
-- Min or Max
minimax :: [Rose (Move,Int)] -> MinMaxPlayer -> (Move,Int)
minimax roseTreeList player = case player of
  Max -> chooseMove (map (optimalValue Min) roseTreeList) Max
  Min -> chooseMove (map (optimalValue Max) roseTreeList) Min
  where
  -- | 'optimalValue' chooses an optimal (Move,Int) depending 
  -- on if the player is Min or Max. If there are no children of 
  -- the RoseTree, it simply returns the node value.
  optimalValue :: MinMaxPlayer -> Rose (Move,Int)  -> (Move,Int)
  optimalValue newPlayer roseTree = case roseTree of
     (Rose x []) -> x
     (Rose (mv,_) moves) -> (mv, takeSecondEle (minimax moves newPlayer))
  -- | 'chooseMoveAB' chooses the maximum or minimum (Move,Int)
  -- tuple from a list depending on the current player being
  -- Min or Max
  chooseMove :: [(Move,Int)] -> MinMaxPlayer -> (Move,Int)
  chooseMove list currentPlayer = case list of
     [] -> error "empty list"
     x:xs -> case currentPlayer of
          Max -> foldl (\(mv1,scr1) (mv2,scr2) -> 
            if scr1>scr2 then (mv1,scr1) else (mv2,scr2)) x xs
          Min -> foldl (\(mv1,scr1) (mv2,scr2) -> 
            if scr1>scr2 then (mv2,scr2) else (mv1,scr1)) x xs

-- AI 

-- | A very simple AI, which picks the first move returned by the
-- 'legalMoves' function. AIs can rely on the 'legalMoves' list being
-- non-empty; if there were no legal moves, the framework would have
-- ended the game.
firstLegalMove :: GameState -> Move
firstLegalMove st = head (legalMoves st)

-- | 'greedyAI' is a NoLookAhead AI which picks the next immediate 
-- legal Move which gives us the maximum running score 
greedyAI :: GameState -> Move
greedyAI st = greedyAIHeuristic (legalMoves st) st

-- | 'minimaxAI' is a LookAhead AI which picks a move by looking 
-- ahead an average of 5 moves, by using 'bossHeuristic' which is 
-- the combination of Mobility, Greedy & Weighted Sum Heuristics. 
minimaxAI :: GameState -> Int -> Move
minimaxAI st depth = takeFirstEle (minimax 
 (buildGameTree (legalMoves st) st st depth) Max)

-- | 'minimaxABprune' is a LookAhead AI which picks a move by looking 
-- ahead an average of 8 moves, by using 'bossHeuristic' which is 
-- the combination of Mobility, Greedy & Weighted Sum Heuristics. 
-- It uses AB pruning to boost LookAhead numbers
minimaxABprune :: GameState -> Int -> Move
minimaxABprune st depth = takeFirstEle (minimaxAB 
 (buildGameTree (legalMoves st) st st depth) Max (-10000) 10000)

