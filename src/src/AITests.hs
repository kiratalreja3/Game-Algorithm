{-|
Module      : AITests
Description : Tests for your AI functions
Copyright   : (c) 2020 Your Name Here
License     : AllRightsReserved
-}
module AITests where

import           AI
import           Othello
import           Testing

aiTests :: Test
aiTests = TestGroup "AI"
  [ getUsOpponentTest
  ,takeEleTest
  ,mobilityHeuristicTest
  ,weightsTreeHeuristicTest
  ,treeGreedyHeuristicTest
  ,bossHeuristicTest
  ,minimaxTest
  ]

-- | Tests for all the possible main & helper functions 
-- for default AI, using the initial state

-- Tests involving large lists & rose trees 
-- have been omitted 

getUsOpponentTest :: Test 
getUsOpponentTest = TestGroup "getUsOpponent"
   [ Test "Player1" (assertEqual
    (getUs (GameState (8,8) (Turn Player1)
    (initialBoard (8,8)))) Player1)
   , Test "Player2" (assertEqual (getUs
    (GameState (8,8) (Turn Player2) 
    (initialBoard (8,8)))) Player2)
   , Test "Player1" (assertEqual (getOpponent
    (GameState (8,8) (Turn Player1)
    (initialBoard (8,8)))) Player2)
   , Test "Player2" (assertEqual (getOpponent 
     (GameState (8,8) (Turn Player2)
     (initialBoard (8,8)))) Player1)
   ]

takeEleTest :: Test 
takeEleTest = TestGroup "takeEleTest"
   [Test "firstEle" (assertEqual 
   (takeFirstEle (Move (2,3),3)) (Move (2,3)))
   ,Test "secondEle" (assertEqual 
   (takeSecondEle (Move (2,3),3)) 3)
   ]

mobilityHeuristicTest :: Test 
mobilityHeuristicTest = Test "mobilityTest" (assertEqual
  (mobilityTreeHeuristic (Just (GameState (8,8) (Turn Player1)
  (initialBoard (8,8)))) (GameState (8,8) (Turn Player1) 
  (initialBoard (8,8)))) 4)

weightsTreeHeuristicTest :: Test 
weightsTreeHeuristicTest = TestGroup "weightTests"
                           [ Test "sumZippedWeights" (assertEqual
                            (sumZippedWeights [1,2,3] [3,4,5]) 26)
                           , Test "weightsTreeHeuristic" (assertEqual
                            (weightsTreeHeuristic (Just (GameState (8,8) 
                            (Turn Player1) (initialBoard (8,8)))) 
                            (GameState (8,8) (Turn Player1) 
                            (initialBoard (8,8)))) 0)
                           , Test "boardToPlayer" (assertEqual (boardToPlayer 
                           [Nothing,Nothing,(Just Player1),(Just Player2)]
                           (Just Player1) (Just Player2)) [0,0,1,-1])
                           ]
 
treeGreedyHeuristicTest :: Test 
treeGreedyHeuristicTest = TestGroup "greedyTests"
                          [ Test "treeGreedyHeurisitc" (assertEqual 
                          (treeGreedyHeurisitc (Just (GameState (8,8) 
                          (Turn Player1) (initialBoard (8,8)))) (GameState (8,8)
                           (Turn Player1) (initialBoard (8,8)))) 0 )
                          ,Test "treeGreedyHelper" (assertEqual 
                          (treeGreedyHelper (Just (GameState (8,8) 
                          (Turn Player1) (initialBoard (8,8))))) 0)
                          ]

bossHeuristicTest :: Test 
bossHeuristicTest = Test "bossTest" (assertEqual (bossHeuristic 
 (Just (GameState (8,8) (Turn Player1) (initialBoard (8,8)))) 
 (GameState (8,8) (Turn Player1) (initialBoard (8,8)))) 12)

minimaxTest :: Test 
minimaxTest = TestGroup "minimax"
             [Test "minimaxAI3depth" (assertEqual
              (minimaxAI (GameState (8,8) (Turn Player1)
              (initialBoard (8,8))) 3) (Move (4,5)))
             ,Test "minimaxAI5depth" (assertEqual 
              (minimaxAI (GameState (8,8) (Turn Player1)
              (initialBoard (8,8))) 5) (Move (4,5))) 
             ,Test "newGameState" (assertEqual (newGameState
              Nothing (GameState (8,8) (Turn Player1) 
              (initialBoard (8,8)))) (GameState (8,8) 
              (Turn Player1) (initialBoard (8,8))))
             ,Test "maybeLegalMoves" (assertEqual 
             (maybeLegalMoves Nothing ) [])
             ,Test "minimaxMin" (assertEqual (minimax 
             [Rose (Move (3,2),14) [],Rose (Move (2,3),14) 
             [],Rose (Move (5,4),14) [],Rose (Move (4,5),14)
             []] Min) (Move (3,2),14))
             ,Test "minimaxMax" (assertEqual (minimax 
             [Rose (Move (3,2),14) [],Rose (Move (2,3),14) 
             [],Rose (Move (5,4),14) [],Rose (Move (4,5),14) 
             []] Max) (Move (4,5),14))
             ]

