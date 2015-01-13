{- Assignment 2 - Finite Automata (due November 11, noon)

Notes:
- You may import Data.List; you may not import any other modules

***Write the names and CDF accounts for each of your group members below.***
<Jesse Chua>, <c2chuajf>
<Name>, <CDF>
-}
module Dfa (State, Symbol, Transition, Automaton(..),
            allStrings, tableToDelta, extend, possibleOutcomes,
            accept, language,
            removeUseless, isFiniteLanguage, language',
            epsilonClosure) where

import Data.List

-- Basic data types
type State = Integer
type Symbol = Char
type Transition = (State, Symbol, State)

-- Automaton Data Type
-- Automaton states alphabet transitions initial final
data Automaton = Automaton [State] [Symbol] [Transition] State [State] deriving Show
-- Some helper functions for you to access the different automaton components
states :: Automaton -> [State]
states (Automaton s _ _ _ _) = s
alphabet :: Automaton -> [Symbol]
alphabet (Automaton _ a _ _ _) = a
transitions :: Automaton -> [Transition]
transitions (Automaton _ _ ts _ _) = ts
initial :: Automaton -> State
initial (Automaton _ _ _ i _) = i
final :: Automaton -> [State]
final (Automaton _ _ _ _ f) = f


-- Questions 1-4: transitions
tableToDelta :: [Transition] -> State -> Symbol -> [State]
tableToDelta trans = \a b -> sort (nub (helperDelta a b trans))

helperDelta :: State -> Symbol -> [Transition] -> [State]
helperDelta a b [] = []
helperDelta a b (x:xs) = if ((tpleIndx1 x) == a) && ((tpleIndx2 x) == b)
					     then (tpleIndx3 x): (helperDelta a b xs)
					     else (helperDelta a b xs) 

tpleIndx1 (x,_,_) = x
tpleIndx2 (_,x,_) = x
tpleIndx3 (_,_,x) = x

--a will be the starting state. b will be the string of symbols
extend :: (State -> Symbol -> [State]) -> (State -> String -> [State])
extend f = \a b -> (helperExtend f [a] b)

--f is the transition function. a is the possible current states.
-- (x:xs) is the string of symbols.
helperExtend :: (State -> Symbol -> [State]) -> [State] -> String -> [State]
helperExtend f a "" = a
helperExtend f a (x:xs) = if (applyTransition f a x) == []
						  then []
						  else helperExtend f (sort (nub (applyTransition f a x))) xs

--returns all possible next states from one symbol
applyTransition :: (State -> Symbol -> [State]) -> [State] -> Symbol -> [State]
applyTransition f [] symbol = []
applyTransition f (x:xs) symbol = (f x symbol) ++ (applyTransition f xs symbol)

allStrings :: [Symbol] -> [[String]]
allStrings symbols = [[""]] ++ helpAllStrings 1 symbols

helpAllStrings :: Int -> [Symbol] -> [[String]]
helpAllStrings n symbols = [makePerm n symbols] ++ helpAllStrings (n + 1) symbols
--finds all permutations
makePerm :: Int -> [Symbol] -> [String]
makePerm 0 symbols = [[]]
makePerm n symbols = [x:y | x <- symbols, y <- (makePerm (n-1) symbols)]

-- auto for the automaton. q for the initial state
possibleOutcomes :: Automaton -> State -> [[(String, [State])]]
possibleOutcomes auto q= [[("",[q])]] ++ helperpO 1 auto q

helperpO :: Int -> Automaton -> State -> [[(String, [State])]]
helperpO n auto q= [subLists n auto q] ++ helperpO (n+1) auto q

--pStr is all possible strings
subLists :: Int -> Automaton -> State -> [(String, [State])]
subLists n auto q = let pStr = ((allStrings (unwords (words (alphabet auto)))) !! n)
					in (map (\x -> (x, (extend ((tableToDelta (transitions auto))) q x))) pStr)

-- Questions 5-6: acceptance
accept :: Automaton -> String -> Bool
--this line gets final state after inputting the given string
accept auto str = let fstate = (extend (tableToDelta (transitions auto)) (initial auto) str)
			  	  in if null (filter (\x -> x `elem` fstate) (final auto))
			      then False
			      else True

language :: Automaton -> [String]
language auto = let str = (allStrings (unwords (words (alphabet auto))))
				in (filter (\x-> (accept auto x)) (concat str))

-- Questions 7-9: finiteness
removeUseless :: Automaton -> Automaton
removeUseless auto = let usefulStates = (removeHelper (length(states auto)) 0 (states auto) (transitions auto) (alphabet auto) (final auto))
	in Automaton
	usefulStates
	(alphabet auto)
	(removeTrans (transitions auto) usefulStates (states auto))
	(initial auto)
	(final auto)

--returns the used transitions
-- useful is the useful states. all is all the states
removeTrans :: [Transition] -> [State] -> [State] -> [Transition]
removeTrans trans useful  all= 
	let notUsed = (filter (\x -> (not (x `elem` useful ))) all)
	in (filter (\x -> removeTransHelper (ltfy1 x) (ltfy2 x) notUsed) trans)
--remove transitions by pattern matching tuples
removeTransHelper _ _ [] = True
removeTransHelper lst1 lst2 (x:xs) = if (x `elem` lst1 || x `elem` lst2)
									 then False
									 else removeTransHelper lst1 lst2 xs

--to access each transition
ltfy1 (x,_,_) = [x]
ltfy2 (_,_,z) = [z]

--returns all useful states
--len is # of states.
--n is a counter to keep track of length n string. (x:xs) is the list of states. 
--trans is the list of transitions. alpha is the string of symbols
--fin is the list of final states
removeHelper :: Int -> Int -> [State] -> [Transition] -> [Symbol] -> [State] ->[State]
removeHelper _ _ [] _ _ _ = []
removeHelper len n (x:xs) trans alpha fin = 
	--makes a list of end states after applying strings of length n. list of lists
	let fstate = 
		(map (\y-> (extend (tableToDelta trans) x y)) 
			(allStrings alpha !! n))
	--check if any of the end states are the final state or greater than len.
	in  if ((not (null (filter (\x -> (not (null(filter (\y -> y `elem` fin ) x)))) fstate)))
		|| x `elem` fin) && (len >= n)
		then [x] ++ removeHelper len 0 xs trans alpha fin
		else if len == n
			 then removeHelper len 0 xs trans alpha fin
			 else removeHelper len (n+1) (x:xs) trans alpha fin

isFiniteLanguage :: Automaton -> Bool
								--gets all possible strings of length n+1
isFiniteLanguage auto = let all = allStrings (alphabet auto) !! (length(states auto) + 1)
							--put 1 in list if no possible state is transitioned to; 0 otherwise.
							--if there is a 0 in the list then false. Means there is a string
							--of length n+1 that can transition to a state
					    in if 0 `elem` (map (\x -> 
					   					if (null (extend 
					   						(tableToDelta (transitions auto)) (initial auto) x))
					    					then 1
					    					else 0) 
					    				all)
					    	then False
					    	else True

language' :: Automaton -> [String]
language' auto = if (isFiniteLanguage auto)
				 then (take (length (states (removeUseless auto))) (language auto))
				 else (language auto)

-- Question 10: epsilon transitions
epsilonClosure :: Automaton -> [State] -> [State]
epsilonClosure auto state = sort(nub(state ++ epsilonHelper state (transitions auto)))
--checks tuples for epsilon transition
epsilonHelper :: [State] -> [Transition] -> [State]
epsilonHelper _ [] = []
epsilonHelper state (x:xs) = if (tpleIndx1 x `elem` state && tpleIndx2 x == ' ')
							 then tpleIndx3 x: (epsilonHelper state xs)
							 else (epsilonHelper state xs)
