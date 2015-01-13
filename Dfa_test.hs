{- Sample tests for Assignment 2 -}
import Test.HUnit
import Dfa (State, Symbol, Transition, Automaton(..),
            allStrings, tableToDelta, extend, possibleOutcomes,
            accept, language,
            removeUseless, isFiniteLanguage, language', epsilonClosure)


tableToDeltaTests = TestList [
    --simple accepted test
    [2] ~=? tableToDelta [(1, 'f', 2)] 1 'f',
    -- Note: a symbol could be passed in that doesn't appear in any transition
    [] ~=? tableToDelta [(1, 'f', 2)] 1 'b',
    --empty string
    [] ~=? tableToDelta [(1, 'f', 2)] 1 ' ',
    --multiple with no right transition
    [] ~=? tableToDelta [(1, 'f', 2), (2, 'f', 2)] 1 'b',
    --returning multiple states
    [1,2] ~=? tableToDelta [(1, 'f', 2), (1, 'f', 1)] 1 'f',
    --starting from useless state
    [] ~=? tableToDelta [(1, 'f', 2), (2, 'f', 2)] 3 'f',
    --a lot of states with same transition but different start state
    [0,2] ~=? tableToDelta 
            [(0,'a',1), (1,'a',2), (0,'b',0), (1,'b',1), (2,'b',2), (1,'a',0)] 1 'a'
    ]

extendTests = TestList [
    --more than one transition
    [2] ~=? extend (tableToDelta [(1, 'f', 2), (2, 'f', 2)]) 1 "ff",
    --empty string
    [] ~=? extend (tableToDelta [(1, 'f', 2), (2, 'f', 2)]) 1 " ",
    --not accepted string
    [] ~=? extend (tableToDelta [(1, 'f', 2), (2, 'f', 2)]) 1 "g",
    --useless start state
    [] ~=? extend (tableToDelta [(1, 'f', 2), (2, 'f', 2)]) 3 "f",
    --starting not at the initial state
    [2] ~=? extend (tableToDelta [(1, 'f', 2), (2, 'f', 2)]) 2 "f",
    --repeating the same state multiple times
    [] ~=? extend (tableToDelta [(1, 'f', 2), (2, 'f', 2)]) 3 "ffffff",
    --multiple states returned
    [1,2] ~=? extend (tableToDelta [(1, 'f', 2), (1, 'f', 1), (2, 'f', 1)]) 1 "ff",
    --multiple transitions
    [0,2] ~=? extend (tableToDelta 
            [(0,'a',1), (1,'a',2), (0,'b',0), (1,'b',1), (2,'b',2), (1,'a',0)]) 0 "aba"
    ]

allStringsTests = TestList [
    --longer than 1 character string
    ["aa", "ab", "ba", "bb"] ~=? allStrings "ab" !! 2,
    --one symbol
    ["aaa"] ~=? allStrings "a" !! 3,
    --make sure first index is no string
    [""] ~=? allStrings "ab" !! 0,
    --more symbols than index
    ["aa", "ab", "ac", "ba", "bb", "bc", "ca", "cb", "cc"] ~=? allStrings "abc" !! 2,
    --empty string
    [] ~=? allStrings"" !! 1
    ]

possibleOutcomesTests = TestList [
    --multiple transitions
    [("aa",[1]), ("ab",[0,2]), ("ba",[0,2]), ("bb",[1])] ~=?
        (possibleOutcomes (Automaton [0,1,2]
                                     ['a','b']
                                     [(0,'a',1),
                                      (1,'a',2),
                                      (0,'b',0),
                                      (1,'b',1),
                                      (2,'b',2),
                                      (1,'a',0)] 0 [2]) 1) !! 2,
    --state not in automaton
    [("aa",[]), ("ab",[]), ("ba",[]), ("bb",[])] ~=?
        (possibleOutcomes (Automaton [0,1,2]
                                     ['a','b']
                                     [(0,'a',1),
                                      (1,'a',2),
                                      (0,'b',0),
                                      (1,'b',1),
                                      (2,'b',2),
                                      (1,'a',0)] 0 [2]) 3) !! 2,
    --zero index
    [("",[2])] ~=?
        (possibleOutcomes (Automaton [0,1,2]
                                     ['a','b']
                                     [(0,'a',1),
                                      (1,'a',2),
                                      (0,'b',0),
                                      (1,'b',1),
                                      (2,'b',2),
                                      (1,'a',0)] 0 [2]) 2) !! 0,
    --one symbol
    [("aa",[0,2])] ~=?
        (possibleOutcomes (Automaton [0,1,2]
                                     ['a']
                                     [(0,'a',1),
                                      (1,'a',2),
                                      (0,'b',0),
                                      (1,'b',1),
                                      (2,'b',2),
                                      (1,'a',0)] 0 [2]) 0) !! 2
    ]

a1 = Automaton [0,1] ['a'] [(0,'a',1),(1,'a',0)] 0 [0]
a12 = Automaton [0,1] ['a'] [(0,'a',1)] 0 [0]
a13 = Automaton [0,1] ['a'] [] 0 [0]
acceptTests = TestList [
    -- if returning True when in goal state
    True ~=? accept a1 "aa",
    --not in automaton
    False ~=? accept a1 "b",
    --string leading to no transitions
    False ~=? accept a12 "aa",
    --long string
    False ~=? accept a1 "aaaaa",
    --no transitions
    False ~=? accept a13 "aaaaa",
    --Making sure it is not extending test
    False ~=? accept a1 "a"
    ]

a14 = Automaton [0,1] ['a'] [(0,'a',1),(1,'a',0)] 0 [1]
a15 = Automaton [0,1] ['a','b'] [(0,'a',1),(1,'a',0), (0,'b',1),(1,'b',0)] 0 [1]

languageTests = TestList [
    --checking if return correct for expected case
    ["","aa"] ~=? take 2 (language a1),
    --first index not always empty string
    ["a"] ~=? take 1 (language a14),
    --checking if all n strings come before n+1 strings
    ["","aa","aaaa","aaaaaa"] ~=? take 4 (language a1),
    --checking if alphabetical in order
    ["a","b","aaa","aab","aba"] ~=? take 5 (language a15)
    ]

a2 = Automaton [0,1] ['a'] [(0,'a',1)] 0 [0]
a16 = Automaton [0,1,2] "a" [(0,'a',1),(1,'a',0)] 0 [1]
a17 = Automaton [0,1] "a" [(0,'a',1)] 0 [1]
a18 = Automaton [0,1,2,3,4] "a" [] 0 [0]

eq :: Automaton -> Automaton -> Bool
eq (Automaton s1 a1 ts1 i1 f1) (Automaton s2 a2 ts2 i2 f2) =
    s1 == s2 &&
    a1 == a2 &&
    ts1 == ts2 &&
    i1 == i2 &&
    f1 == f2

removeUselessTests = let a3 = removeUseless a2
    in
    TestList [
        --typical test
        True ~=? eq a3 (Automaton [0] ['a'] [] 0 [0]),
        --dont remove initial or final state
        True ~=? eq (removeUseless a14) (Automaton [0,1] "a" [(0,'a',1),(1,'a',0)] 0 [1]),
        --remove state with no transitions to or from it
        True ~=? eq (removeUseless a16) (Automaton [0,1] "a" [(0,'a',1),(1,'a',0)] 0 [1]),
        --dont remove final state with no transitions out of it
        True ~=? eq (removeUseless a17) (Automaton [0,1] "a" [(0,'a',1)] 0 [1]),
        --no transitions. 
        True ~=? eq (removeUseless a18) (Automaton [0] "a" [] 0 [0])
        ]

isFiniteLanguageTests = TestList [
    --finite test
    True ~=? isFiniteLanguage a2,
    --no transitions
    True ~=? isFiniteLanguage a18,
    --not finite
    False ~=? isFiniteLanguage a16,
    --more than one transition
    False ~=? isFiniteLanguage a15
    ]

a19 = Automaton [0,1,2] "a" [(0,'a',1), (1,'a',2)] 0 [2]
a20 = Automaton [0,1,2,3] "ab" [(0,'a',1), (0,'a',2), (1,'a',3), (2,'b',3)] 0 [3]

language'Tests = TestList [
    --finite language
    [""] ~=? language' a2,
    --finite language with 0 char < string
    ["aa"] ~=? language' a19,
    --non finite language
    ["","aa","aaaa","aaaaaa"] ~=? take 4 (language' a1),
    --non finite in alphabetical order
    ["a","b","aaa","aab","aba"] ~=? take 5 (language' a15),
    --more than one string finite
    ["aa", "ab"] ~=? language' a20
    ]

a3 = Automaton [0,1,2] ['a','b'] [(0,' ',2),(0,'a',1),(2,'b',0)] 0 [1]
a31 = Automaton [0,1,2] ['a','b'] [(0,' ',2),(0,' ',1),(2,'b',0)] 0 [1]

epsilonClosureTests = TestList [
    --one epsilon transition
    [0,2] ~=? epsilonClosure a3 [0],
    --two epsilon transitions
    [0,1,2] ~=? epsilonClosure a31 [0],
    --more than one state
    [0,1,2] ~=? epsilonClosure a3 [0,1],
    --no epsilon transition
    [0] ~=? epsilonClosure a19 [0],
    --given state that isn't the start state
    [2] ~=? epsilonClosure a3 [2],
    --no state given
    [] ~=? epsilonClosure a3 []
    ]

main :: IO ()
main = do
    -- Put each call to "runTestTT" on a separate line
    runTestTT tableToDeltaTests
    runTestTT extendTests
    runTestTT allStringsTests
    runTestTT possibleOutcomesTests
    runTestTT acceptTests
    runTestTT languageTests
    runTestTT removeUselessTests
    runTestTT isFiniteLanguageTests
    runTestTT language'Tests
    runTestTT epsilonClosureTests
    return ()
