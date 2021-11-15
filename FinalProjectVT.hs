{-
Victor C Thompson
Final Project
4/24/2021

Reads a fsa from a file and then asks 
the user to input a string. Tells user
whether the string was accepted by the 
fsa then loops and asks for another 
string. To end program, user must input:
%quit%

Example of a fsa that would be found 
in a file (minus the comments):
a b					--alphabet 
q0 q1 q2            --states
q0					--init state
q0					--final state
q0, a -> q1         --transitions
q0, b -> q0
q1, a -> q2
q1, b -> q0
q2, a -> q2
q2, b -> q2

Throws error if there is no way to reach a 
final state from the init state (not done yet)
-}

type State = String 
type Input = Char

data Fsa = Fsa {alphabet :: [Input],
                states :: [State],
                initial :: State,
                final :: [State],
                transition :: [((State, Input), State)]}
  deriving Show

{-
Calls the inputAnalyzer function with the fsa, initial state,
and input string and returns the result.
-}
analyze :: Fsa -> String -> Bool
analyze fsa s = inputAnalyzer fsa "q0" s 

{-
The inputAnalyzer function takes a given fsa, the current state, and 
a list of the input left and uses the current state and next input to 
transition to the next state. If the list is empty and the current 
state is a final state the string is accepted, otherwise if the list 
is empty and the current state is not a final state the string is not 
accepted. If an input is not in the alphabet the string is rejected.
-}
inputAnalyzer :: Fsa -> State -> [Input] -> Bool 
inputAnalyzer (Fsa {final = f}) q [] | q `elem` f = True
                                     | otherwise = False
inputAnalyzer fsa@(Fsa {alphabet = a, transition = t}) q (i:is)
-- guards make sure all letters in the string are part of the alphabet
--Previous DFA-only version 
    -- | i `elem` a = inputAnalyzer fsa (lookUp q i t) is
    | i `elem` a = interpret [inputAnalyzer fsa x is | x <- pStates]
    | otherwise = False 
    where pStates = lookUp q i t 

{-
Helper function called by inputAnalyzer. This was added to help the 
inputAnalyzer interpret NFAs instead of just DFAs.  It simply checks
all paths to see whether or not one of the possible paths leads to 
the acceptance of the string.
-}
interpret :: [Bool] -> Bool
interpret [] = False 
interpret (x:xs) = x || interpret xs 

{-
The lookUp function is a helper function to inputAnalyzer. It takes the current
state, current input, and the list of transitions to replace the current state 
with the next state according to the list of transitions.
-}
--Previous DFA-only version 
-- lookUp :: State -> Input -> [((State, Input), State)] -> State
-- lookUp q i (((cstate,cinput), nstate):ts) | (q == cstate && i == cinput) = nstate
                                          -- | otherwise = lookUp q i ts 
lookUp :: State -> Input -> [((State, Input), State)] -> [State]
lookUp q i [] = []
--first statement is for lambda transitions. The "++ (lookUp q i ts)" is for
--the case that there are multiple \ transitions from the current state 
lookUp q i (((cstate,cinput), nstate):ts) | (q == cstate && cinput == '\\') = (lookUp nstate i ts) ++ (lookUp q i ts)
                                          | (q == cstate && i == cinput) = nstate : (lookUp q i ts) 
                                          | otherwise = lookUp q i ts

{-
Parses the input taken from the file and turns it into an fsa.
We know the first four lines will be the alphabet, set of states,
the initial state, and the set of final states in that order. 
The rest of the lines represent the transition functions and are 
passed to the process function.
-}
parse :: [String] -> Fsa
parse (a:s:i:f:xs) = Fsa {alphabet = [x | x <- a, x /= ' '], 
                          states = words s, 
                          initial = i, 
                          final = words f, 
                          transition = process xs}

{-
The process function takes a list of strings where each string 
represents an individual transition function. Each transition is 
first passed to the preproc method and then they are concatenated
into a list of transitions, [((State, Input), State)].
-}
process :: [String] -> [((State, Input), State)]
process [] = []
process (s:xs) = 
  let s' = words $ preproc s 
      (s1:s2:s3) = s'
      s2' = head s2 --turns string into char
      s3' = head s3 --turns list of strings into string 
  in [((s1, s2'), s3')] ++ process xs 

{-
Removes the "," and "->" characters from the strings which are
in the original file for readability.
-}
preproc :: String -> String
preproc [] = []
preproc (',':xs) = preproc xs
preproc ('-':'>':xs) = preproc xs
preproc (x:xs) = x : preproc xs

{-
Prompts use for a filename, reads the contents of the file, 
and separates the contents by line. From there it enters a 
loop, prompting the user for strings and then telling them
whether or not the string is accepted by the fsa.
-}
main :: IO()
main = do 
  putStrLn "Enter filename:"
  filename <- getLine
  contents <- readFile filename
  let separated = lines contents        --separates contents of file into lines 
  --let parsed = parse separated        --commented lines are for testing purposes
  --putStrLn (show separated)
  --putStrLn (show parsed)
  let loop = do 
             putStrLn "Enter string(%quit% to exit):"
             string <- getLine 
             if string == "%quit%" 
             then return() 
             else do 
                     if analyze (parse separated) string   
                     then putStrLn "Accepted\n" >> loop 
                     else putStrLn "Not accepted\n" >> loop
  loop

{-
Sources:
https://stackoverflow.com/questions/35639501/haskell-do-while-loop

http://learnyouahaskell.com/starting-out#im-a-list-comprehension
-}