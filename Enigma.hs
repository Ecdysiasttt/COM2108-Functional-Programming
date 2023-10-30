module Enigma where
  import Data.Char  -- to use functions on characters
  import Data.Maybe -- breakEnigma uses Maybe type
  import Data.List -- needed for findIndices

{- Part 1: Simulation of the Enigma -}

  type Rotor = (String, Int)
  type Reflector = [(Char, Char)]
  type Offsets = (Int, Int, Int)
  type Stecker = [(Char, Char)]

  data Enigma = SimpleEnigma {r3 :: Rotor, r2 :: Rotor, r1 :: Rotor, rflct :: Reflector, offsets :: Offsets}
                | SteckeredEnigma {r3 :: Rotor, r2 :: Rotor, r1 :: Rotor, rflct :: Reflector, offsets :: Offsets, stecker :: Stecker}

  {- !! NOTE !!
     The order of the rotors supplied throughout this file is the order in which
     an operator would view them in the machine, i.e. enigma rotor1 rotor2 rotor3 ... 
                                                            = r3 r2 r1
                                                              LR MR RR
  -}  

  -- Encodes the string
  encodeMessage :: String -> Enigma -> String
  encodeMessage [] _ = ""
  encodeMessage (x:xs) enigma
  -- If the current char is not a valid one, it is not encoded and no new offsets are calculated
    | isLetter x == False = encodeMessage xs enigma
    | otherwise = shiftChar newOffset (toUpper c) : encodeMessage xs newOffset
    where
      -- Swaps char now if it is steckered. Makes shiftChar a little neater
      c = if isSteckered enigma then swapChar (stecker enigma) x else x
      newOffset = enigma {offsets = newOffsets (offsets enigma) (r2 enigma) (r1 enigma)}
  
  -- Calls rotorShift for each rotor and offsets, and swaps the char if it is steckered
  shiftChar :: Enigma -> Char -> Char
  shiftChar enigma c = if isSteckered enigma then swapChar (stecker enigma) backward else backward
    where
      (x, y, z) = offsets enigma
      (r3o, r2o, r1o) = ((r3 enigma, x),(r2 enigma, y),(r1 enigma, z))
      forward = rotorShift r3o True . rotorShift r2o True $ rotorShift r1o True c
      backward = rotorShift r1o False . rotorShift r2o False . rotorShift r3o False $ swapChar (rflct enigma) forward

  -- Encodes through a single rotor
  rotorShift :: (Rotor, Int) -> Bool -> Char -> Char
  rotorShift (rotor, offset) forward c
    | forward == True = int2let $ ((alphaPos forwardShiftC) - offset) `mod` 26                                
    | forward == False = int2let $ (head indexOfC - offset) `mod` 26
    where
      offsetC = ((alphaPos c) + offset) `mod` 26
      forwardShiftC = (fst rotor)!!offsetC
      indexOfC = findIndices (==(int2let offsetC)) $ fst rotor

  {- Implements 'correct' enigma knock-on:
     Left and middle rotors will advance when the rotor to their
     right moves TO the knock-on position, not as it moves past it
  -}
  newOffsets :: Offsets -> Rotor -> Rotor -> Offsets
  newOffsets (a, b, c) r2 r1 = (newA, newB, newC)
      where
          newC = (c + 1) `mod` 26
          newB = if newC == snd r1 then (b + 1) `mod` 26 else b
          newA = if newB == snd r2 && newC == snd r1 then (a + 1) `mod` 26 else a

  {- Unzips plugboard/stecker e.g. returns ("FDVKHI","TUAWZX") for stecker.
     Looks for index of supplied char in either string, and returns char at same index
     in the other string. If no match is found (i.e. for stecker), returns input char
  -}
  swapChar :: Stecker -> Char -> Char
  swapChar pairs c 
    | fstMatch /= [] = snd(unzipped)!!(head fstMatch)
    | sndMatch /= [] = fst(unzipped)!!(head sndMatch)
    | otherwise = c
    where
      unzipped = unzip pairs
      fstMatch = findIndices (==c) . fst $ unzipped
      sndMatch = findIndices (==c) . snd $ unzipped
  
  isSteckered :: Enigma -> Bool
  isSteckered (SimpleEnigma _ _ _ _ _) = False
  isSteckered (SteckeredEnigma _ _ _ _ _ _) = True
      
  -- Opposite of alphaPos, taken from the Caesar cypher lab
  int2let :: Int -> Char
  int2let n = chr $ ord 'A' + n

{- Part 2: Finding the Longest Menu -}

  type Menu = [Int]
  type Crib =  [(Char, Char)]

  longestMenu :: Crib -> Menu
  longestMenu _ = []

{- Part 3: Simulating the Bombe -}

  breakEnigma :: Crib -> Maybe (Offsets, Stecker)
  breakEnigma _ = Nothing

{- Useful definitions and functions -}

   -- substitution cyphers for the Enigma rotors
   -- as pairs of (wirings, knock-on position)
   -- knock-on position is where it will cause the next left wheel to
   -- advance when it moves past this position
 
        --"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  rotor1=("EKMFLGDQVZNTOWYHXUSPAIBRCJ",17::Int)
  rotor2=("AJDKSIRUXBLHWTMCQGZNPYFVOE",5::Int)
  rotor3=("BDFHJLCPRTXVZNYEIWGAKMUSQO",22::Int)
  rotor4=("ESOVPZJAYQUIRHXLNFTGKDCMWB",10::Int)
  rotor5=("VZBRGITYUPSDNHLXAWMJQOFECK",0::Int)

  {- the standard Enigma reflector (Reflector B)
    swapped A<->Y, B<->R, C<->U,D<->H, E<->Q, F<->S, G<->L, 
            I<->P, J<->X, K<->N, M<->O, T<->Z,V<->W
  -}
  reflectorB= [('A','Y'),
              ('B','R'),
              ('C','U'),
              ('D','H'),
              ('E','Q'),
              ('F','S'),
              ('G','L'),
              ('I','P'),
              ('J','X'),
              ('K','N'),
              ('M','O'),
              ('T','Z'),
              ('V','W')]

  {- alphaPos: given an uppercase letter, returns its index in the alphabet
     ('A' = position 0; 'Z' = position 25)
  -}
  alphaPos :: Char -> Int
  alphaPos c = (ord c) - ord 'A'