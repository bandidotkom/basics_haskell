
--- ALPI Turing-Machine-Simulator
--- Original version: WS-97-98 Prof. Dr. Raul Rojas
--- modified WS-12-13 by: Prof. Dr. Margarita Esponda

module Turing (sim, start) where

data Move = L | R | S
              deriving (Show, Eq)

type State = Int
type Input = Char
type Output = Char

type Tape = ([Char], Char, [Char])
type StateTable =  [(State, Input, Output, Move, State)]
type History = [(([Char], Char, [Char]), State)]


step ::  (Tape, State)  ->  StateTable -> (Tape, State)
step ((l, input, r), state)  s_table 
                 | state==0  = ((l, input, r), state)
                 | otherwise = ((new_l, new_input, new_r), n_state)
                                where
                                  (n_state, out, dir) = find_next (state, input) s_table
                                  (new_l, new_input, new_r) 
                                                | dir==L = move_left  (l, out, r) 
                                                | dir==R = move_right (l, out, r)
                                                | dir==S = (l, out, r)
                              

find_next :: (State, Input) -> StateTable -> (State, Output, Move)
find_next (state, input) [] = error "END OF STATE-TABLE and not Matching founded"
find_next (state, input) ((s, i, out, dir, ns):rest) 
                    | (state==s && input==i)   = (ns, out,   dir)
                    | (state==s && '#' == i)   = (ns, input, dir)
                    | otherwise                = find_next (state, input) rest

move_left :: Tape -> Tape
move_left (l, h, r) 
                   | new_h == '_'  =  error "left margin reached"
                   | otherwise     = (tail l, new_h, h:r)
                                      where 
                                          new_h = head l

move_right :: Tape -> Tape
move_right(l, h, r) 
                   | new_h == '_'  = error "right margin reached"
                   | otherwise     = (h:l, new_h, tail r)
                                      where 
                                          new_h = head r

sim :: (Tape, State) -> StateTable -> History -> History
               
sim ((l, input, r),     0) s_table  history = history
sim ((l, input, r), state) s_table  history = sim n_tape s_table (n_tape:history)
                                            where 
                                              n_tape = step ((l,input,r), state) s_table

start ((l,input,r), state) s_table 
           = putStr (show_states (sim ((l1,input,r1), state) s_table [((l1,input,r1),state)]))
             where 
                    l1 = reverse l ++ empty_tape
                    r1 = r ++ empty_tape

-- auxiliary output functions

show_states [] = ""
show_states (x:xs) = (show_states xs)++"  "++show_state(x)

show_state ((l,input,r), state) = "  "++rev_l++[input]++(cut_tape r)++"  "++(show state) ++"\n"
                                  ++(spaces (4+(length rev_l)))++"T\n"
                                  where 
                                     rev_l = reverse_finite l []

spaces n = [' '|_<-[1..n]]

cut_tape ('_':x) = []
cut_tape (x:y) = x:(cut_tape y)

reverse_finite ('_':x) y = y
reverse_finite (x:xs)  y = reverse_finite xs (x:y)

empty_tape :: [Char]
empty_tape = '_':empty_tape

-- TM-Examples

-- increment a binary number in two's-complement representation 

inc  = [ (   add1, ' ', ' ', L,   add1),
         (   add1, '0', '1', L, goback),
         (   add1, '1', '0', L,   add1),
         (   add1, ' ', ' ', S,   stop),
         ( goback, ' ', ' ', S,   stop),
         ( goback, '#', '#', L, goback)
       ]
         where
            add1   = 1
            goback = 2
            stop   = 0

test_inc_1011111 = start (("_ 11011111",' ',"_"), 1) inc

-- convert an unary number into a binary number

unary2binary = [ ( findB, 'B', 'X', L,  inc1),
                 ( findB, ' ', ' ', L,  delX),
                 ( findB, '#', '#', R, findB),

                 (  inc1, '1', '0', L,  inc1),
                 (  inc1, '0', '1', R, findB),
                 (  inc1, ' ', '1', R, findB),
                 (  inc1, 'X', 'X', L,  inc1),

                 (  delX, 'X', ' ', L,  delX),
                 (  delX, '0', '0', S,  stop),
                 (  delX, '1', '1', S,  stop)
                ]  
                where
                   findB = 1
                   inc1  = 2
                   delX  = 3
                   stop  = 0

test_unary2binary = start (("_        ",'B',"BBBBBBB  _"), 1) unary2binary

-- calculate n/2 of a binary number

div2 = [ (   w0, '0', '0', R,   w0),
         (   w0, '1', '0', R,   w1),
         (   w1, '1', '1', R,   w1),
         (   w1, '0', '1', R,   w0),
         (   w0, ' ', ' ', L, back), 
         (   w1, ' ', ' ', L, back),
         ( back, '0', '0', L, back), 
         ( back, '1', '1', L, back),
         ( back, ' ', ' ', R,  end) 
       ]
        where
           w0   = 1
           w1   = 2
           back = 3
           end  = 0

test_div2 = start (("_ ",'0',"010101 _"), 1) div2

-- checks the balancing of parenthesis

klammer= [ ( start, 'A', 'A', R,   find2),

           ( find2, '(', '(', R,  find2),
           ( find2, 'X', 'X', R,  find2),
           ( find2, ')', 'X', L,  findC),
           ( find2, 'A', 'A', L, checkC),

           ( findC, 'X', 'X', L,  findC),
           ( findC, '(', 'X', R,  find2),
           ( findC, 'A', '0', S,   halt),

           (checkC, 'X', 'X', L, checkC),
           (checkC, '(', '(', L,  error),
           (checkC, 'A', '1', S,   halt),

           ( error, 'A', '0', S,   halt),
           ( error, '#', '#', L,  error)
         ]
           where 
                start = 1  
                find2 = 2  -- look for )
                findC = 3  -- look for (
                checkC = 4 -- go back and check of any ( was left
                error = 5  -- go back to start position and write a zero
                halt  = 0

test_klammer_0 = start (("_",'A',"(()())A_"), 1) klammer
test_klammer_1 = start (("_",'A',"(()))A_"), 1) klammer
test_klammer_2 = start (("_",'A',"(()())(()A_"), 1) klammer

-- copy an unary number

unary_number_copy = [ ( 1, '1', 'A', R, 1 ),
                      ( 1, 'A', 'A', R, 2 ),
                      ( 1, '0', '0', L, 6 ),
                      ( 2, '1', '1', R, 2 ),
                      ( 2, '0', '0', R, 3 ),
                      ( 3, '1', '1', R, 3 ),
                      ( 3, '0', '1', S, 4 ),
                      ( 4, '1', '1', L, 4 ),
                      ( 4, '0', '0', L, 5 ),
                      ( 5, '1', '1', L, 5 ),
                      ( 5, 'A', 'A', R, 1 ),
                      ( 6, '1', '1', L, 6 ),
                      ( 6, 'A', '1', S, 6 ),
                      ( 6, '0', '0', R, 0 )
                    ]

test_copy = start (("_00",'1',"1100000000_"), 1) unary_number_copy

-- this is a first very simple example for the UTM (Universal Turing-Machine)

deleteOnes = [ (1, '0', '0', L, 0),
               (1, '1', '0', L, 1)
             ]

test_deleteOnes = start (("_011",'1',"000___"), 1) deleteOnes

-------------------------------------------------------------------------
universal_TM_table = [
            (  1, 'Y', 'Y', R,   2 ), -- start (P0)
            (  1, '0', 'A', L,   1 ),
            (  1, '1', 'B', L,   1 ),
            (  1, '#', '#', L,   1 ),

            (  2, 'A', '0', R,   3 ),  -- searching a matching state (P1)
            (  2, 'B', '1', R,   4 ),
            (  2, 'X', 'X', S,   7 ),   
            (  2, '#', '#', R,   2 ),
            (  3, '1', 'B', R,   5 ),  
            (  3, '0', 'A', L,   6 ),
            (  3, '#', '#', R,   3 ),
            (  6, 'Y', 'Y', R,   2 ),
            (  6, '#', '#', L,   6 ),
            (  4, '1', 'B', L,   6 ),
            (  4, '0', 'A', R,   5 ),
            (  4, '#', '#', R,   4 ),
            (  5, 'Y', 'Y', S,   0 ), -- end of the programm
            (  5, 'X', 'X', L,   1 ),
            (  5, '#', '#', R,   5 ),

            (  7, '1', 'B', L,   8 ), -- copy the new state (P2)
            (  7, '0', 'A', L,   9 ), 
            (  7, '#', '#', R,   7 ),
            (  8, 'Y', 'Y', R,  10 ),
            (  8, '#', '#', L,   8 ),
            (  9, 'Y', 'Y', R,  11 ),
            (  9, '#', '#', L,   9 ),
            ( 10, '0', 'B', R,  12 ),
            ( 10, '1', 'B', R,  12 ),
            ( 10, 'X', 'X', L,  13 ), 
            ( 10, '#', '#', R,  10 ),
            ( 11, '0', 'A', R,  12 ),
            ( 11, '1', 'A', R,  12 ),
            ( 11, 'X', 'X', L,  14 ), 
            ( 11, '#', '#', R,  11 ), 
            ( 12, 'X', 'X', R,   7 ),
            ( 12, '#', '#', R,  12 ),

            ( 13, 'M', 'B', R,  15 ), -- store the head move (P3)
            ( 13, '#', '#', L,  13 ),
            ( 14, 'M', 'A', R,  15 ),
            ( 14, '#', '#', L,  14 ),
            ( 15, 'A', '0', R,  15 ),
            ( 15, 'B', '1', R,  15 ),
            ( 15, 'X', 'X', R,  16 ),
            ( 15, '#', '#', R,  15 ),
            ( 16, '0', '0', L,  17 ),
            ( 16, '1', '1', L,  17 ),
            ( 16, 'Y', 'Y', L,  17 ),
            ( 16, '#', '#', R,  16 ),
            ( 17, 'A', '0', L,  17 ),
            ( 17, 'B', '1', L,  17 ),
            ( 17, '0', 'S', L,  18 ), 
            ( 17, '1', 'S', L,  19 ), 
            ( 17, '#', '#', L,  17 ),
-------------------------------------------------
            ( 18, 'B', '0', R,  20 ), -- write the new input (P4)
            ( 18, 'A', '0', L,  21 ),
            ( 18, '#', '#', L,  18 ),
            ( 19, 'B', '1', R,  20 ),
            ( 19, 'A', '1', L,  21 ),
            ( 19, '#', '#', L,  19 ),
            ( 20, '0', 'M', R,  22 ),
            ( 20, '1', 'M', R,  23 ),
            ( 20, '#', '#', R,  20 ),
            ( 21, '0', 'M', R,  22 ),
            ( 21, '1', 'M', R,  23 ),
            ( 21, '#', '#', L,  21 ),
            ( 22, 'S', 'A', L,   1 ),
            ( 22, '#', '#', R,  22 ),
            ( 23, 'S', 'B', L,   1 ),
            ( 23, '#', '#', R,  23 )
          ]

-- test of the universal TM

turingTape = start (("_0110M1001Y011",'1',"X111001000X011101100Y_"), 1) universal_TM_table
