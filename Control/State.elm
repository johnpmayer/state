
{-
    Copyright (c) John P Mayer Jr, 2013

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module Control.State where

{-| -}

type alias State s a = { runState : s -> (a, s) }

get : State s s
get = State <| \s -> (s, s)

put : s -> State s ()
put s' = State <| \s -> ((), s')

evalState : State s a -> s -> a
evalState st = fst << st.runState

execState : State s a -> s -> s
execState st = snd << st.runState

{- Monad Instance -}

returnS : a -> State s a
returnS x = State <| \s -> (x, s)

bindS : State s a -> (a -> State s b) -> State s b
bindS st f = 
  State <| 
    \s -> let (x, s') = st.runState s
          in (f x).runState s'

sequenceS : List (State s a) -> State s (List a)
sequenceS ms = 
  let k m m' = 
    bindS m (\x ->
    bindS m' (\xs ->
    returnS <| x :: xs))
  in List.foldr k (returnS []) ms

mapMS : (a -> State s b) -> List a -> State s (List b)
mapMS f = sequenceS << List.map f

{- Functor Instance (derived) -}

fmapS : (a -> b) -> State s a -> State s b
fmapS f st =
  bindS st (\x ->
  returnS <| f x)

updateS : (a -> a) -> State a ()
updateS f = bindS get (\s -> put <| f s)
