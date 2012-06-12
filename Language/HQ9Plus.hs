{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
-- |
-- Module       : Acme.HQ9Plus
-- License      : Public domain
-- Maintainer   : joeyadams3.14159@gmail.com
-- Portability  : FlexibleInstances, GADTs
--
-- An embedded DSL for the HQ9+ programming language.  For more information, see
-- <http://esolangs.org/wiki/HQ9>.
module Acme.HQ9Plus (
    -- * The HQ monad
    HQ,
    runHQ,

    -- * Operations
    h, q, nine, plus,
    (+),

    module Control.Applicative,
) where

import Prelude hiding ((+))
import qualified Prelude as P

import Control.Applicative      (Applicative(pure, (<*>)))
import Control.Monad            (ap, liftM)
import Data.List                (intercalate)
import Unsafe.Coerce            (unsafeCoerce)

------------------------------------------------------------------------
-- Frontend API

data HQ a where
    H      :: HQ ()
    Q      :: HQ ()
    Nine   :: HQ ()
    Plus   :: HQ ()

    Bind   :: HQ a -> (a -> HQ b) -> HQ b
    Return :: a -> HQ a

    N      :: Integer -> HQ ()

instance Functor HQ where
    fmap = liftM

instance Applicative HQ where
    pure  = return
    (<*>) = ap

instance Monad HQ where
    m >>= k  = Bind m k
    return a = Return a

-- | Run an 'HQ' computation.
runHQ :: HQ a -> IO a
runHQ hq = do
    let (a, output) = runHQEx showSource hq
    putStr output
    return a

runHQEx :: ([String] -> String) -> HQ a -> (a, String)
runHQEx f hq =
    let (x, _acc, insts, output) = runHQM (interpret hq) (f $ insts []) 0
     in (x, output [])

-- | Print @\"Hello, world!\"@
h :: HQ ()
h = H

-- | Print the program's source code.
q :: HQ ()
q = Q

-- | Print the lyrics to \"99 Bottles of Beer\".  May also be written as @9@.
nine :: HQ ()
nine = Nine

-- | Increment the accumulator.
plus :: HQ ()
plus = Plus

-- | An infix alias for 'plus'.
(+) :: HQ ()
(+) = Plus

-- This ought to be Num (HQ ()) instead of Num (HQ a), but then GHC's instance
-- resolution won't pick it up when you say e.g. runHQ 9
instance Num (HQ a) where
    (N 0) + x = x
    x + (N 0) = x
    a + b = fromInteger $ hqToInteger a P.+ hqToInteger b

    (N 1) * x = x
    x * (N 1) = x
    a * b = fromInteger $ hqToInteger a * hqToInteger b

    x - (N 0) = x
    a - b = fromInteger $ hqToInteger a - hqToInteger b

    negate n = fromInteger $ negate $ hqToInteger n
    abs    n = fromInteger $ abs    $ hqToInteger n
    signum n = fromInteger $ signum $ hqToInteger n

    fromInteger n = unsafeCoerce (N n)

hqToInteger :: HQ a -> Integer
hqToInteger Nine  = 9
hqToInteger (N n) = n
hqToInteger _     = error "Invalid use of arithmetic in HQ9+"

------------------------------------------------------------------------
-- The HQM monad

newtype HQM a = HQM { runHQM :: String          -- The program's source code
                             -> Int             -- The accumulator

                             -> ( a             -- Return value
                                , Int           -- New state of the accumulator
                                , DList String  -- List of instructions
                                , DList Char    -- Output
                                )
                    }

type DList a = [a] -> [a]

instance Monad HQM where
    return x = HQM $ \_ acc -> (x, acc, id, id)
    m >>= k = HQM $ \src acc0 ->
        let (x1, acc1, insts1, out1) = runHQM m      src acc0
            (x2, acc2, insts2, out2) = runHQM (k x1) src acc1
         in (x2, acc2, insts1 . insts2, out1 . out2)

increment :: HQM ()
increment = HQM $ \_ acc -> ((), acc P.+ 1, id, id)

tellInst :: String -> HQM ()
tellInst inst = HQM $ \_ acc -> ((), acc, (inst :), id)

tellLine :: String -> HQM ()
tellLine line = HQM $ \_ acc -> ((), acc, id, (line ++) . ('\n' :))

tellLines :: [String] -> HQM ()
tellLines xs = HQM $ \_ acc -> ((), acc, id, (unlines xs ++))

tellSource :: HQM ()
tellSource = HQM $ \src acc -> ((), acc, id, (src ++))

showSource :: [String] -> String
showSource xs =
    unlines $
    "import Acme.HQ9Plus" :
    case xs of
        []  -> ["main=runHQ$pure()"]
        [x] -> ["main=runHQ " ++ x]
        _    | length xs < 4
            -> ["main=runHQ$" ++ intercalate ">>" xs]
             | length (filter (== "plus") xs) < 25
            -> ["main=runHQ$do " ++ intercalate ";" xs]
             | otherwise
            -> [ "import Prelude hiding(+)"
               , "main=runHQ$do " ++ intercalate ";" (map shortenPlus xs)
               ]
               where shortenPlus "plus" = "(+)"
                     shortenPlus x      = x

------------------------------------------------------------------------
-- The interpreter

interpret :: HQ a -> HQM a

interpret H = do
    tellInst "h"
    tellLine "Hello, world!"

interpret Q = do
    tellInst "q"
    tellSource

interpret Nine = do
    tellInst "9"
    tellLines $ verse 99
  where
    verse :: Int -> [String]
    verse n =
        [ bottles n ++ " of beer on the wall,"
        , bottles n ++ " of beer."
        ] ++
        if n > 0 then 
            [ "Take one down, pass it around,"
            , bottles (n-1) ++ " of beer on the wall."
            , ""
            ] ++ verse (n-1)
        else
            [ "Go to the store, buy some more,"
            , "99 bottles of beer on the wall."
            ]
    bottles 0 = "No bottles"
    bottles 1 = "1 bottle"
    bottles n = show n ++ " bottles"

interpret Plus = do
    tellInst "plus"
    increment

interpret (Bind m k) = do
    interpret m >>= interpret . k

interpret (Return a) = do
    return a

interpret (N 9) = do
    interpret Nine

interpret (N x) = do
    error $ show x ++ " is not a valid HQ9+ instruction"
