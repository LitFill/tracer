{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Fmt
import Data.Kind (Type)

data TraceData :: Type where
    TraceData :: forall a b.
        (Show a, Show b, Buildable a, Buildable b) =>
        { tracemsg    :: String
        , tracearg    :: a
        , traceoutput :: b
        , getresult   :: String
        } -> TraceData

instance Buildable TraceData where
    build :: TraceData -> Builder
    build (TraceData msg arg output _) =
        "TraceData:\n\tmessage: " +| msg |+
            "\n\targs: " +| arg |+
            "\n\toutput: " +| output |+ "\n"

instance Show TraceData where
    show = fmt . build

mkTraceData
    :: (Show a, Show b, Buildable a, Buildable b)
    => String -> a -> b -> TraceData
mkTraceData msg arg output = TraceData msg arg output (show output)

sample :: TraceData
sample = mkTraceData @Int @Int "sample" 1 6

newtype Trace :: Type where
    Trace :: [TraceData] -> Trace

instance Semigroup Trace where
    (Trace a) <> (Trace b) = Trace (a <> b)

instance Monoid Trace where
    mempty = Trace []

infixr 6 **>
(**>) :: TraceData -> Trace -> Trace
x **> (Trace xs) = Trace (x : xs)

traceCall
    :: (Show a, Show b, Buildable a, Buildable b)
    => String
    -> (a -> (Trace, b))
    -> a
    -> (Trace, b)
traceCall msg fn x =
    ((mkTraceData msg x y) **> t1, y)
        where (t1, y) = fn x

showTrace :: Trace -> String
showTrace (Trace [])    = mempty
showTrace (Trace tdata@(t1:_)) =
    let depth = length tdata
        (TraceData _ _ _ result) = t1
        tdata' = zip @Int [1..] tdata
     in ("stack depth: "+| depth |+"\nfactor")

instance Show Trace where show = showTrace

instance (Buildable a, Buildable b) => Buildable (a, b) where
    build (a, b) = "("+| a |+", "+| b |+")"

factor :: Int -> (Trace, [Int])
factor n = traceCall "factor" go (n, 2)
  where
    go (1, _) = mempty
    go (num, curFact)
        | num `mod` curFact == 0 =
            let nextNumber       = num `div` curFact
                message          = "consFactor " <> show curFact
                (traced, result) = traceCall message go (nextNumber, curFact)
             in (traced, curFact : result)
        | otherwise =
            traceCall "skipFactor" go (num, curFact + 1)
verboseFactor :: Int -> IO ()
verboseFactor n = do
    let (traced, factors) = factor n
    putStrLn "factor: "
    print factors
    putStrLn "trace: "
    print traced

main :: IO ()
main = do
    fmtLn $ "hello, " +| (["LitFill"] :: [String]) |+ "! ready to trace?"
