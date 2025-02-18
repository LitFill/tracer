{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Fmt
    (
        (+|), (|+),
        fmt, fmtLn,
        indentF,
        Buildable(..),
        Builder
    )

import Data.Kind        (Type)
import Data.Traversable (forM)

data TraceData :: Type where
    TraceData :: forall a b.
        (Show a, Show b) =>
        { tracemsg    :: String
        , tracearg    :: a
        , traceoutput :: b
        , getarg      :: String
        , getresult   :: String
        } -> TraceData

instance Buildable TraceData where
    build :: TraceData -> Builder
    build (TraceData msg _ _ arg output) =
        "TraceData:\n"
        <> "\tmessage: "+| msg    |+"\n"
        <> "\targs: "   +| arg    |+"\n"
        <> "\toutput: " +| output |+"\n"

instance Show TraceData where
    show = fmt . build

mkTraceData
    :: (Show a, Show b)
    => String -> a -> b -> TraceData
mkTraceData msg arg output =
    TraceData msg arg output (show arg) (show output)

showSimpleTraceData :: TraceData -> String
showSimpleTraceData (TraceData msg _ _ arg res) =
    msg |+" "+| arg |+" => "+| res |+ mempty

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
    :: (Show a, Show b)
    => String
    -> (a -> (Trace, b))
    -> a
    -> (Trace, b)
traceCall msg fn x = (mkTraceData msg x y **> traced, y)
  where (traced, y) = fn x

showTrace :: Trace -> String
showTrace (Trace tdata) =
    let depth = length tdata
        tdata' = zip @Int [0..] tdata
     in "stack depth: "+| depth |+"\n"
        <> build (concat $ forM tdata' withIndent)
    where
        withIndent (indent, traced) =
            [indentF indent . build $ showSimpleTraceData traced]

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
    putStrLn "factors: "
    fmt . indentF 4 $ build factors
    putStrLn "trace: "
    print traced

main :: IO ()
main = do
    fmtLn $ "hello, " +| (["LitFill"] :: [String]) |+ "! ready to trace?"
    verboseFactor 1080
