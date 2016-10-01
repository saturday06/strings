{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Strings where

import           Data.Function              (($))
import           Data.Monoid                ((<>))
import           Data.String                (String)

import qualified Data.ByteString            as B
import qualified Data.ByteString.Builder    as BB
import qualified Data.ByteString.Char8      as CB
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Lazy.Char8 as CLB
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as E
import qualified Data.Text.Lazy             as LT
import qualified Data.Text.Lazy.Encoding    as LE

class StringLiteralInferencer a where string :: a -> String
instance StringLiteralInferencer String where string x = x

class BinaryOperation l r a where (++) :: l -> r -> a

-- | You can append string variants.
-- >>> ("foo" ++ "bar") :: String
-- "foobar"
instance {-# INCOHERENT #-} BinaryOperation                                 B.ByteString  B.ByteString  B.ByteString  where (++) l r = l <> r
instance {-# INCOHERENT #-} BinaryOperation                                 B.ByteString  LB.ByteString B.ByteString  where (++) l r = l ++ LB.toStrict r
instance {-# INCOHERENT #-} BinaryOperation                                 B.ByteString  T.Text        B.ByteString  where (++) l r = l ++ E.encodeUtf8 r
instance {-# INCOHERENT #-} BinaryOperation                                 B.ByteString  LT.Text       B.ByteString  where (++) l r = l ++ LE.encodeUtf8 r
instance {-# INCOHERENT #-} BinaryOperation                                 B.ByteString  String        B.ByteString  where (++) l r = l ++ BB.toLazyByteString (BB.stringUtf8 r)
instance {-# INCOHERENT #-} (StringLiteralInferencer r) => BinaryOperation  B.ByteString  r             B.ByteString  where (++) l r = l ++ string r
instance {-# INCOHERENT #-} BinaryOperation                                 LB.ByteString B.ByteString  B.ByteString  where (++) l r = LB.toStrict l ++ r
instance {-# INCOHERENT #-} BinaryOperation                                 LB.ByteString LB.ByteString LB.ByteString where (++) l r = l <> r
instance {-# INCOHERENT #-} BinaryOperation                                 LB.ByteString T.Text        LB.ByteString where (++) l r = l ++ LT.fromStrict r
instance {-# INCOHERENT #-} BinaryOperation                                 LB.ByteString LT.Text       LB.ByteString where (++) l r = l ++ LE.encodeUtf8 r
instance {-# INCOHERENT #-} BinaryOperation                                 LB.ByteString String        LB.ByteString where (++) l r = l ++ BB.toLazyByteString (BB.stringUtf8 r)
instance {-# INCOHERENT #-} (StringLiteralInferencer r) => BinaryOperation  LB.ByteString r             LB.ByteString where (++) l r = l ++ string r
instance {-# INCOHERENT #-} BinaryOperation                                 T.Text        B.ByteString  B.ByteString  where (++) l r = E.encodeUtf8 l ++ r
instance {-# INCOHERENT #-} BinaryOperation                                 T.Text        LB.ByteString LB.ByteString where (++) l r = LT.fromStrict l ++ r
instance {-# INCOHERENT #-} BinaryOperation                                 T.Text        T.Text        T.Text        where (++) l r = l <> r
instance {-# INCOHERENT #-} BinaryOperation                                 T.Text        LT.Text       T.Text        where (++) l r = l ++ LT.toStrict r
instance {-# INCOHERENT #-} BinaryOperation                                 T.Text        String        T.Text        where (++) l r = l ++ T.pack r
instance {-# INCOHERENT #-} (StringLiteralInferencer r) => BinaryOperation  T.Text        r             T.Text        where (++) l r = l ++ string r
instance {-# INCOHERENT #-} BinaryOperation                                 LT.Text       B.ByteString  B.ByteString  where (++) l r = LE.encodeUtf8 l ++ r
instance {-# INCOHERENT #-} BinaryOperation                                 LT.Text       LB.ByteString LB.ByteString where (++) l r = LE.encodeUtf8 l ++ r
instance {-# INCOHERENT #-} BinaryOperation                                 LT.Text       T.Text        T.Text        where (++) l r = LT.toStrict l <> r
instance {-# INCOHERENT #-} BinaryOperation                                 LT.Text       LT.Text       LT.Text       where (++) l r = l <> r
instance {-# INCOHERENT #-} BinaryOperation                                 LT.Text       String        LT.Text       where (++) l r = l ++ LT.pack r
instance {-# INCOHERENT #-} (StringLiteralInferencer r) => BinaryOperation  LT.Text       r             LT.Text       where (++) l r = l ++ string r
instance {-# INCOHERENT #-} BinaryOperation                                 String        B.ByteString  B.ByteString  where (++) l r = BB.toLazyByteString (BB.stringUtf8 l) ++ r
instance {-# INCOHERENT #-} BinaryOperation                                 String        LB.ByteString LB.ByteString where (++) l r = BB.toLazyByteString (BB.stringUtf8 l) ++ r
instance {-# INCOHERENT #-} BinaryOperation                                 String        T.Text        T.Text        where (++) l r = T.pack l <> r
instance {-# INCOHERENT #-} BinaryOperation                                 String        LT.Text       LT.Text       where (++) l r = LT.pack l <> r
instance {-# INCOHERENT #-} BinaryOperation                                 String        String        String        where (++) l r = l <> r
instance {-# INCOHERENT #-} (StringLiteralInferencer r) => BinaryOperation  String        r             String        where (++) l r = l <> string r
instance {-# INCOHERENT #-} (StringLiteralInferencer l) => BinaryOperation  l             B.ByteString  B.ByteString  where (++) l r = string l ++ r
instance {-# INCOHERENT #-} (StringLiteralInferencer l) => BinaryOperation  l             LB.ByteString LB.ByteString where (++) l r = string l ++ r
instance {-# INCOHERENT #-} (StringLiteralInferencer l) => BinaryOperation  l             BB.Builder    LB.ByteString where (++) l r = string l ++ r
instance {-# INCOHERENT #-} (StringLiteralInferencer l) => BinaryOperation  l             T.Text        T.Text        where (++) l r = string l ++ r
instance {-# INCOHERENT #-} (StringLiteralInferencer l) => BinaryOperation  l             LT.Text       LT.Text       where (++) l r = string l ++ r
instance {-# INCOHERENT #-} (StringLiteralInferencer l) => BinaryOperation  l             String        String        where (++) l r = string l ++ r
instance {-# INCOHERENT #-} (StringLiteralInferencer l, StringLiteralInferencer r) => BinaryOperation l r String      where (++) l r = string l <> string r
