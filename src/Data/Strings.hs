{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Strings where

import           Data.Function              (($))
import           Data.Monoid                ((<>))
import           Data.String                (String)
import           System.IO                  (IO, print)

import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as CB
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Lazy.Char8 as CLB
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as LT

class StringInferencer a where
  string :: a -> String

instance StringInferencer String where
  string x = x

class BinaryOperation l r a where
  (++) :: l -> r -> a

instance {-# INCOHERENT #-} BinaryOperation B.ByteString B.ByteString B.ByteString where
  (++) l r = l <> r

instance {-# INCOHERENT #-} (StringInferencer l) => BinaryOperation l B.ByteString B.ByteString where
  (++) l r = CB.pack (string l) <> r

instance {-# INCOHERENT #-} (StringInferencer l, StringInferencer r) => BinaryOperation l r String where
  (++) l r = string l <> string r
