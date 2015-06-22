{-# OPTIONS_GHC -Wall #-}

module Content
  ( pWord8
  , pWord16
  , pWord32
  ) where

import qualified Data.Word              as W
import qualified Koshucode.Baala.Core   as K

pWord8 :: (K.CContent c) => W.Word8 -> c
pWord8 i = K.pDecFromInt i' where
    i' = fromIntegral i :: Int

pWord16 :: (K.CContent c) => W.Word16 -> c
pWord16 i = K.pDecFromInt i' where
    i' = fromIntegral i :: Int

pWord32 :: (K.CContent c) => W.Word32 -> c
pWord32 i = K.pDecFromInt i' where
    i' = fromIntegral i :: Int

