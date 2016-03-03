{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

module Data.Bifunctor.Extended
    ( module Data.Bifunctor.Extended
    , module Data.Bifunctor
    )
  where

import Data.Bifunctor


bimap' :: (Bifunctor p) => (a -> b) -> p a a -> p b b
bimap' a = bimap a a
