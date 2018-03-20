{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Tao.Example.List
  ( type Take
  , type Drop
  , type Chunk
  ) where

import GHC.TypeNats (type (-), Nat)

type family Take (n :: Nat) (xs :: [k]) where
  Take _ '[]       = '[]
  Take 0 _         = '[]
  Take i (x ': xs) = x ': Take (i - 1) xs

type family Drop (n :: Nat) (xs :: [k]) where
  Drop _ '[]       = '[]
  Drop 0 xs        = xs
  Drop i (_ ': xs) = Drop (i - 1) xs

type family Chunk (n :: Nat) (xs :: [k]) where
  Chunk _ '[] = '[]
  Chunk 0 _   = '[]
  Chunk s xs  = Take s xs ': Chunk s (Drop s xs)
