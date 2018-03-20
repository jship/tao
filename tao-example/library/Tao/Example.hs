{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Tao.Example
  ( -- * Example Usage
    -- $intro

    -- ** Unit tests
    unitTests

    -- ** Type synonyms (for readability)
  , OneToFour
  , OneTwo
  , ThreeFour
  , Empty
  ) where

import Tao.Example.List (type Take, type Drop, type Chunk)

import Data.Proxy (Proxy(Proxy))
import Tao (type (@<>), type (@=?), type AssertAll)

type OneToFour = '[1, 2, 3, 4]
type OneTwo    = '[1, 2]
type ThreeFour = '[3, 4]
type Empty     = '[]

-- | Implementation:
--
-- @
-- unitTests :: Proxy '()
-- unitTests = Proxy :: Proxy (AssertAll
--   '[ \"Take\" \@<> OneTwo \@=? Take 2 OneToFour
--    , "Take (none)" \@<> Empty \@=? Take 0 OneToFour
--    , "Take (more than length)" \@<> OneToFour \@=? Take 8 OneToFour
--
--    , \"Drop\" \@<> ThreeFour \@=? Drop 2 OneToFour
--    , "Drop (none)" \@<> OneToFour \@=? Drop 0 OneToFour
--    , "Drop (more than length)" \@<> Empty \@=? Drop 8 OneToFour
--
--    , \"Chunk\" \@<> '[OneTwo, ThreeFour] \@=? Chunk 2 OneToFour
--    , "Chunk (none)" \@<> Empty \@=? Chunk 0 OneToFour
--    , "Chunk (more than length)" \@<> '[OneToFour] \@=? Chunk 8 OneToFour
--    , "Chunk (uneven groups)" \@<> '[ '[1, 2, 3], '[4] ] \@=? Chunk 3 OneToFour
--    ])
-- @
unitTests :: Proxy '()
unitTests = Proxy :: Proxy (AssertAll
  '[ "Take" @<> OneTwo @=? Take 2 OneToFour
   , "Take (none)" @<> Empty @=? Take 0 OneToFour
   , "Take (more than length)" @<> OneToFour @=? Take 8 OneToFour

   , "Drop" @<> ThreeFour @=? Drop 2 OneToFour
   , "Drop (none)" @<> OneToFour @=? Drop 0 OneToFour
   , "Drop (more than length)" @<> Empty @=? Drop 8 OneToFour

   , "Chunk" @<> '[OneTwo, ThreeFour] @=? Chunk 2 OneToFour
   , "Chunk (none)" @<> Empty @=? Chunk 0 OneToFour
   , "Chunk (more than length)" @<> '[OneToFour] @=? Chunk 8 OneToFour
   , "Chunk (uneven groups)" @<> '[ '[1, 2, 3], '[4] ] @=? Chunk 3 OneToFour
   ])

{- $intro

@tao-example@ provides example usages of the @tao@ package to test some slightly more complicated type-level computations than what is shown the the @tao@ package's Getting Started section. @tao-example@ tests three different type functions: 'Take', 'Drop', and 'Chunk'. The implementations of these type functions are available in @Tao.Example.List@.

* 'Take' grabs the first @n@ elements from a type-level list, where @n@ is a 'GHC.TypeNats.Nat'. This is similar to the value-level 'take' function.
* 'Drop' discards the first @n@ elements from a type-level list, where @n@ is a 'GHC.TypeNats.Nat'. This is similar to the value-level 'drop' function.
* 'Chunk' groups the type-level list into sub-lists each of size @n@, where @n@ is a 'GHC.TypeNats.Nat'.

-}
