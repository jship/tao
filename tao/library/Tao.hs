{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Tao
  ( -- * Introduction
    -- $intro

    -- ** Getting Started
    -- $gettingStarted

    -- ** Assertions as type operators
    type (@=?)
  , type (@?=)
  , type (@<>)
  , type (<>@)
  , type (@<>?)
  , type (?<>@)

    -- ** Assertions as type functions
  , type AssertBool
  , type AssertEq
  , type AssertEq'

    -- ** Folding lists of assertions
  , type AssertAll
  ) where

import Data.Type.Equality (type (==))
import GHC.TypeLits (ErrorMessage((:<>:), ShowType, Text), Symbol, type TypeError)

-- | Asserts that the expected type is equal to the actual type.
--
-- Roughly an alias for 'AssertEq', but as an operator.
type family (expected :: (Symbol, k)) @=? (actual :: k) :: () where
  '(m, e) @=? a = AssertEq m e a

-- | Asserts that the actual type is equal to the expected type.
--
-- Roughly an alias for 'AssertEq', but as an operator.
type family (actual :: k) @?= (expected :: (Symbol, k)) :: () where
  a @?= '(m, e) = AssertEq m e a

-- | Pairs an assertion message with an expected type.
type family (msg :: Symbol) @<> (expected :: k) :: (Symbol, k) where
  m @<> e = '(m, e)

-- | Pairs an assertion message with an expected type. Flipped version of
-- '@<>'.
type family (expected :: k) <>@ (msg :: Symbol) :: (Symbol, k) where
  e <>@ m = '(m, e)

-- | Operator version of 'AssertBool'.
type family (msg :: Symbol) @<>? (result :: Bool) :: () where
  m @<>? r = AssertBool m r

-- | Flipped, operator version of 'AssertBool'.
type family (result :: Bool) ?<>@ (msg :: Symbol) :: () where
  r ?<>@ m = AssertBool m r

-- | Asserts that the expected type is equal to the actual type. Returns unit
-- when the types are equal and produces a type error otherwise. The input
-- 'Symbol' is a message that is displayed as part of the type error.
type family AssertEq (msg :: Symbol) (expected :: k) (actual :: k) :: () where
  AssertEq m e a = AssertEq' m e a (e == a)

-- | Helper for the implementation of 'AssertEq'. Users should never need
-- to use this directly.
type family AssertEq' (msg :: Symbol) (expected :: k) (actual :: k) (result :: Bool) :: () where
  AssertEq' m _ _ 'True  = '()
  AssertEq' m e a 'False = TypeError ( 'Text m
                                 ':<>: 'Text ": expected ("
                                 ':<>: 'ShowType e
                                 ':<>: 'Text "), actual ("
                                 ':<>: 'ShowType a
                                 ':<>: 'Text ")"
                                     )

-- | Asserts that the type-level condition is true. Returns unit when the
-- condition is true and produces a type error otherwise. The input 'Symbol'
-- is a message that is displayed as part of the type error.
type family AssertBool (msg :: Symbol) (condition :: Bool) :: () where
  AssertBool m 'True  = '()
  AssertBool m 'False = TypeError ('Text m ':<>: 'Text ": condition was false")

-- | Helper function to fold a bunch of type-level units down to a single unit.
-- This is convenient for squashing down a type-level list of assert results.
type family AssertAll (xs :: [()]) :: () where
  AssertAll '[] = '()
  AssertAll ('() ': xs) = AssertAll xs

infix 1 @=?
infix 1 @?=
infix 2 @<>
infix 2 <>@
infix 2 @<>?
infix 2 ?<>@

{- $intro

@tao@ provides type-level assertion operators/functions and only depends on
@base@. The operators are influenced by the [HUnit](https://github.com/hspec/HUnit#readme)
unit testing library. Using this library should feel somewhat similar to unit testing,
but at the type-level!

The [Getting Started](#start) section walks through general usage.

-}

{- $gettingStarted #start#

We will start with a minimal example. We want to confirm that the promoted
'True' type is equal to the promoted 'True' type.

@
unitTest :: Proxy '()
unitTest = Proxy :: Proxy (AssertEq "True is True" 'True 'True)
@

This function's type is a proxy containing promoted unit. In the body of
the function, we have annotated the 'Proxy' value we are constructing using
'AssertEq', and 'AssertEq' produces the promoted unit type. This is where
we are specifying our test. We give 'AssertEq' three things:

* a 'Symbol' that is used as a message on assertion failure
* an expected type
* the actual type

If the expected type and actual type are equal, the test passes and our code compiles! What if the types are not equal?

@
unitTest :: Proxy '()
unitTest = Proxy :: Proxy (AssertEq "True is True" 'True 'False)
@

Then the compiler will tell us something is very wrong:

@
    * True is True: expected ('True), actual ('False)
    * In the expression:
          Proxy :: Proxy (AssertEq "True is True"  'True  'False)
      In an equation for ‘unitTest’:
          unitTest = Proxy :: Proxy (AssertEq "True is True"  'True  'False)
@

Testing that true is true is not super useful. We could do something instead
like:

@
unitTest :: Proxy '()
unitTest = Proxy :: Proxy (AssertEq "3 = 1 + 2" 3 (1 + 2))
@

In the above we have specified that @3@ is equal to @1 + 2@ using type-level
natural numbers.

What if we want to have more than one test? We could make multiple proxies:

@
testAddingNats :: Proxy '()
testAddingNats = Proxy :: Proxy (AssertEq "3 = 1 + 2" 3 (1 + 2))

upFromHowManyChambers :: Proxy '()
upFromHowManyChambers = Proxy :: Proxy (AssertEq "36 = 6 * 6" 36 (6 * 6))
@

This works, but it is fairly heavy-handed. It would be nice to instead just
stuff all of our assertions in a type-level list.  A first attempt might
look like this:

@
unitTests :: Proxy '[ '(), '() ]
unitTests = Proxy :: Proxy
  '[ AssertEq  "3 = 1 + 2"  3 (1 + 2)
   , AssertEq "36 = 6 * 6" 36 (6 * 6)
   ]
@

This is an improvement, but now we have to add a promoted unit to the
type-level list in @unitTest@'s type signature every time we add an assertion.

A helper called 'AssertAll' is provided for this case:

@
unitTests :: Proxy '()
unitTests = Proxy :: Proxy (AssertAll
  '[ AssertEq  "3 = 1 + 2"  3 (1 + 2)
   , AssertEq "36 = 6 * 6" 36 (6 * 6)
   ])
@

'AssertAll' will squash down all the result units from assertions into a single
unit. This way, we can keep on adding assertions and never have to fuss with
@unitTest@'s type signature along the way.

So far, we have used 'AssertEq' in all of the above examples. @tao@ provides
@HUnit@-like operators too:

@
unitTests :: Proxy '()
unitTests = Proxy :: Proxy (AssertAll
  '[  "3 = 1 + 2" \@<>  3 \@=? 1 + 2
   , "36 = 6 * 6" \@<> 36 \@=? 6 * 6
   ])
@

In the above, we used '@=?'  as a rough alias for 'AssertEq'. We attached an
assertion message using '@<>'. Using the operators can be nice when we want to
cut down on parentheses.

If we prefer to put our expected types and assertion
messages to the right, we could write this equivalently as:

@
unitTests :: Proxy '()
unitTests = Proxy :: Proxy (AssertAll
  '[ 1 + 2 \@?=  3 <>\@  "3 = 1 + 2"
   , 6 * 6 \@?= 36 <>\@ "36 = 6 * 6"
   ])
@

Woo - now we know how to use @tao@! As a next step, we can take a look at
the [tao-example](https://hackage.haskell.org/package/tao-example) repo to
see @tao@ being used to test a few slightly more complicated type-level
computations.

-}
