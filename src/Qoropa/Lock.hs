{-# LANGUAGE CPP, DeriveDataTypeable, NoImplicitPrelude #-}

--------------------------------------------------------------------------------
-- |
-- Module     : Control.Concurrent.Lock
-- Copyright  : (c) 2010 Bas van Dijk & Roel van Dijk
-- License    : BSD3 (see the file LICENSE)
-- Maintainer : Bas van Dijk <v.dijk.bas@gmail.com>
--            , Roel van Dijk <vandijk.roel@gmail.com>
--
-- This module provides the 'Lock' synchronisation mechanism. It was inspired by
-- the Python and Java @Lock@ objects and should behave in a similar way. See:
--
-- <http://docs.python.org/3.1/library/threading.html#lock-objects>
--
-- and:
--
-- <http://java.sun.com/javase/7/docs/api/java/util/concurrent/locks/Lock.html>
--
-- All functions are /exception safe/. Throwing asynchronous exceptions will not
-- compromise the internal state of a 'Lock'.
--
-- This module is intended to be imported qualified. We suggest importing it like:
--
-- @
-- import           Control.Concurrent.Lock         ( Lock )
-- import qualified Control.Concurrent.Lock as Lock ( ... )
-- @
--
--------------------------------------------------------------------------------

module Qoropa.Lock
    ( Lock

      -- * Creating locks
    , new
    , newAcquired

      -- * Locking and unlocking
    , acquire
    , release

      -- * Convenience functions
    , with
    , wait

      -- * Querying locks
    , locked
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Applicative     ( liftA2 )
import Control.Concurrent.MVar ( MVar, newMVar, newEmptyMVar
                               , takeMVar
                               , tryPutMVar
                               , isEmptyMVar
                               )
import Control.Exception       ( block, bracket_ )
import Control.Monad           ( Monad, return, (>>=), (>>), fail, unless )
import Data.Bool               ( Bool )
#ifdef __HADDOCK__
import Data.Bool               ( Bool(False, True) )
#endif
import Data.Eq                 ( Eq )
import Data.Function           ( ($) )
import Data.Typeable           ( Typeable )
import Prelude                 ( error, (.) )
import System.IO               ( IO )

--------------------------------------------------------------------------------
-- Locks
--------------------------------------------------------------------------------

-- | A lock is in one of two states: \"locked\" or \"unlocked\".
newtype Lock = Lock {un :: MVar ()} deriving (Eq, Typeable)


--------------------------------------------------------------------------------
-- Creating locks
--------------------------------------------------------------------------------

-- | Create a lock in the \"unlocked\" state.
new :: IO Lock
new = do
    m <- newMVar ()
    return Lock { un = m }

-- | Create a lock in the \"locked\" state.
newAcquired :: IO Lock
newAcquired = do
    m <- newEmptyMVar
    return Lock { un = m }


--------------------------------------------------------------------------------
-- Locking and unlocking
--------------------------------------------------------------------------------

{-|
Acquires the 'Lock'. Blocks if another thread has acquired the 'Lock'.

@acquire@ behaves as follows:

* When the state is \"unlocked\" @acquire@ changes the state to \"locked\".

* When the state is \"locked\" @acquire@ /blocks/ until a call to 'release' in
another thread wakes the calling thread. Upon awakening it will change the state
to \"locked\".

There are two further important properties of @acquire@:

* @acquire@ is single-wakeup. That is, if there are multiple threads blocked on
@acquire@ and the lock is released, only one thread will be woken up. The
runtime guarantees that the woken thread completes its @acquire@ operation.

* When multiple threads are blocked on @acquire@, they are woken up in FIFO
order. This is useful for providing fairness properties of abstractions built
using locks. (Note that this differs from the Python implementation where the
wake-up order is undefined.)
-}
acquire :: Lock -> IO ()
acquire = takeMVar . un

{-|
@release@ changes the state to \"unlocked\" and returns immediately.

Note that it is an error to release a lock in the \"unlocked\" state!

If there are any threads blocked on 'acquire' the thread that first called
@acquire@ will be woken up.
-}
release :: Lock -> IO ()
release (Lock mv) = do
  b <- tryPutMVar mv ()
  unless b $ error "Control.Concurrent.Lock.release: Can't release unlocked Lock!"


--------------------------------------------------------------------------------
-- Convenience functions
--------------------------------------------------------------------------------

{-|
A convenience function which first acquires the lock and then performs the
computation. When the computation terminates, whether normally or by raising an
exception, the lock is released.

Note that: @with = 'liftA2' 'bracket_' 'acquire' 'release'@.
-}
with :: Lock -> IO a -> IO a
with = liftA2 bracket_ acquire release

{-|
* When the state is \"locked\", @wait@ /blocks/ until a call to 'release' in
another thread changes it to \"unlocked\".

* When the state is \"unlocked\" @wait@ returns immediately.

@wait@ does not alter the state of the lock.

Note that @wait@ is just a convenience function defined as:

@wait l = 'block' '$' 'acquire' l '>>' 'release' l@
-}
wait :: Lock -> IO ()
wait l = block $ acquire l >> release l


--------------------------------------------------------------------------------
-- Querying locks
--------------------------------------------------------------------------------

{-|
Determines if the lock is in the \"locked\" state.

Note that this is only a snapshot of the state. By the time a program reacts
on its result it may already be out of date.
-}
locked :: Lock -> IO Bool
locked = isEmptyMVar . un


-- The End ---------------------------------------------------------------------
