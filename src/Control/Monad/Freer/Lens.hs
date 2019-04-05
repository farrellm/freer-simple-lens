{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}

module Control.Monad.Freer.Lens
  ( view
  , views
  , use
  , uses
  , assign
  , (.=)
  , modifying
  , (%=)
  ) where

import Control.Monad.Freer
import Control.Monad.Freer.Reader (Reader)
import qualified Control.Monad.Freer.Reader as Reader
import Control.Monad.Freer.State (State)
import qualified Control.Monad.Freer.State as State
import qualified Control.Lens as Lens
import Control.Lens.Getter (Getting)
import Control.Lens.Setter (ASetter)

-- | View the value pointed to by a 'Control.Lens.Getter.Getter',
-- 'Control.Lens.Iso.Iso' or 'Control.Lens.Lens.Lens' or the result of
-- folding over all the results of a 'Control.Lens.Fold.Fold' or
-- 'Control.Lens.Traversal.Traversal' corresponding to the 'Reader'
-- context of the given monadic carrier.
view :: forall r a effs . (Member (Reader r) effs) => Getting a r a -> Eff effs a
view l = Reader.asks (Lens.view l)
{-# INLINE view #-}

-- | View a function of the value pointed to by a
-- 'Control.Lens.Getter.Getter', 'Control.Lens.Iso.Iso' or
-- 'Control.Lens.Lens.Lens' or the result of folding over all the
-- results of a 'Control.Lens.Fold.Fold' or
-- 'Control.Lens.Traversal.Traversal' corresponding to the 'Reader'
-- context of the given monadic carrier.
--
-- This is slightly more general in lens itself, but should suffice for our purposes.
views :: forall s a b effs . (Member (Reader s) effs) => Getting a s a -> (a -> b) -> Eff effs b
views l f = fmap f (Reader.asks (Lens.view l))
{-# INLINE views #-}

-- | Extract the target of a 'Control.Lens.Lens',
-- 'Control.Lens.Iso.Iso', or 'Control.Lens.Getter.Getter' from the,
-- or use a summary of a 'Control.Lens.Fold.Fold' or
-- 'Control.Lens.Traversal.Traversal' that points to a monoidal value.
use :: forall s a effs . (Member (State s) effs) => Getting a s a -> Eff effs a
use l = State.gets (Lens.view l)
{-# INLINE use #-}

-- | Use a function of the target of a 'Control.Lens.Lens.Lens',
-- 'Control.Lens.Iso.Iso', or 'Control.Lens.Getter.Getter' in the
-- current state, or use a summary of a 'Control.Lens.Fold.Fold' or
-- 'Control.Lens.Traversal.Traversal' that points to a monoidal value.
uses :: forall s a b effs . (Member (State s) effs) => Getting a s a -> (a -> b) -> Eff effs b
uses l f = fmap f (State.gets (Lens.view l))
{-# INLINE uses #-}

-- | Replace the target of a 'Control.Lens.Lens.Lens' or all of the
-- targets of a 'Control.Lens.Setter.Setter' or
-- 'Control.Lens.Traversal.Traversal' in our monadic state with a new
-- value, irrespective of the old.
--
-- This is a prefix version of '.='.
assign :: forall s a b effs . (Member (State s) effs) => ASetter s s a b -> b -> Eff effs ()
assign l b = State.modify (Lens.set l b)
{-# INLINE assign #-}

-- | Replace the target of a 'Control.Lens.Lens.Lens' or all of the
-- targets of a 'Control.Lens.Setter.Setter' or
-- 'Control.Lens.Traversal.Traversal' in our monadic state with a new
-- value, irrespective of the old.
--
-- This is an infix version of 'assign'.
infixr 4 .=
(.=) :: forall s a b effs . (Member (State s) effs) => ASetter s s a b -> b -> Eff effs ()
(.=) = assign
{-# INLINE (.=) #-}

-- | Map over the target of a 'Control.Lens.Lens.Lens' or all of the
-- targets of a 'Control.Lens.Setter.Setter' or
-- 'Control.Lens.Traversal.Traversal' in our monadic state.
--
-- This is a prefix version of '%='.
modifying :: forall s a b effs . (Member (State s) effs) => ASetter s s a b -> (a -> b) -> Eff effs ()
modifying l f = State.modify (Lens.over l f)
{-# INLINE modifying #-}

-- | Map over the target of a 'Control.Lens.Lens.Lens' or all of the
-- targets of a 'Control.Lens.Setter.Setter' or
-- 'Control.Lens.Traversal.Traversal' in our monadic state.
--
-- This is an infix version of 'modifying'.
infixr 4 %=
(%=) :: forall s a b effs . (Member (State s) effs) => ASetter s s a b -> (a -> b) -> Eff effs ()
(%=) = modifying
{-# INLINE (%=) #-}
