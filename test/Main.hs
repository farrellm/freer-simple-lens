{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Control.Monad.Freer.State
import Control.Lens.Wrapped
import Control.Lens.TH
import Test.Hspec

import Control.Monad.Freer.Lens

data Context = Context
  { _amount :: Int
  , _sweatshirt :: Bool
  } deriving (Eq, Show)

initial :: Context
initial = Context 0 False

makeLenses ''Context

stateTest :: (Member (State Context) effs) => Eff effs Int
stateTest = do
  initial <- use amount
  assign amount (initial + 1)
  assign sweatshirt True
  use amount

newtype Foo = Foo { _unFoo :: Int } deriving (Eq, Show)

makeWrapped ''Foo

newtype Bar = Bar { _unBar :: Float } deriving (Eq, Show)

makeWrapped ''Bar

doubleStateTest :: (Member (State Bar) effs, Member (State Foo) effs) => Eff effs Int
doubleStateTest = do
  assign @Foo _Wrapped 5
  assign @Bar _Wrapped 30.5
  pure 50

readerTest :: (Member (Reader Context) effs) => Eff effs Int
readerTest = succ <$> view amount

spec :: Spec
spec = describe "use/assign" $ do
  it "should modify stateful variables" $ do
    let result = run $ runState initial stateTest
    result `shouldBe` (1, Context 1 True)

  it "works in the presence of polymorphic lenses with -XTypeAnnotations" $ do
    let result = run $ runState (Bar 5) $ runState (Foo 500) doubleStateTest
    result `shouldBe` ((50, Foo 5), Bar 30.5)

  it "should read from an environment" $ do
    let result = run $ runReader initial readerTest
    result `shouldBe` 1

main :: IO ()
main = hspec $ describe "Control.Monad.Freer.Lens" spec
