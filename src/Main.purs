module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Record (get, insert)
import Global.Unsafe (unsafeStringify)
import Type.Prelude (class IsSymbol, class RowLacks, class RowToList, RLProxy(RLProxy), SProxy(SProxy))
import Type.Row (Cons, Nil, kind RowList)

foldRecord :: forall row xs a b row'
   . RowToList row xs
  => FoldRecord xs row a b row'
  => Monoid b
  => (a -> b)
  -> Record row
  -> b
foldRecord = foldRecordImpl (RLProxy :: RLProxy xs)

class FoldRecord (xs :: RowList) (row :: # Type) a b (row' :: # Type)
  | xs -> row row' a b where
  foldRecordImpl :: Monoid b => RLProxy xs -> (a -> b) -> Record row -> b

instance foldRecordCons ::
  ( IsSymbol name
  , RowCons name a trash row
  , FoldRecord tail row a b tailRow'
  , RowLacks name tailRow'
  , RowCons name b tailRow' row'
  ) => FoldRecord (Cons name a tail) row a b row' where
  foldRecordImpl _ f r =
    val <> rest
    where
      nameP = SProxy :: SProxy name
      val = f $ get nameP r
      rest = foldRecordImpl (RLProxy :: RLProxy tail) f r

instance foldRecordNil :: FoldRecord Nil row a b () where
  foldRecordImpl _ _ _ = mempty

traverseRecord :: forall m row xs a b row'
   . RowToList row xs
  => TraverseRecord xs row a b row'
  => Applicative m
  => (a -> m b)
  -> Record row
  -> m (Record row')
traverseRecord = traverseRecordImpl (RLProxy :: RLProxy xs)

class TraverseRecord (xs :: RowList) (row :: # Type) a b (row' :: # Type)
  | xs -> row row' a b where
  traverseRecordImpl :: forall m . Applicative m => RLProxy xs -> (a -> m b) -> Record row -> m (Record row')

instance traverseRecordCons ::
  ( IsSymbol name
  , RowCons name a trash row
  , TraverseRecord tail row a b tailRow'
  , RowLacks name tailRow'
  , RowCons name b tailRow' row'
  ) => TraverseRecord (Cons name a tail) row a b row' where
  traverseRecordImpl _ f r =
    (\v r -> insert nameP v r) <$> val <*> rest
    where
      nameP = SProxy :: SProxy name
      val = f $ get nameP r -- m b
      rest = traverseRecordImpl (RLProxy :: RLProxy tail) f r

instance traverseRecordNil :: TraverseRecord Nil row a b () where
  traverseRecordImpl _ _ _ = pure {}

class SequenceMaybeRecord rl row row'
  | rl -> row row'
  where
    sequenceMaybeRecordImpl :: RLProxy rl -> Record row -> Maybe (Record row')

instance sequenceMaybeRecordCons ::
  ( IsSymbol name
  , RowCons name (Maybe ty) trash row
  , SequenceMaybeRecord tail row tailRow'
  , RowLacks name tailRow'
  , RowCons name ty tailRow' row'
  ) => SequenceMaybeRecord (Cons name (Maybe ty) tail) row row' where
  sequenceMaybeRecordImpl _ a  = 
       insert namep <$> valA <*> rest
    where
      namep = SProxy :: SProxy name
      valA = get namep a
      tailp = RLProxy :: RLProxy tail
      rest = sequenceMaybeRecordImpl tailp a
      

instance sequenceMaybeRecordNil :: SequenceMaybeRecord Nil row () where
  sequenceMaybeRecordImpl _ _ = pure {}

sequenceMaybeRecord :: forall row row' rl
   . RowToList row rl
  => SequenceMaybeRecord rl row row'
  => Record row
  -> Maybe (Record row')
sequenceMaybeRecord a = sequenceMaybeRecordImpl (RLProxy :: RLProxy rl) a
          

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  print $ foldRecord (\x -> [x]) {a: 1, b: 2, c: 3}
  -- {"c":4,"b":3,"a":2}

  let traversed = traverseRecord (\x -> Just x) {x: 1, y: 2, z: 3}
  print traversed
  -- {"c":4,"b":3,"a":2}

  let t = sequenceMaybeRecord {x: Just "a", y: Just 1, z: Just 3}
  print t
  where
    print :: forall a. a -> Eff (console :: CONSOLE | e) Unit
    print = log <<< unsafeStringify
