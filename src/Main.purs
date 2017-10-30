module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Foldable (traverse_)
import Data.List (List, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (class Monoid, mempty)
import Data.Record (get, insert)
import Data.Tuple (Tuple(..))
import Global.Unsafe (unsafeStringify)
import Type.Prelude (class IsSymbol, class ListToRow, class RowLacks, class RowToList, RLProxy(RLProxy), RProxy(..), SProxy(SProxy), reflectSymbol, kind Boolean)
import Type.Row (Cons, Nil, kind RowList)

mapRecord :: forall row xs a b row'
   . RowToList row xs
  => MapRecord xs row a b row'
  => (a -> b)
  -> Record row
  -> Record row'
mapRecord = mapRecordImpl (RLProxy :: RLProxy xs)

class MapRecord (xs :: RowList) (row :: # Type) a b (row' :: # Type)
  | xs -> row row' a b where
  mapRecordImpl :: RLProxy xs -> (a -> b) -> Record row -> Record row'

instance mapRecordCons ::
  ( IsSymbol name
  , Show a
  , RowCons name a trash row
  , MapRecord tail row a b tailRow'
  , RowLacks name tailRow'
  , RowCons name b tailRow' row'
  ) => MapRecord (Cons name a tail) row a b row' where
  mapRecordImpl _ f r =
    insert nameP val rest
    where
      nameP = SProxy :: SProxy name
      val = f $ get nameP r
      rest = mapRecordImpl (RLProxy :: RLProxy tail) f r

instance mapRecordNil :: MapRecord Nil row a b () where
  mapRecordImpl _ _ _ = {}
{-}
showRecord :: forall row xs row'
   . RowToList row xs
  => ShowRecord xs row row'
  => Record row
  -> Record row'
showRecord = showRecordImpl (RLProxy :: RLProxy xs)

class ShowRecord (xs :: RowList) (row :: # Type) (row' :: # Type)
  | xs -> row row' where
  showRecordImpl :: RLProxy xs -> Record row -> Record row'

instance showRecordCons ::
  ( IsSymbol name
  , RowCons name a trash row
  , ShowRecord tail row tailRow'
  , RowLacks name tailRow'
  , RowCons name String tailRow' row'
  ) => ShowRecord (Cons name a tail) row row' where
  showRecordImpl _ r =
    insert nameP val rest
    where
      nameP = SProxy :: SProxy name
      val = show $ get nameP r
      rest = showRecordImpl (RLProxy :: RLProxy tail) r

instance showRecordNil :: ShowRecord Nil row () where
  showRecordImpl _ _ = {}
-}
class ZipRecord
  ( rla :: RowList )
  ( ra :: # Type )
  ( rlb :: RowList )
  ( rb :: # Type )
  ( rc :: # Type )
  | rla -> ra rc
  , rlb -> rb rc
  where
    zipRecordImpl ::
         RLProxy rla
      -> Record ra
      -> RLProxy rlb
      -> Record rb
      -> Record rc

instance zipRecordNil :: ZipRecord Nil trashA Nil trashB ()
  where
    zipRecordImpl _ _ _ _ = {}

instance zipRecordCons
    :: ( IsSymbol k
       , RowCons k a trashA ra
       , RowCons k b trashB rb
       , RowCons k (Tuple a b) rc' rc
       , RowLacks k rc'
       , ZipRecord ta ra tb rb rc'
       )
    => ZipRecord
         (Cons k a ta)
         ra
         (Cons k b tb)
         rb
         rc
  where
    zipRecordImpl _ ra _ rb = insert name head tail
      where
        name = SProxy :: SProxy k
        head = Tuple (get name ra) (get name rb)
        ta = RLProxy :: RLProxy ta
        tb = RLProxy :: RLProxy tb
        tail = zipRecordImpl ta ra tb rb

zipRecord :: forall ta ra tb rb rc a b
   . RowToList ra ta
  => RowToList rb tb
  => ZipRecord ta ra tb rb rc
  => Record ra
  -> Record rb
  -> Record rc
zipRecord ra rb = zipRecordImpl ta ra tb rb
  where
    ta = RLProxy :: RLProxy ta
    tb = RLProxy :: RLProxy tb

class Keys (xs :: RowList) where
  keysImpl :: RLProxy xs -> List String

instance nilKeys :: Keys Nil where
  keysImpl _ = mempty

instance consKeys ::
  ( IsSymbol name
  , Keys tail
  ) => Keys (Cons name ty tail) where
  keysImpl _ = first : rest
    where
      first = reflectSymbol (SProxy :: SProxy name)
      rest = keysImpl (RLProxy :: RLProxy tail)

keys :: forall g row rl
   . RowToList row rl
  => Keys rl
  => g row -- this will work for any type with the row as a param!
  -> List String
keys _ = keysImpl (RLProxy :: RLProxy rl)

slistKeys :: forall g tuples rl
   . SListToRowList tuples rl
  => Keys rl
  => g tuples
  -> List String
slistKeys _ = keysImpl (RLProxy :: RLProxy rl)

foreign import kind SList
foreign import data SCons :: Symbol -> SList -> SList
foreign import data SNil :: SList

data SLProxy (xs :: SList) = SLProxy

infixr 6 type SCons as :::

class SListToRowList (xs :: SList) (rl :: RowList) | xs -> rl, rl -> xs

instance slToRlSNil :: SListToRowList SNil Nil

instance slToRlSCons ::
  ( SListToRowList sTail tail
  ) => SListToRowList (SCons name sTail) (Cons name trash tail)

class EqRecord rl row
  | rl -> row
  where
    eqRecordImpl :: RLProxy rl -> Record row -> Record row -> Boolean

instance eqRecordCons ::
  ( IsSymbol name
  , Eq ty
  , RowCons name ty trash row
  , EqRecord tail row
  ) => EqRecord (Cons name ty tail) row where
  eqRecordImpl _ a b =
    if valA == valB
      then eqRecordImpl tailp a b
      else false
    where
      namep = SProxy :: SProxy name
      valA = get namep a
      valB = get namep b
      tailp = RLProxy :: RLProxy tail

instance eqRecordNil :: EqRecord Nil row where
  eqRecordImpl _ _ _ = true

eqRecord :: forall row rl
   . RowToList row rl
  => EqRecord rl row
  => Record row
  -> Record row
  -> Boolean
eqRecord a b = eqRecordImpl (RLProxy :: RLProxy rl) a b



class EqMemberRecord rl row row'
  | rl -> row row'
  where
    eqMemberRecordImpl :: RLProxy rl -> Record row -> Record row -> Record row'

instance eqMemberRecordCons ::
  ( IsSymbol name
  , Eq ty
  , RowCons name ty trash row
  , EqMemberRecord tail row tailRow'
  , RowLacks name tailRow'
  , RowCons name Boolean tailRow' row'
  ) => EqMemberRecord (Cons name ty tail) row row' where
  eqMemberRecordImpl _ a b =
    insert namep val rest
    where
      val = valA == valB
      namep = SProxy :: SProxy name
      valA = get namep a
      valB = get namep b
      tailp = RLProxy :: RLProxy tail
      rest = eqMemberRecordImpl tailp a b

instance eqMemberRecordNil :: EqMemberRecord Nil row () where
  eqMemberRecordImpl _ _ _ = {}

eqMemberRecord :: forall row row' rl
   . RowToList row rl
  => EqMemberRecord rl row row'
  => Record row
  -> Record row
  -> Record row'
eqMemberRecord a b = eqMemberRecordImpl (RLProxy :: RLProxy rl) a b


class ShowMemeberRecord rl row row'
  | rl -> row row'
  where
    showMemberRecordImpl :: RLProxy rl -> Record row -> Record row'

instance showMemberRecordCons ::
  ( IsSymbol name
  , Show ty
  , RowCons name ty trash row
  , ShowMemeberRecord tail row tailRow'
  , RowLacks name tailRow'
  , RowCons name String tailRow' row'
  ) => ShowMemeberRecord (Cons name ty tail) row row' where
  showMemberRecordImpl _ a  =
    insert namep val rest
    where
      val = show valA
      namep = SProxy :: SProxy name
      valA = get namep a
      tailp = RLProxy :: RLProxy tail
      rest = showMemberRecordImpl tailp a

instance showMemberRecordNil :: ShowMemeberRecord Nil row () where
  showMemberRecordImpl _ _ = {}

showMemberRecord :: forall row row' rl
   . RowToList row rl
  => ShowMemeberRecord rl row row'
  => Record row
  -> Record row'
showMemberRecord a = showMemberRecordImpl (RLProxy :: RLProxy rl) a

foldShowRecord :: forall row xs a b row'
   . RowToList row xs
  => FoldShowRecord xs row a b row'
  => Monoid b
  => (a -> b)
  -> Record row
  -> b
foldShowRecord = foldShowRecordImpl (RLProxy :: RLProxy xs)

class FoldShowRecord (xs :: RowList) (row :: # Type) a b (row' :: # Type)
  | xs -> row row' a b where
  foldShowRecordImpl :: Monoid b => RLProxy xs -> (a -> b) -> Record row -> b

instance foldShowRecordCons ::
  ( IsSymbol name
  , RowCons name a trash row
  , FoldShowRecord tail row a b tailRow'
  , RowLacks name tailRow'
  , RowCons name b tailRow' row'
  ) => FoldShowRecord (Cons name a tail) row a b row' where
  foldShowRecordImpl _ f r =
    val <> rest
    where
      nameP = SProxy :: SProxy name
      val = f $ get nameP r
      rest = foldShowRecordImpl (RLProxy :: RLProxy tail) f r

instance foldShowRecordNil :: FoldShowRecord Nil row a b () where
  foldShowRecordImpl _ _ _ = mempty

traverseShowRecord :: forall m row xs a b row'
   . RowToList row xs
  => TraverseShowRecord xs row a b row'
  => Applicative m
  => (a -> m b)
  -> Record row
  -> m (Record row')
traverseShowRecord = traverseShowRecordImpl (RLProxy :: RLProxy xs)

class TraverseShowRecord (xs :: RowList) (row :: # Type) a b (row' :: # Type)
  | xs -> row row' a b where
  traverseShowRecordImpl :: forall m . Applicative m => RLProxy xs -> (a -> m b) -> Record row -> m (Record row')

instance traverseShowRecordCons ::
  ( IsSymbol name
  , RowCons name a trash row
  , TraverseShowRecord tail row a b tailRow'
  , RowLacks name tailRow'
  , RowCons name b tailRow' row'
  ) => TraverseShowRecord (Cons name a tail) row a b row' where
  traverseShowRecordImpl _ f r =
    (\v r -> insert nameP v r) <$> val <*> rest
    where
      nameP = SProxy :: SProxy name
      val = f $ get nameP r -- m b
      rest = traverseShowRecordImpl (RLProxy :: RLProxy tail) f r

instance traverseShowRecordNil :: TraverseShowRecord Nil row a b () where
  traverseShowRecordImpl _ _ _ = pure {}


{-
class Applicative m <= SequenceRecord rl row row' m
  | rl -> m row row'
  where
    sequenceRecordImpl :: RLProxy rl -> Record row -> m (Record row')

instance sequenceRecordCons ::
  ( IsSymbol name
  , Apply m
  , RowCons name (m ty) trash row
  , SequenceRecord tail row tailRow'
  , RowLacks name tailRow'
  , RowCons name ty tailRow' row'
  ) => SequenceRecord (Cons name (m ty) tail) row row' m where
  sequenceRecordImpl _ a  = 
       insert namep <$> valA <*> rest
    where
      namep = SProxy :: SProxy name
      valA = get namep a
      tailp = RLProxy :: RLProxy tail
      rest = sequenceRecordImpl tailp a
      

instance sequenceRecordNil :: SequenceRecord Nil row () m where
  sequenceRecordImpl _ _ = pure {}

sequenceRecord :: forall row row' rl
   . RowToList row rl
  => SequenceRecord rl row row' m
  => Record row
  -> m (Record row')
sequenceRecord a = sequenceRecordImpl (RLProxy :: RLProxy rl) a
-}           

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  --print $ showRecord {a: "a", b: "c", c: "d"}
  -- {"c":4,"b":3,"a":2}
  print $ mapRecord (append "shown: " <<< show) {a: 1, b: 2, c: 3}
  -- {"c":"3","b":"2","a":"1"}
  print $ foldShowRecord (\x -> [x]) {a: 1, b: 2, c: 3}
  -- {"c":4,"b":3,"a":2}

  let traversed = traverseShowRecord (\x -> Just x) {x: 1, y: 2, z: 3}
  print traversed
  -- {"c":4,"b":3,"a":2}

  --let sequenced = sequenceShowRecord {x: Just 1, y: Just 2}
  --print sequenced
  -- {"c":4,"b":3,"a":2}

  print $ zipRecord { a: 1, b: 5 } { a: 1, b: 4 }
  -- {"b":{"value0":5,"value1":4},"a":{"value0":1,"value1":1}}

  -- works with records and RProxy because of the forall g. definition above
  traverse_ print $ keys { a: 1, b: 2 }
  -- "a" "b"
  traverse_ print $ keys $ RProxy :: RProxy (c :: Void, d :: Void)
  -- "c" "d"

  -- used my symbol list definition to do stuff
  traverse_ print $ slistKeys $ SLProxy :: SLProxy ("a" ::: "b" ::: "c" ::: SNil)
  -- "a" "b" "c"

  -- can't do nested until Eq Record integrated into prelude
  -- or we use a new type class with overloading tricks until instance chains get added :(
  print $ showMemberRecord {a: 1, b: "abc", c: 3}
  -- true
  print $ eqRecord {a: 1, b: 2, c: 3} {a: 1, b: 2, c: 9999999}
  -- false
--  let t = sequenceRecord {x: Just "a", y: Just 1, z: Just 3}
--  print t
  where
    print :: forall a. a -> Eff (console :: CONSOLE | e) Unit
    print = log <<< unsafeStringify
