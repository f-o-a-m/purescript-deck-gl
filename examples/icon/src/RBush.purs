module RBush
  ( RBush
  , Node
  , SearchBox
  , empty
  , insert
  , insertMany
  , search
  ) where

foreign import data RBush' :: Type -> Type

type Node a =
  { x :: Number
  , y :: Number
  , entry :: a
  }

newtype RBush a = RBush (RBush' (Node a))

foreign import empty :: forall a. Int -> RBush a

foreign import insert :: forall a. Node a -> RBush a -> RBush a

foreign import insertMany :: forall a. Array (Node a) -> RBush a -> RBush a

type SearchBox =
  { maxX :: Number
  , maxY :: Number
  , minX :: Number
  , minY :: Number
  }

foreign import search :: forall a. SearchBox -> RBush a -> Array (Node a)
