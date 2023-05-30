module Control.Comonad.Cofree.Zipper.Internal 
  ( Zipper
  --, Idx

  -- * Constructing Zippers
  , zipper
  , fromRecursive
  , tagged

  -- * Movement
  --, down
  --, up
  --, sibling
  , tug

  -- * Folding and flattening
  --, rezip
  --, flatten
  --, fold

  -- * Getters
  --, focus
  , branches
  , currentIndex

  -- * Optics
  --, focus_
  --, unwrapped_
  , branches_
  --, children_
  --, ichildren_
  )
where

import Prelude
import Data.List (List(..), (:), reverse)
import Data.Leibniz (type (~))
import Data.Eq (class Eq1)
import Data.Ord (class Ord1)
import Data.Functor
import Data.Traversable (class Traversable)
import Data.Maybe
import Data.Tuple
import Data.Tuple.Nested ((/\))
import Control.Comonad.Cofree ((:<), Cofree, head, tail, buildCofree)
import Data.Lens
import Data.Lens.Index (class Index)
import Matryoshka.Class.Recursive (class Recursive, project)
import Matryoshka.Class.Corecursive (class Corecursive, embed)
import Matryoshka (cata)

--type Idx i f a = (Index (Tuple (f (Cofree f a)) IxValue (f (Cofree f a))) ~ (Tuple (Cofree f a) Index (f (Cofree f a)) ~ i))

-- | The core zipper type
data Zipper i f a = Zipper (List (Tuple i (Cofree f a))) (Cofree f a)

derive instance Functor f => Functor (Zipper i f)
derive instance (Eq i, Eq1 f, Eq a) => Eq (Zipper i f a)
derive instance (Ord i, Ord1 f, Ord a) => Ord (Zipper i f a)

-- | Get the location of the current selection within its parent if we have one
--
-- | @O(1)@
currentIndex :: forall i f a. Zipper i f a -> Maybe i
currentIndex (Zipper ((i /\ _) : _) _ ) = Just i
currentIndex _ = Nothing

-- |
currentPath :: forall i f a. Zipper i f a -> List i
currentPath (Zipper parents focus) = reverse $ map fst parents

--focus_ :: forall i f a. Lens' (Zipper i f a) a
--focus_ f (Zipper parents focus) = Zipper parents <$>  (focus # _extract `id` f)

id :: forall a. a -> a
id x = x
--
--unwrapped_ :: forall i f a. Lens' (Zipper i f a) (Cofree f a)
--unwrapped_ f (Zipper parents focus) = Zipper parents <$> f focus

_extract :: forall f g a. Functor f => (a -> f a) -> Cofree g a -> f (Cofree g a)
_extract f t = (flip (:<) (tail t)) <$> (f $ head t)

tug :: forall a. (a -> Maybe a) -> a -> a
tug f a = fromMaybe a (f a)

zipper :: forall i f a. Cofree f a -> Zipper i f a
zipper f = Zipper Nil f

fromRecursive :: forall t f i a. Recursive t f => t -> Zipper i f Unit
fromRecursive t = zipper $ buildCofree ((unit /\ _) <<< project) t

tagged :: forall t f i a. Recursive t f => (t -> a) -> t -> Zipper i f a
tagged f t = zipper $ buildCofree (\x -> (f x /\ project x)) t

--down :: forall i f a. i -> Zipper i f a -> Maybe (Zipper i f a)
--down i (Zipper parents current) = Zipper ((i /\ current) : parents) <$>  (current ^? _unwrap . ix i)

--up :: forall i f a. Zipper i f a -> Maybe (Zipper i f a)
--up (Zipper ((i /\ p) : parents) current) = Just $ Zipper parents (p # _unwrap . ix i .~ current)


--rezip :: forall i f a. Zipper i f a -> Cofree f a
--rezip z = case up z of 
--  Nothing -> focus z
--  Just p -> rezip p

--flatten :: (Corecursive f) => Zipper i t a -> f
--flatten = cata alg <<< rezip
--  where alg t = embed $ tail t

--sibling :: forall i f a. Zipper i f a -> Maybe (Zipper i f a)
--sibling i = up >=> down i

--children_ :: forall i f a. Traversable f => Traversal' (Zipper i f a) (Cofree f a)
--children_ f (Zipper parents current) = Zipper parents <$> (?w)
--children_ f (Zipper parents current) = Zipper parents <$> (current # _unwrap <<< (traversed id f))

branches :: forall i f a. Zipper i f a -> f (Cofree f a)
branches (Zipper _ t) = tail t

branches_ :: forall i f a. Lens' (Zipper i f a) (f (Cofree f a))
branches_ = lens getter setter
  where
    getter (Zipper _ t) = tail t
    setter (Zipper p t) f = (Zipper p (head t :< f))


--fold :: Functor f => (a -> f r -> r) -> Zipper i f a -> r
--fold f = cata (\t -> f (head t) (tail t)) <<< rezip

_unwrap :: forall f g a. Functor f => (g (Cofree g a) -> f (g (Cofree g a))) -> Cofree g a -> f (Cofree g a)
_unwrap f t = ((:<) (head t)) <$> f (tail t)