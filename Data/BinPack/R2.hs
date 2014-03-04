module Data.BinPack.R2
( Bin2
, pack
, toList
, sample
, container
, sqContainer
, item
, sqItem
)
where

import Control.Applicative
import Data.Foldable (Foldable, foldMap, foldlM)
import Data.Monoid
import Data.List
import Data.Ord

data Bin2 n a
    = Node  { _width, _height   :: !n
            , _right, _below    :: Bin2 n a
            , _item             :: a }
    | Free  { _width, _height   :: !n }
    deriving (Show)

newtype Item n a = Item (n, n, a)

instance Functor (Bin2 n) where
    fmap f (Node w h r b e) = Node w h (fmap f r) (fmap f b) (f e)
    fmap _ (Free w h)       = Free w h

instance Foldable (Bin2 n) where
    foldMap f (Node _ _ r b e) = f e <> foldMap f r <> foldMap f b
    foldMap _ (Free _ _)       = mempty

container :: n -> n -> Bin2 n a
container = Free

sqContainer :: n -> Bin2 n a
sqContainer x = container x x

item :: n -> n -> a -> Item n a
item w h i = Item (w, h, i)

sqItem :: n -> a -> Item n a
sqItem l = item l l

pack' :: (Num n, Ord n) => Bin2 n a -> Item n a -> Maybe (Bin2 n a)
pack' (Free fw fh) (Item (w, h, i))
    | fits = Just $ Node w h pright pbelow i
    | otherwise = Nothing
    where
        fits = w <= fw && h <= fh
        pright = container (fw - w) h
        pbelow = container fw (fh - h)

pack' (Node nw nh r b ni) i = pright i <|> pbelow i
    where
        pright = fmap (\r' -> Node nw nh r' b ni) . pack' r
        pbelow = fmap (\b' -> Node nw nh r b' ni) . pack' b

pack :: (Num n, Ord n) => Bin2 n a -> [Item n a] -> Maybe (Bin2 n a)
pack c = foldlM pack' c . sortBy (comparing area)
    where
        area (Item (w, h, _)) = w * h

toList :: (Num n, Ord n) => Bin2 n a -> [(a, (n, n, n, n))]
toList = unpack' 0 0
    where
        unpack' _ _ (Free _ _)       = []
        unpack' x y (Node w h r b i) =
            (i, (x, y, w, h)) : unpack' (x + w) y r ++ unpack' x (y + h) b

sample :: (Num n, Ord n) => Bin2 n a -> n -> n -> Maybe (a, n, n)
sample (Free _ _)       _ _ = Nothing
sample (Node w h r b i) x y =
    case (compare x w, compare y h) of
        (LT, LT) -> Just (i, x, y)
        (_ , LT) -> sample r (x - w) y
        _        -> sample b x (y - h)
