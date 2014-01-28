module Data.BinPack.R2 where

import Control.Applicative
import Data.Foldable
import Data.Monoid

data Bin2 n a = B2Node
              { binWidth, binHeight     :: n
              , binRight, binBelow      :: Bin2 n a
              , binItem                 :: a
              }
              | B2Empty
              { binWidth, binHeight     :: n
              }
    deriving (Show)

instance Functor (Bin2 n) where
    fmap _ (B2Empty w h) = B2Empty w h
    fmap f (B2Node w h r b i) = B2Node w h (fmap f r) (fmap f b) (f i)

instance Foldable (Bin2 n) where
    foldMap _ (B2Empty _ _) = mempty
    foldMap f (B2Node _ _ r b i) = f i <> foldMap f r <> foldMap f b

rectBin :: n -> n -> Bin2 n a
rectBin = B2Empty

sqBin :: n -> Bin2 n a
sqBin x = rectBin x x

pack :: (Num n, Ord n) => Bin2 n a -> n -> n -> a -> Maybe (Bin2 n a)
pack (B2Node bw bh r b bi) w h i = right i <|> below i
    where
        right = fmap (\r' -> B2Node bw bh r' b bi) . pack r w h
        below = fmap (\b' -> B2Node bw bh r b' bi) . pack b w h
pack (B2Empty bw bh) w h i
    | fits = Just $ B2Node w h right below i
    | otherwise = Nothing
    where
        fits = w <= bw && h <= bh
        right = B2Empty (bw - w) h
        below = B2Empty bw (bh - h)

sample :: (Num n, Ord n) => Bin2 n a -> n -> n -> Maybe a
sample (B2Node bw bh r b i) x y =
    case (compare x bw, compare y bh) of
        (LT, LT) -> Just i
        (_ , LT) -> sample r (x - bw) y
        _        -> sample b x (y - bh)
sample (B2Empty _ _) _ _ = Nothing
