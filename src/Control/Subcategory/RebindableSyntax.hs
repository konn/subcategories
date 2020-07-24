{-# LANGUAGE NoImplicitPrelude #-}
module Control.Subcategory.RebindableSyntax
  ( ifThenElse, Eq(..), Num(..)
  , (>>=), (>>), fromLabel, return
  , module Control.Arrow
  ) where
import Control.Subcategory.Applicative
import Control.Subcategory.Bind
import Control.Subcategory.Functor
import Control.Subcategory.Pointed

import Control.Arrow
import GHC.OverloadedLabels
import Prelude              (Bool (..), Eq (..), Num (..))

ifThenElse :: Bool -> a -> a -> a
ifThenElse True t _  = t
ifThenElse False _ f = f
{-# INLINE ifThenElse #-}

(>>=) :: (Cat m a, Cat m b, CBind m)
      => m a -> (a -> m b) -> m b
(>>=) = (>>-)
{-# INLINE (>>=) #-}

(>>) :: (Cat m a, Cat m b, CApplicative m)
      => m a -> m b -> m b
(>>) = (.>)
{-# INLINE (>>) #-}

return :: (Cat m a, CPointed m) => a -> m a
return = cpure
{-# INLINE return #-}
