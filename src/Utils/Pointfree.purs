module Utils.Pointfree where

import Prelude

compMap :: forall a b c f. Functor f => (b -> c) -> (a -> f b) -> (a -> f c)
compMap f g = (f <$> _) <<< g

compMapFlipped :: forall a b c f. Functor f => (a -> f b) -> (b -> c) -> (a -> f c)
compMapFlipped g f = g >>> (f <$> _)

infixl 4 compMap as <<$>>
infixl 1 compMapFlipped as <<#>>
