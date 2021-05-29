module Util.Internal.Indexed where

-- TODO: use unboxed tuples?

data WithIndex a = WithIndex !Int a

-- | > Compose (State Int) f a
newtype Indexed f a = Indexed { runIndexed :: Int -> WithIndex (f a) }

instance Functor f => Functor (Indexed f) where
    fmap f (Indexed sf) = Indexed $ \s -> let WithIndex s' x = sf s in WithIndex s' (fmap f x)
    {-# INLINE fmap #-}

instance Applicative f => Applicative (Indexed f) where
    pure x = Indexed $ \s -> WithIndex s (pure x)
    {-# INLINE pure #-}

    Indexed sfa <*> Indexed sfb = Indexed $ \s ->
        let WithIndex s' f = sfa s
            WithIndex s'' x = sfb s'
        in WithIndex s'' (f <*> x)
    {-# INLINE (<*>) #-}

evalIndexed :: Indexed f a -> Int -> f a
evalIndexed (Indexed sf) x = let WithIndex _ y = sf x in y
{-# INLINE evalIndexed #-}
