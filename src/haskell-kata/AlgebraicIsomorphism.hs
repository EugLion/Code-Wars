module AlgebraicIsomorphism where

-- | Make all the algebraic isomorphisms typecheck (2 kyu)
-- | Link: https://biturl.io/AlgIso

-- | My original solution (incomplete)

import Data.Void
import Data.Tuple
import Control.Arrow ((***))

type ISO a b = (a -> b, b -> a)

-- given ISO a b, we can go from a to b
substL :: ISO a b -> (a -> b)
substL = fst

-- and vice versa
substR :: ISO a b -> (b -> a)
substR = snd

-- There can be more than one ISO a b
isoBool :: ISO Bool Bool
isoBool = (id, id)

isoBoolNot :: ISO Bool Bool
isoBoolNot = (not, not)

-- isomorphism is reflexive
refl :: ISO a a
refl = (id, id)

-- isomorphism is symmetric
symm :: ISO a b -> ISO b a
symm (ab, ba) = (ba, ab)

-- isomorphism is transitive
trans :: ISO a b -> ISO b c -> ISO a c
trans (ab, ba) (bc, cb) = (bc . ab, ba . cb)

-- We can combine isomorphism:
isoTuple :: ISO a b -> ISO c d -> ISO (a, c) (b, d)
isoTuple (ab, ba) (cd, dc) = (ab *** cd, ba *** dc)

isoList :: ISO a b -> ISO [a] [b]
isoList (ab, ba) = (map ab, map ba)

isoMaybe :: ISO a b -> ISO (Maybe a) (Maybe b)
isoMaybe (ab, ba) = (helper ab, helper ba)
 where
  helper f mx = case mx of
    Just x -> Just $ f x
    _      -> Nothing

isoEither :: ISO a b -> ISO c d -> ISO (Either a c) (Either b d)
isoEither (ab, ba) (cd, dc) = (helper, helper')
 where
  helper (Left  a) = Left $ ab a
  helper (Right c) = Right $ cd c
  helper' (Left  b) = Left $ ba b
  helper' (Right d) = Right $ dc d

isoFunc :: ISO a b -> ISO c d -> ISO (a -> c) (b -> d)
isoFunc (ab, ba) (cd, dc) = (\ac -> cd . ac . ba, \bd -> dc . bd . ab)

-- Going another way is hard (and is generally impossible)
isoUnMaybe :: ISO (Maybe a) (Maybe b) -> ISO a b
isoUnMaybe (mab, mba) = (helper mab, helper mba)
 where
  helper f a = case (f $ Just a, f Nothing) of
    (Just x, _     ) -> x
    (_     , Just x) -> x
    _                -> undefined

isoEU :: ISO (Either [()] ()) (Either [()] Void)
isoEU = (f, g)
 where
  f (Left xs) = Left $ () : xs
  f _         = Left []
  g (Left  []    ) = Right ()
  g (Left  (_:xs)) = Left xs
  g (Right x     ) = absurd x

-- And we have isomorphism on isomorphism!
isoSymm :: ISO (ISO a b) (ISO b a)
isoSymm = (swap, swap)

-- Algebraic data type got the name because
-- it satisfies a lot of algebraic rules under isomorphism

-- a = b -> c = d -> a * c = b * d
isoProd :: ISO a b -> ISO c d -> ISO (a, c) (b, d)
isoProd = isoTuple

-- a = b -> c = d -> a + c = b + d
isoPlus :: ISO a b -> ISO c d -> ISO (Either a c) (Either b d)
isoPlus = isoEither

-- a = b -> S a = S b
isoS :: ISO a b -> ISO (Maybe a) (Maybe b)
isoS = isoMaybe

-- a = b -> c = d -> c ^ a = d ^ b
isoPow :: ISO a b -> ISO c d -> ISO (a -> c) (b -> d)
isoPow = isoFunc

-- a + b = b + a
plusComm :: ISO (Either a b) (Either b a)
plusComm = (swapEither, swapEither)
 where
  swapEither (Left  x) = Right x
  swapEither (Right x) = Left x

-- a + b + c = a + (b + c)
plusAssoc :: ISO (Either (Either a b) c) (Either a (Either b c))
plusAssoc = (f, g)
 where
  f (Left  (Left  a)) = Left a
  f (Left  (Right b)) = Right $ Left b
  f (Right c        ) = Right $ Right c
  g (Left  a        ) = Left $ Left a
  g (Right (Left  b)) = Left $ Right b
  g (Right (Right c)) = Right c

-- a * b = b * a
multComm :: ISO (a, b) (b, a)
multComm = (swap, swap)

-- a * b * c = a * (b * c)
multAssoc :: ISO ((a, b), c) (a, (b, c))
multAssoc = (f, g)
 where
  f ((a, b), c) = (a, (b, c))
  g (a, (b, c)) = ((a, b), c)

-- dist :: a * (b + c) = a * b + a * c
dist :: ISO (a, Either b c) (Either (a, b) (a, c))
dist = (f, g)
 where
  f (a, Left b ) = Left (a, b)
  f (a, Right c) = Right (a, c)
  g (Left  (a, b)) = (a, Left b)
  g (Right (a, c)) = (a, Right c)

-- (c ^ b) ^ a = c ^ (a * b)
curryISO :: ISO (a -> b -> c) ((a, b) -> c)
curryISO = (uncurry, curry)

-- 1 = S O (we are using peano arithmetic)
one :: ISO () (Maybe Void)
one = (const Nothing, const ())

-- 2 = S (S O)
two :: ISO Bool (Maybe (Maybe Void))
two = (f, g)
 where
  f True = Just Nothing
  f _    = Nothing
  g (Just _) = True
  g _        = False

-- O + b = b
plusO :: ISO (Either Void b) b
plusO = (left, Right)
 where
  left (Left  x) = absurd x
  left (Right b) = b

-- S a + b = S (a + b)
plusS :: ISO (Either (Maybe a) b) (Maybe (Either a b))
plusS = (f, g)
 where
  f (Left  Nothing ) = Nothing
  f (Left  (Just a)) = Just $ Left a
  f (Right b       ) = Just $ Right b
  g Nothing          = Left Nothing
  g (Just (Left  a)) = Left $ Just a
  g (Just (Right b)) = Right b

-- 1 + b = S b
plusSO :: ISO (Either () b) (Maybe b)
plusSO = isoPlus one refl `trans` plusS `trans` isoS plusO

-- O * a = O
multO :: ISO (Void, a) Void
multO = (fst, \x -> (x, absurd x))

-- S a * b = b + a * b
multS :: ISO (Maybe a, b) (Either b (a, b))
multS = (f, g)
 where
  f (Nothing, b) = Left b
  f (Just a , b) = Right (a, b)
  g (Left  b     ) = (Nothing, b)
  g (Right (a, b)) = (Just a, b)

-- 1 * b = b
multSO :: ISO ((), b) b
multSO =
  isoProd one refl
    `trans` multS
    `trans` isoPlus refl multO
    `trans` plusComm
    `trans` plusO

-- a ^ O = 1
powO :: ISO (Void -> a) ()
powO = (const (), const absurd)

-- a ^ (S b) = a * (a ^ b)
powS :: ISO (Maybe b -> a) (a, b -> a)
powS = (f, g)
 where
  f mba = (mba Nothing, mba . pure)
  g (a, ba) b = case b of
    Nothing -> a
    Just x  -> ba x

-- I went the easy way, will update the code once I get it right using trans
powSO :: ISO (() -> a) a
powSO = (f, g)
 where
  f :: (() -> a) -> a
  f x = x ()
  g = const
