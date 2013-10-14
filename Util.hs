module Util where

(&) :: (a -> b) -> (b -> c) -> (a -> c);
(f & g) x = g (f x);

infixl 0 `onn`;
onn :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c;
onn f g x y z = f (g x) (g y) (g z);

infixl 0 `onnn`;
onnn :: (b -> b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> a -> c;
onnn f g w x y z = f (g w) (g x) (g y) (g z);

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d;
uncurry3 f (x, y, z) = f x y z;

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e;
uncurry4 f (w, x, y, z) = f w x y z;
