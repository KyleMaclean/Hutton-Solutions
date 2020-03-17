-- 1
-- define an instance of the Functor class for the following type of binary trees that have data in their nodes:

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

-- 2
-- complete the following instance declaration to make the partially-applied function type (a ->) into a functor: (hint: first write down the type of fmap and then think if you already know a library function that has this type.

instance Functor ((->) a) where -- ...

-- 3
-- define an instance of the Applicative class for the type (a ->). if you are familiar with combinatory logic, you might recognise `pure` and `<*>` for this type as being the well-known K and S combinators.

-- 4
-- there may be more than one way to make a parameterised type into an applicative functor. for example, the library Control.Applicative provides an alternative "zippy" instance for lists, in which the function `pure` makes an infinite list of copies of its argument, and the operator `<*>` applies each argument function to the corresponding argument value at the same position. complete the following declarations that implement this idea:

newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
         -- fmap :: (a -> b) -> ZipList a -> ZipList b
         fmap g (Z xs) = -- ...
         
instance Applicative ZipList where
         -- pure :: a -> ZipList a
         pure x = -- ...
         
         -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
         (Z gs) <$> (Z xs) = -- ...
         -- the ZipList wrapper around the list type is required because each type can have only at most one instance declaration for a given class

-- 5
-- work out the types for the variables in the four applicative laws.

-- 6
-- define an instance of the Monad clas for the type (a ->).

-- 7
-- given the following type of expressions, that contain variables of some type a, show how to make this type into instance of the Functor, Applicative and Monad classes. With the aid of an example, explain what the >>= operator for this type does.

-- 8
-- rather than making a parameterised type into instances of the Functor, Applicative and Monad classes in this order, in practice it is sometimes simpler to define the functor and applicative instances in terms of the monad instance, relying on the fact that the order in which declarations are made is not important in Haskell. complete the missing parts in the folloing declarations for the ST type using the do notation.

instance Functor ST where
         -- fmap :: (a -> b) -> ST a -> ST b
         fmap g st = do -- ...
         
instance Applicative ST where
         -- pure :: a -> ST a
         pure x = S (\s -> (x,s))
         
         -- (<*>) :: ST (a -> b) -> ST a -> ST b
         stf <*> stx = do -- ...
         
instance Monad ST where
         -- (>>=) :: ST a -> (a -> ST b) -> ST b
         st >>= f = S (\s -> let (x,s') = app st s in app (f x) s')
