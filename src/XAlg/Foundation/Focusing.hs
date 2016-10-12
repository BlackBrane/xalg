module XAlg.Foundation.Focusing (

-- * Types
  Doubled,
  Focused,

-- * Generic term traversal operations
-- | These functions provide a means to step through the layers of any functorial data type
--   providing a `Traversable` instance.
--
--   The result is what amounts to two dimensional freedom of movement.
--   The horizonal direction corresponds to movement between child nodes at a particular level.
--   More precisely it corresponds to moving along the output of 'toList', which is
--   provided by the `Foldable` instance.
--
--   The vertical direction corresponds to moving to deeper or shallower nesting levels within
--   the expression tree.
  moveUp,
  moveDown,
  moveLeft,
  moveRight,

-- ** Constituent traversal functions
-- | Used to construct the above.
  setFocusAt,
  moveLeftNode,
  moveRightNode,

-- ** Doubled helpers
  unSum,
  isLeft,
  isRight,
  sumBimap,

-- ** Focus operations

  focus,
  defocus,
  initFocus,
  collapseFocus,
  peelFocus,
  wrapFocus,
  extractFocus,
  transformFocus,


-- ** Combinators

  focusMap,
  childrenMap,
  subTerms,

-- ** Localization

  hasFocusedNode,
  position,
  focusPosition

) where

import XAlg.Foundation.Core
import XAlg.Foundation.Recursion
-- import XAlg.Languages.Arithmetic

import Control.Monad.State
import Data.Foldable (toList)
import Data.Maybe (fromMaybe, catMaybes)


-- | Doubled is the functor sum of a functor with itself.
--   Useful for when we want to store 1 extra bit at each node of a structure.
type Doubled f = f :+: f

-- | Focused f is the functor fixed-point of the `Doubled` f.
--   It's used to denote focus upon a particular node in a structure.
--
--   The encoding for our purposes is:
--
--     * /__SumR__ denotes the focused element./
--     * /__SumL__ denotes an unfocused element./
type Focused f = Fix (Doubled f)


isLeft :: (f :+: g) a -> Bool
isLeft (SumL _) = True
isLeft (SumR _) = False

isRight :: (f :+: g) a -> Bool
isRight (SumR _) = True
isRight (SumL _) = False

isFocus :: Focused f -> Bool
isFocus = isRight . out

unSum :: (f :+: f) a -> f a
unSum (SumL fx) = fx
unSum (SumR fx) = fx

sumBimap :: (f a -> b) -> (g a -> b) -> (f :+: g) a -> b
sumBimap h j (SumL fx) = h fx
sumBimap h j (SumR gx) = j gx


-- | Enure the top node is marked as the focus.
focus :: Focused f -> Focused f
focus (In (SumR x)) = In (SumR x)
focus (In (SumL x)) = In (SumR x)

-- | Enure the top node is not marked as the focus.
defocus :: Focused f -> Focused f
defocus (In (SumR x)) = In (SumL x)
defocus (In (SumL x)) = In (SumL x)

-- | Turn a vanilla-style expression into the corresponding 'Focused' expression with focus at the
--   top of the structure.
initFocus :: Fixable f x => x -> Focused f
initFocus = initFocus' . toFix
  where initFocus' :: Functor f => Fix f -> Focused f
        initFocus' (out . sumFixR -> SumL fx) = In $ SumR fx
        initFocus' _                          = undefined -- impossible

-- | Cast a structure down to its non-functorial, non-focused form via the Fixable class
--   method `fromFix`.
collapseFocus :: Fixable f x => Focused f -> x
collapseFocus = cata (foldFix . unSum)

-- | Remove `Fix` and `:+:` constructors to access a raw node of a structure.
peelFocus :: Focused f -> f (Focused f)
peelFocus = unSum . out

-- | Put the `Fix` and `:+:` back on, not applying the focus (using SumL as opposed to SumR).
wrapFocus :: f (Focused f) -> Focused f
wrapFocus = In . SumL

-- | Put the `Fix` and `:+:` back on, while applying focus (use SumR instead of SumL).
wrapFocusR :: f (Focused f) -> Focused f
wrapFocusR = In . SumR


-- | Flatten data type to non-parametric form while applying function to focused subexpression.
transformFocus :: Fixable f x => (x -> x) -> Focused f -> x
transformFocus g = cata $ sumBimap foldFix (g . foldFix)


-- | Return the first element of a foldable structure satisfying the predicate, or `Nothing`.
find :: Foldable f => (a -> Bool) -> f a -> Maybe a
find f (filter f . toList -> x:_) = Just x
find _ _                          = Nothing

-- | Return a sequence of lists of focused expressions, each representing the subexpressions of
--   the input at successively deeper levels of nesting.
subTerms :: Foldable f => Focused f -> [[Focused f]]
subTerms = takeWhile (not . null) . iterate step . pure
  where step :: Foldable f => [Focused f] -> [Focused f]
        step = concatMap (toList . peelFocus)

-- | Return the (flattened, as in 'fromFix') subexpression marked as focused.
extractFocus :: (Foldable f, Fixable f a) => Focused f -> Maybe a
extractFocus = fmap collapseFocus . find isFocus . concat . subTerms

focusMap :: (f (Focused f) -> g (Focused g)) -> Focused f -> Focused g
focusMap h (In (SumL fx)) = In (SumL (h fx))
focusMap h (In (SumR fx)) = In (SumR (h fx))

childrenMap :: Functor f => (Focused f -> Focused f) -> Focused f -> Focused f
childrenMap h = focusMap (fmap h)

-- | Are any of the immediate children of the top node of a structure marked as the focus?
hasFocusedNode :: Foldable f => Focused f -> Bool
hasFocusedNode = any (isRight . out) . peelFocus

-- | For a predicate function p, position p yields the (0-based) index of the first element
--   that satisfies p.
position :: Foldable f => (a -> Bool) -> f a -> Maybe Int
position p xs = let l1 = length . takeWhile (not . p) . toList $ xs
                    l2 = length . toList $ xs
                in if l1 == l2 then Nothing
                               else Just l1


-- | Return the index of the 'horizontal' focus position at a given node.
--   Yields only the first index as there should be only one in our use-cases.
focusPosition :: Foldable f => f ((g :+: h) a) -> Maybe Int
focusPosition = position isRight


setFocusStep :: Int -> Focused f -> State Int (Focused f)
setFocusStep i fx = do
  j <- get
  modify (+1)
  return $ if i == j then focus fx else defocus fx


setFocusAt_ :: Traversable t => Int -> t (Focused f) -> t (Focused f)
setFocusAt_ i xs = evalState (traverse (setFocusStep i) xs) 0


focusPosition_ :: Traversable t => Focused t -> Maybe Int
focusPosition_ = focusPosition . fmap out . unSum . out



setFocusAt :: Traversable f => Int -> Focused f -> Focused f
setFocusAt i = In . setFocusAt_ i . out

boundDown b x = if x < b then b else x
boundUp   b x = if x > b then b else x

-- | Move focus 'right' if one exists at the current node level.
moveRightNode :: Traversable t => Int -> Focused t -> Focused t
moveRightNode b tx = fromMaybe tx $
  setFocusAt . boundUp b . (+1) <$> focusPosition_ tx <*> pure tx

-- | Move focus 'left' if one exists at the current node level.
moveLeftNode :: Traversable t => Focused t -> Focused t
moveLeftNode tx = fromMaybe tx $
  setFocusAt . boundDown 0 . subtract 1 <$> focusPosition_ tx <*> pure tx



moveRight :: Traversable t => Focused t -> Focused t
moveRight tx = let b = subtract 1 . numChildren . peelFocus $ tx
               in  topDown (moveRightNode b) tx

moveLeft :: Traversable t => Focused t -> Focused t
moveLeft = topDown moveLeftNode


moveUp :: Traversable t => Focused t -> Focused t
moveUp x | isFocus x        = x
         | hasFocusedNode x = childrenMap defocus $ focus x
         | otherwise        = childrenMap moveUp x

moveDown :: Traversable t => Focused t -> Focused t
moveDown fx | null . peelFocus $ fx = fx -- don't do anything if there are no children
            | isFocus fx = defocus . setFocusAt 0 $ fx
            | otherwise  = childrenMap moveDown fx
