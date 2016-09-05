module HogueScript.Zipper where

data Zipper a = Zipper [a] [a]

-- | Create a zipper from a list
fromList :: [a] -> Zipper a
fromList = Zipper [] 

-- | Convert a zipper into a list
toList :: Zipper a -> [a]
toList (Zipper [] ys) = ys
toList (Zipper (x:xs) ys) =
    toList $ Zipper xs (x:ys)

-- | Move the focus of the zipper one element to the right
right :: Zipper a -> Zipper a
right (Zipper xs []) = Zipper xs []
right (Zipper xs (y:ys)) = Zipper (y:xs) ys

-- | Move the focus of the zipper one elements to the left
left :: Zipper a -> Zipper a
left (Zipper [] ys) = Zipper [] ys
left (Zipper (x:xs) ys) = Zipper xs (x:ys)

-- | Is this zipper at it's end point?
atEnd :: Zipper a -> Bool
atEnd (Zipper _ []) = True
atEnd _ = False


-- | Get the elment cur:ently in focus
get :: Zipper a -> Maybe a
get (Zipper _ []) = Nothing
get (Zipper _ (y:_)) = Just y

set :: Zipper a -> a -> Zipper a
set (Zipper _ []) _ = error "Can't set at end of zipper"
set (Zipper xs (_:ys)) v = Zipper xs (v:ys)

-- | Shift focus to the next (right) element matching a predicate
find :: (Zipper a -> Maybe b) -> Zipper a -> Maybe b
find _ (Zipper _ []) = Nothing
find p z =
    case p z of
      Just result -> Just result
      Nothing -> find p $ right z

