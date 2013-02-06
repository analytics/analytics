-- | This represents a sequence of bits pulled in a prioritized fashion from different schedules.
--
-- One invariant is that if X occurs before Y in one list of indices and in another list of indices
-- then the Z-order relative addressing will be consistent in that the same bits of X will occur
-- before the same bits of Y, irrespective of other keys being interleaved. This is an important
-- property to maintain for joins.
data Supply f
  = Supply {-# UNPACK #-} !Int (f Int64) (Supply f)
  | Done
  deriving Show

supplyN :: Functor f => Node f -> Heap f -> Supply f
supplyN n@(Node p s w i b) (Heap m@(Node p' s' _ _ _) ts) = case (p' - p) `quotRem` w of
  (q,r) | k <- if r == 0 && s <= s' then q + 1 else q -> Supply (min i k) (b .&. (bit i - 1)) $
    if i > k
    then supplyN m $ meldWithNode (Node (p + q + 1) s w (i - k) shiftR b k) ts
    else case ts of
      []     -> finish m
      (z:zs) -> supplyN m (meldWithHeap z zs)
{-# INLINEABLE supplyN #-}

finish :: Node f -> Supply f
finish (Node _ _ _ j b) = Supply j b Done
{-# INLINE finish #-}

supply :: Functor f => Heap f -> Supply f
supply (Heap n (t:ts)) = supplyN n (meldWithHeap t ts)
supply (Heap n [])     = finish n
{-# INLINE supply #-}

meldWithNode :: Node f -> [Heap f] -> Heap f
meldWithNode n (t2:ts) = insert n (meld t2 ts)
meldWithNode n []      = Heap n []
{-# INLINE meldWithNode #-}

meldWithHeap :: Heap f -> [Heap f] -> Heap f
meldWithHeap t (t2:ts) = t1 <> meldWithHeap t2 ts
meldWithHeap t []      = t
{-# INLINE meldWithHeap #-}

data Node f = Node
  { _nodePriority   :: {-# UNPACK #-} !Int
  , _nodeSequence   :: {-# UNPACK #-} !Int
  , _nodeStride     :: {-# UNPACK #-} !Int
  , _nodeRemainding :: {-# UNPACK #-} !Int
  , _nodeBits       :: f Int64
  } deriving Show

makeClassy ''Node

instance Eq (Node f) where
  Node p s _ _ _ == Node q t _ _ _ = p == q && s == t
  {-# INLINE (==) #-}

instance Ord (Node f) where
  Node p s _ _ _ `compare` Node q t _ _ _ = compare p q <> compare s t
  {-# INLINE compare #-}

-- a non-empty pairing heap
data Heap f = Heap
  { _heapNode :: {-# UNPACK #-} !(Node f)
  , _heapChildren :: [Heap f]
  } deriving Show

makeLenses ''Heap

instance HasNode (Heap f) f where
  node = heapNode

instance Semigroup (Heap f) where
  x@(Heap m xs) <> y@(Heap n ys)
    | m <= n    = Heap m (y:xs)
    | otherwise = Heap n (x:ys)
  {-# INLINE (<>) #-}

insert :: Node f -> Heap f -> Heap f
insert n (Heap m xs)
  | n <= m    = Heap n [Heap m xs]
  | otherwise = Heap m (Heap n []:xs)
{-# INLINE insert #-}

fromList :: [Node f] -> Heap f
fromList xs = foldr insert Tip xs
{-# INLINE fromList #-}

extractMin :: Heap f -> (Node f, Maybe (Heap f))
extractMin (Heap n ts) = (n, meld ts) where
  meld (t1:t2:ts) = Just $ (t1 <> t2) <> meld ts
  meld [t]        = Just t
  meld []         = Nothing
{-# INLINE extractMin #-}

node :: Int -> Schedule a -> Morton f a -> Node f
node s (Schedule p w i _ _) (Morton fa) = Node p s w i fa
{-# INLINE node #-}

interleave :: List Schedule as -> List (Morton f) as -> Supply f
interleave Nil    Nil               = Done
interleave (Cons x xs) (Cons a as)  = supply (go 0 x a xs as) where
  go !n y b (Cons z zs) (Cons c cs) = insert (node n y b) (go (n + 1) z c zs cs)
  go !n y b []                      = Heap (node n y b) []
{-# INLINE interleave #-}

newtype Morton f a = Morton { runMorton :: f Int64 }
  deriving Show
