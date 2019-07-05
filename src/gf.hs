{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Data.List

---

-- main = print $ map (head.head) walk_2d_iii
main = print fibs

---

fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

facs = 1 : zipWith (*) facs [1..]

zipFull two one = go
  where
    go []     ys     = map one ys
    go xs     []     = map one xs
    go (x:xs) (y:ys) = (two x y) : go xs ys


instance Num a => Num [a] where
    (+) = zipFull (+) id
    (*) = zipWith (*)
    abs = map abs
    signum = map signum
    fromInteger n = [fromInteger n]
    negate = map negate

genwalk e go = let w = e : map go w in w

walk_1d_i = genwalk [1] (\w -> (0:w) + (tail w))
walk_1d_ii = genwalk [0,1] (\(w0:w) -> (0:0:w) + (w) + [w0])


newtype PQCoeff = PQCoeff [Integer]
p = PQCoeff [1,0]
q = PQCoeff [0,1]
e = PQCoeff [1]
z = PQCoeff []

instance Num PQCoeff where
    (PQCoeff xs) + (PQCoeff ys)
        | length xs == length ys = PQCoeff $ zipWith (+) xs ys
        | null' xs = PQCoeff ys
        | null' ys = PQCoeff xs
        | otherwise = error $ show (PQCoeff xs,PQCoeff ys)
        where null' = all (==0)
    (PQCoeff xs) * (PQCoeff ys) = PQCoeff $ case uni 0 xs of
                Just Nothing -> []
                Just (Just (n,c,m)) -> replicate n 0 ++ map (c*) ys ++ replicate m 0
                Nothing -> case uni 0 ys of
                    Just Nothing -> []
                    Just (Just (n,c,m)) -> replicate n 0 ++ map (c*) xs ++ replicate m 0

      where
        uni n [] = Just Nothing
        uni n (0:zs) = uni (n+1) zs
        uni n (c:zs) = uni' n c 0 zs

        uni' n c m [] = Just (Just (n,c,m))
        uni' n c m (0:zs) = uni' n c (m+1) zs
        uni' n c m (_:zs) = Nothing

    abs (PQCoeff xs) = PQCoeff $ map abs xs
    signum (PQCoeff xs) = PQCoeff $ map signum xs
    fromInteger n = PQCoeff [fromInteger n]
    negate (PQCoeff xs) = PQCoeff $ map negate xs


showx x 0 = ""
showx x 1 = x
showx x m = x ++ "^" ++ show m

instance Show PQCoeff where
    show (PQCoeff []) = ""
    show (PQCoeff pq) = intercalate " + " . filter (not . null) $ zipWith go pq [0..]
      where
        n = length pq - 1
        go 0 _ = ""
        go 1 m = case showpq m of
                   "" -> "1"
                   s  -> s
        go c m = show c ++ showpq m
        -- go c 0 | n == 0    = show c
        --        | otherwise = show c ++ "p^" ++ show n
        -- go c m | m == n    = show c ++ "q^" ++ show m
        --        | otherwise = show c ++ "p^" ++ show (n-m) ++ "q^" ++ show m

        showpq m = showp (n-m) ++ showq m
        showp = showx "p"
        showq = showx "q"

newtype XCoeff = XCoeff [PQCoeff]

instance Show XCoeff where
    show (XCoeff []) = ""
    show (XCoeff xs) = intercalate " + " . filter (not . null) $ zipWith show' xs [0..]
      where
        show' x n = case show x of
                      ""  -> ""
                      "0" -> ""
                      "1" -> case showxx n of
                               "" -> "1"
                               s  -> s
                      s  -> if '+' `elem` s then "(" ++ s ++ ")" ++ showxx n
                                            else s ++ showxx n
        showxx = showx "x"

walk_1d_iii = map XCoeff $ genwalk [e] go
  where
    go w = map (p*) (x w) + map (q*) (xb w)

    x = (z:)
    xb = tail

walk_1d_iv = map XCoeff $ genwalk [z,e] go
  where
    go w = map (p*) (x' w) + map (q*) (xb w) + [p * x0 w]

    x = (z:)
    x' = x . x . xb
    xb = tail
    x0 = head

------

walk_2d_i = genwalk [[1]] go
  where
    go w = x w + y w + xb w + yb w
    
    x yxs = map (0:) yxs
    y yxs = []:yxs
    xb yxs = map tail yxs
    yb yxs = tail yxs

walk_2d_ii = genwalk [[e]] go
  where
    go w = mmap (p*) (x w + y w) + mmap (q*) (xb w + yb w)
    mmap = map . map

    x yxs = map (0:) yxs
    y yxs = []:yxs
    xb yxs = map tail yxs
    yb yxs = tail yxs

walk_2d_iii = genwalk [[z,e],[e]] go
  where
    go w = mmap (p*) (x' w + y' w)
         + mmap (q*) (xb w + yb w)
         + mmap (p*) (x0 w + y0' w)
    mmap = map . map

    w' ((_:xs):ys) = ((z:xs):ys)

    x = map (z:)
    xb = map tail
    x' = x . w'
    x0 = map (pure . head)

    y = ([]:)
    yb = tail
    y' = y . w'
    -- y0 = pure . head
    y0' = pure . (z:) . tail . head

