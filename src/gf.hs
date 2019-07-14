{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Data.List
import System.Console.GetOpt
import System.Environment

---

data WalkType = Walk1D | Walk2D | Walk2D' Int deriving (Show)
data Mode = GF | MFPT | Total deriving (Show)

data Options = Options
  { type_ :: WalkType
  , mode :: Mode
  , bias :: Double
  , row :: Int
  , col :: Int
  , rowAvg :: Bool
  , help :: Bool
  } deriving (Show)

defaultOpts :: Options
defaultOpts = Options
  { type_ = Walk1D
  , mode = GF
  , bias = 0
  , row = 1
  , col = 0
  , rowAvg = True
  , help = False
  }

options :: [OptDescr (Options -> Options)]
options = 
  [ Option ['1'] [] (NoArg (\opts -> opts {type_ = Walk1D})) "1D Walk"
  , Option ['2'] [] (NoArg (\opts -> opts {type_ = Walk2D})) "2D Walk"
  , Option ['3'] [] (ReqArg (\ws opts -> opts {type_ = Walk2D' (read ws)}) "width")
        "2D quadrant walk with specified constriction width"
  , Option ['h'] ["help"] (NoArg (\opts -> opts {help = True})) "Print this help message"

  , Option ['g'] ["gf", "generating-function"] (NoArg (\opts -> opts {mode = GF}))
        "Output the generating function"
  , Option ['m'] ["mfpt"] (NoArg (\opts -> opts {mode = MFPT}))
        "Output lower bound approximations to MFPT"
  , Option ['t'] ["total"] (NoArg (\opts -> opts {mode = Total}))
        "Output lower bound approximatinos to total probability"

  , Option ['b'] ["bias"] (ReqArg (\bs opts -> opts {bias = read bs}) "bias")
        "Set the bias, e.g. 0.01"
  , Option ['r'] ["row"] (ReqArg (\rs opts -> opts {row = read rs}) "row")
        "Set the row, should usually be positive"
  , Option ['c'] ["col", "column"] (ReqArg (\cs opts -> opts {col = read cs}) "col")
        "Set the column for 2d walks (not row averaged), should be in [0,r]"

  , Option ['s'] ["single", "no-row-average"] (NoArg (\opts -> opts {rowAvg = False}))
        "For 2D summarised modes, don't average over all columns in a row. For GF modes, print a single entry."
  ]

usage :: String -> String
usage progn = usageInfo ("Usage: " ++ progn ++ " [options...]") options

getOpts :: [String] -> Either String Options
getOpts argv =
  case getOpt Permute options argv of
     (o,n,[]  ) -> Right (foldl (flip id) defaultOpts o)
     (_,_,errs) -> Left  (concat errs)

main = do
    argv <- getArgs
    progn <- getProgName
    let usage_ = usage progn

    case getOpts argv of
        Left e -> putStrLn e >> putStr usage_
        Right o -> case o of
            Options {help = True} -> putStr usage_

            Options {mode = GF, type_ = Walk1D, rowAvg = False} -> printWalk $ map (get1 (row o)) walk_1d_iv
            Options {mode = GF, type_ = Walk2D, rowAvg = False} -> printWalk $ map (get2 (row o) (col o)) walk_2d_iii
            Options {mode = GF, type_ = Walk2D' w, rowAvg = False} -> printWalk $ map (get2' (row o) (col o)) (walk_2d' w)

            Options {mode = GF, type_ = Walk1D} -> printWalk walk_1d_iv
            Options {mode = GF, type_ = Walk2D} -> printWalk walk_2d_iii
            Options {mode = GF, type_ = Walk2D' w} -> printWalk (walk_2d' w)

            Options {mode = MFPT, type_ = Walk1D} ->
                mapM_ print $ mfpt_1d (row o) (bias o)
            Options {mode = MFPT, type_ = Walk2D, rowAvg = True} ->
                mapM_ print $ mfpt_2d (row o) (bias o)
            Options {mode = MFPT, type_ = Walk2D, rowAvg = False} ->
                mapM_ print $ mfpt_2d_s (row o) (col o) (bias o)
            Options {mode = MFPT, type_ = Walk2D' w, rowAvg = True} ->
                mapM_ print $ mfpt_2d' w (row o) (bias o)
            Options {mode = MFPT, type_ = Walk2D' w, rowAvg = False} ->
                mapM_ print $ mfpt_2d'_s w (row o) (col o) (bias o)

            Options {mode = Total, type_ = Walk1D} ->
                mapM_ print $ tot_1d (row o) (bias o)
            Options {mode = Total, type_ = Walk2D, rowAvg = True} ->
                mapM_ print $ tot_2d (row o) (bias o)
            Options {mode = Total, type_ = Walk2D, rowAvg = False} ->
                mapM_ print $ tot_2d_s (row o) (col o) (bias o)
            Options {mode = Total, type_ = Walk2D' w, rowAvg = True} ->
                mapM_ print $ tot_2d' w (row o) (bias o)
            Options {mode = Total, type_ = Walk2D' w, rowAvg = False} ->
                mapM_ print $ tot_2d'_s w (row o) (col o) (bias o)

            -- _ -> do
            --     putStrLn "Option set unimplemented..."
            --     putStr "Got: "
            --     print o
            --     putStrLn ""
            --     putStr usage_

  where

    printWalk w = mapM_ putStrLn $ zipWith pwGo [0..] w
    pwGo t x = "t^" ++ show t ++ ": " ++ show x ++ "\n"

    get1 r (XCoeff w) = (w ++ xs) !! r
    get2 r c = get2' (r-c) c
    get2' r c w = (((w ++ xss) !! r) ++ xs) !! c
    xss = repeat []
    xs = repeat 0

    -- let getOpt Permute options args

---

fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

facs = 1 : zipWith (*) facs [1..]

---

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

        showpq m = showp (n-m) ++ showq m
        showp = showx "p"
        showq = showx "q"

pqEval' p q (PQCoeff cs) = sum $ zipWith go cs [0..]
  where
    n = length cs - 1
    go c m = (fromInteger c) * (p^(n-m)) * (q^m)

pqEval p = pqEval' p (1-p)


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

genmfpt walk ev = let ms = 1 : zipWith3 go ms [0..] walk in ms
  where
    go m t w = m + t * ev w

gentot walk ev = let ms = 0 : zipWith go ms walk in ms
  where
    go m w = m + ev w

mfpt_1d n b = genmfpt walk_1d_iv ev
  where
    p = 0.5 * (1 + b)
    ev w = p * pqEval p (ex w)
    ex (XCoeff w) = (w ++ zs) !! n
    zs = repeat z

tot_1d n b = gentot walk_1d_iv ev
  where
    p = 0.5 * (1 + b)
    ev w = p * pqEval p (ex w)
    ex (XCoeff w) = (w ++ zs) !! n
    zs = repeat z

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
         + mmap (p*) (x0 w + y0 w)
    mmap = map . map

    w' ((_:xs):ys) = ((z:xs):ys)

    x = map (z:)
    xb = map tail
    x' = x . w'
    x0 = map (pure . head)

    y = ([]:)
    yb = tail
    y' = y . w'
    y0 = pure . head
    -- y0' = pure . (z:) . tail . head
    -- y0' ((x0:xs):ys) = [z:xs]

mfpt_2d n b = genmfpt walk_2d_iii ev
  where
    rn = 1.0 / (fromIntegral n + 1)
    p = 0.25 * (1 + b)
    q = 0.5 - p
    ef = pqEval' p q
    ex w = sum $ zipWith ex' w [n,n-1..0]
    ex' w m = ef $ (w ++ zs) !! m
    zs = repeat z
    ev w = 0.5 * 2 * p * rn * ex w

mfpt_2d_s n m b = genmfpt walk_2d_iii ev
  where
    p = 0.25 * (1 + b)
    q = 0.5 - p
    ef = pqEval' p q
    ex w = ef $ (((w ++ es) !! m) ++ zs) !! (n-m)
    zs = repeat z
    es = repeat []
    ev w = 0.5 * 2 * p * ex w

tot_2d n b = gentot walk_2d_iii ev
  where
    rn = 1.0 / (fromIntegral n + 1)
    p = 0.25 * (1 + b)
    q = 0.5 - p
    ef = pqEval' p q
    ex w = sum $ zipWith ex' w [n,n-1..0]
    ex' w m = ef $ (w ++ zs) !! m
    zs = repeat z
    ev w = 0.5 * 2 * p * rn * ex w

tot_2d_s n m b = gentot walk_2d_iii ev
  where
    p = 0.25 * (1 + b)
    q = 0.5 - p
    ef = pqEval' p q
    ex w = ef $ (((w ++ es) !! m) ++ zs) !! (n-m)
    zs = repeat z
    es = repeat []
    ev w = 0.5 * 2 * p * ex w

---

walk_2d' width
    | width <= 0 = error "error! constriction width must be positibe"
    | otherwise  = genwalk eps go
  where
    p2 = p + p
    eps0 = replicate width z
    eps1 = [p] ++ replicate (width-1) p2 ++ [p]
    eps = [eps0, eps1]

    go w = mp (rd' w + rc' w)
         + mq (rcb w + rdb w)
         + mp (c0' w + d0' w)
         + mp (r0 w + r0 w)

    mmap = map . map
    mp = mmap (p*)
    mq = mmap (q*)

    w' (_:rs) = eps0:rs

    c = map (z:)
    cb = map tail
    c0 = map (pure . head)
    c0' = c0 . w'

    d = map (++[z])
    db = map init
    d0 = map (\row -> dgo (tail row) (last row))
    dgo xs x = map (const z) xs ++ [x]
    d0' = d0 . w'

    r = (eps0:)
    rb = tail
    r0 = pure . head

    rc' = r . c . w'
    rd' = r . d . w'
    rcb = rb . cb
    rdb = rb . db

mfpt_2d' width n b = genmfpt (walk_2d' width) ev
  where
    n' = n - (width-1)
    rn = 1.0 / (fromIntegral n + 1)
    p = 0.25 * (1 + b)
    q = 0.5 - p
    ef = pqEval' p q
    ex w = sum . map ef $ (w ++ zs) !! n'
    zs = repeat []
    ev w = rn * ex w

mfpt_2d'_s width n m b = genmfpt (walk_2d' width) ev
  where
    n' = n - (width-1)
    p = 0.25 * (1 + b)
    q = 0.5 - p
    ef = pqEval' p q
    ev w = ef $ ((w ++ zs) !! n') !! m
    zs = repeat (repeat z)

tot_2d' width n b = gentot (walk_2d' width) ev
  where
    n' = n - (width-1)
    rn = 1.0 / (fromIntegral n + 1)
    p = 0.25 * (1 + b)
    q = 0.5 - p
    ef = pqEval' p q
    ex w = sum . map ef $ (w ++ zs) !! n'
    zs = repeat []
    ev w = rn * ex w

tot_2d'_s  width n m b = gentot (walk_2d' width) ev
  where
    n' = n - (width-1)
    p = 0.25 * (1 + b)
    q = 0.5 - p
    ef = pqEval' p q
    ev w = ef $ ((w ++ zs) !! n') !! m
    zs = repeat (repeat z)