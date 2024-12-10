import Data.List (intercalate)

data Syn = Leaf Ty String | Branch Syn Syn
    deriving (Show)

data Ty = E | T | V | Ty :-> Ty
    deriving (Show, Eq)

data Sem = Lex Ty String | Comb Ty Mode Sem Sem
    deriving (Show)

data Mode
    = FA
    | BA
    | PM
    | BC
    | RR
    deriving (Show)

modes :: Ty -> Ty -> [(Mode, Ty)]
modes l r = case (l, r) of
    (a :-> b, _      )          | r == a            -> [(FA, b)]        -- Forward application
    (_      , a :-> b)          | l == a            -> [(BA, b)]        -- Backward appliction
    (E :-> T, E :-> T)                              -> [(PM, E :-> T)]  -- Predicate modification
    (c :-> d, a :-> b)          | b == c            -> [(BC, a :-> d)]  -- Backward composition
    (a :-> (b :-> c), d :-> e)  | a == d && c == e  -> [(RR, a :-> c)]


synsem :: Syn -> [Sem]
synsem syn = case syn of 
    (Leaf t w) -> [Lex t w]
    (Branch lsyn rsyn) ->
        [Comb ty op lsem rsem
            | lsem      <- synsem lsyn
            , rsem      <- synsem rsyn
            , (op, ty)  <- modes (getType lsem) (getType rsem)]
    where
        getType (Comb ty _ _ _) = ty
        getType (Lex ty _) = ty


thetallest :: Syn
thetallest = Branch (Leaf ((E :-> T ) :-> T) "the") (Leaf ((E :-> T) :-> (E :-> T)) "tallest")

vmeowed :: Syn
vmeowed = Branch (Leaf (E :-> (V :-> T)) "v") (Leaf (E :-> T) "meowed")

syntacticTree :: Syn
syntacticTree =
    Branch
        (Branch
            (Branch
                (Leaf ((E :-> T) :-> E) "the")
                (Leaf ((E :-> T) :-> (E :-> T)) "tallest")
            )
            (Branch
                (Leaf (E :-> T) "happy")
                (Leaf (E :-> T) "cat")
            )
        )
        (Branch
            (Leaf (E :-> (V :-> T)) "v")
            (Leaf (E :-> T) "meowed")
        )

prettyPrintSem :: Sem -> String
prettyPrintSem = prettyPrintSem' 0
  where
    -- Helper function to manage indentation
    prettyPrintSem' :: Int -> Sem -> String
    prettyPrintSem' indent sem = case sem of
        Lex ty word ->
            replicate indent ' ' ++ "Lex " ++ show ty ++ " " ++ show word
        Comb ty mode left right ->
            replicate indent ' ' ++ "Comb " ++ show ty ++ " (" ++ show mode ++ ")\n" ++
            prettyPrintSem' (indent + 2) left ++ "\n" ++
            prettyPrintSem' (indent + 2) right


-- -- Pretty-print semantic trees as a tree-like structure
-- prettyPrintTree :: Sem -> String
-- prettyPrintTree sem = unlines (buildTree sem)

-- -- Helper function to recursively build the tree
-- buildTree :: Sem -> [String]
-- buildTree sem = case sem of
--     Lex ty word ->
--         [nodeText ("Lex " ++ show ty ++ " " ++ show word)]
--     Comb ty mode left right ->
--         let
--             leftTree  = buildTree left
--             rightTree = buildTree right
--             combined  = zipTrees leftTree rightTree
--         in
--             nodeText ("Comb " ++ show ty ++ " (" ++ show mode ++ ")") : combined

-- -- Format a node's text with spaces for alignment
-- nodeText :: String -> String
-- nodeText txt = " " ++ txt ++ " "

-- -- Combine two subtrees into a single tree-like structure
-- zipTrees :: [String] -> [String] -> [String]
-- zipTrees left right =
--     let
--         width = maximum (map length left ++ map length right)
--         leftPadded  = map (padToWidth width) left
--         rightPadded = map (padToWidth width) right
--         connector   = replicate (width `div` 2) ' ' ++ "|" ++ replicate (width `div` 2) ' '
--         merged      = zipWith (\l r -> l ++ " " ++ r) leftPadded rightPadded
--     in connector : merged

-- -- Helper to pad strings to a specified width
-- padToWidth :: Int -> String -> String
-- padToWidth n str = str ++ replicate (n - length str) ' '

-- Example usage
main :: IO ()
main = do
    -- Generate semantic trees for `thetallest` and `vmeowed`
    putStrLn "Semantics of `The tallest happy cat meowed`:"
    mapM_ (putStrLn . prettyPrintSem) (synsem syntacticTree)