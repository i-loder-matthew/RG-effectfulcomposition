import Data.List (intercalate)

data Syn = Leaf Ty String | Branch Syn Syn
    deriving (Show)

data Ty = E | T | V | Ty :-> Ty | Comp EffX Ty
    deriving (Show, Eq)

data Sem = Lex Ty String | Comb Ty Mode Sem Sem
    deriving (Show)

data EffX 
    = SX       -- set (indeterminacy)
    | Gx Ty    -- reader (read from assignments)
    | Wx Ty    -- writer
    | Cx Ty Ty -- scope/continutations?
    deriving (Show, Eq)

functor :: EffX -> Bool
functor _ = True

data Mode
    = FA
    | BA
    | PM
    | BC
    | RR
    | MR Mode 
    | ML Mode 
    deriving (Show)

modes :: Ty -> Ty -> [(Mode, Ty)]
modes l r = case (l, r) of
    (a :-> b, _      )          | r == a            -> [(FA, b)]        -- Forward application
    (_      , a :-> b)          | l == a            -> [(BA, b)]        -- Backward appliction
    (E :-> T, E :-> T)                              -> [(PM, E :-> T)]  -- Predicate modification
    (c :-> d, a :-> b)          | b == c            -> [(BC, a :-> d)]  -- Backward composition
    (a :-> (b :-> c), d :-> e)  | a == d && c == e  -> [(RR, a :-> c)]


combine :: Ty -> Ty -> [(Mode, Ty)]
combine l r = 
    modes l r ++ 
    addMR l r ++ 
    addML l r

addMR l r = case r of 
    Comp f t | functor f -> [(MR op, Comp f u) | (op, u) <- combine l t]
    _ -> []

addML l r = case l of 
    Comp f s | functor f -> [(ML op, Comp f u) | (op, u) <- combine s r]
    _ -> []

synsem :: Syn -> [Sem]
synsem syn = case syn of 
    (Leaf t w) -> [Lex t w]
    (Branch lsyn rsyn) ->
        [Comb ty op lsem rsem
            | lsem      <- synsem lsyn
            , rsem      <- synsem rsyn
            , (op, ty)  <- combine (getType lsem) (getType rsem)]
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


-- Example usage
main :: IO ()
main = do
    -- Generate semantic trees for `thetallest` and `vmeowed`
    putStrLn "Semantics of `The tallest happy cat meowed`:"
    mapM_ (putStrLn . prettyPrintSem) (synsem syntacticTree)
