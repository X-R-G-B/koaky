
module ReplaceFunctionParams
    (
        replaceFunctionParams
    ) where
    
import Types
import Errors

replaceSymbol :: Tree -> String -> Tree -> Tree
replaceSymbol (Symbol smbl) toReplace replacement
    | smbl == toReplace = replacement
    | otherwise = Symbol smbl
replaceSymbol (List lst) toReplace replacement
    = List (map (\t -> replaceSymbol t toReplace replacement) lst)
replaceSymbol t _ _ = t

replaceFunctionParams :: Env -> [String] -> Tree -> [Tree] -> (Env, Maybe Tree)
replaceFunctionParams env fnParams body args
    | length fnParams /= length args = (registerError env "Mismatched number of arguments", Nothing)
    | otherwise =
        let replacement = zip fnParams args
            replacedbody = foldl (\acc (param, arg) -> replaceSymbol acc param arg) body replacement
        in (env, Just replacedbody)
