
module ReplaceFunctionParams
    (
        replaceFunctionParams
    ) where
    
import Types
import Errors

replaceSymbol :: Tree -> String -> Tree -> Tree
replaceSymbol (List lst) toReplace with =
    List (map (\t -> if t == Symbol toReplace then with else replaceSymbol t toReplace with) lst)
replaceSymbol t _ _ = t

replaceFunctionParams :: Env -> [String] -> Tree -> [Tree] -> (Env, Maybe Tree)
replaceFunctionParams env params body args
    | length params /= length args = (registerError env "Mismatched number of arguments", Nothing)
    | otherwise =
        let replacement = zip params args
            replacedbody = foldl (\acc (param, arg) -> replaceSymbol acc param arg) body replacement
        in (env, Just replacedbody)
