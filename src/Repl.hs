module Repl where
import Eval
import Parser hiding (env)
import Syntax (Top(..), Env(..))
import System.Console.Haskeline
import Control.Monad.IO.Class

process :: String -> Assignments -> Either Assignments DH
process line env = do
  let parsed = parse top "" line
  case parsed of
    Left err -> error $ show err
    Right t -> case t of
      (TExpr e) -> Right $ eval e env
      (TEnv e)  -> Left $ rho e env
      (TDefn d) -> Left $ rho (Defn d) env

repl :: IO ()
repl = runInputT defaultSettings (loop allPrims)
  where
    loop env = do
      i <- getInputLine "FL> "
      case i of
        Nothing -> outputStrLn "Peace out."
        Just input ->
          case process input env of
            (Right o) -> (liftIO $ print o) >> loop env
            (Left env') -> (liftIO $ print "Defined.") >> loop env'
