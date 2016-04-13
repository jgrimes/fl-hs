module Repl where
import Eval
import Parser hiding (env)
import Syntax (Top(..), Env(..))
import System.Console.Haskeline
import Control.Monad.IO.Class

-- TODO Fix all of this

process :: String -> Assignments -> Either ParseError (Either Assignments DH)
process line env = do
  let parsed = parse top "" line
  case parsed of
    Left err -> Left err
    Right t -> case t of
      (TExpr e) -> Right $ Right $ eval e env
      (TEnv e)  -> Right $ Left $ rho e env
      (TDefn d) -> Right $ Left $ rho (Defn d) env

command :: String -> Assignments -> IO (Maybe String)
command ":defs" as = do print as; return $ Just (show as)
command ":quit" _ = return (Just ":quit")
command _ _ = return Nothing

repl :: IO ()
repl = runInputT defaultSettings (loop allPrims)
  where
    loop env = do
      i <- getInputLine "FL> "
      case i of
        Nothing -> outputStrLn "Peace out."
        Just input -> do
          c <- liftIO $ command input env
          case c of
            (Just ":quit") -> outputStrLn "Laterrrr."
            (Just _) -> loop env
            Nothing ->
              case process input env of
                (Left err) -> (liftIO $ print err) >> loop env
                (Right out) -> case out of
                  (Right o) -> (liftIO $ print o) >> loop env
                  (Left env') -> (liftIO $ print "Defined.") >> loop (env' -+- env)
