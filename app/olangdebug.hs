import CodeGenerator ( Generatable(generate) )
import Machine ( runDebug )
import Parser ( ParseResult, Parseable(parse) )
import SyntaxTree ( Program )
import System.Environment ( getArgs )
import System.IO ( stdout, hSetBuffering, BufferMode(NoBuffering) )
import Tokenizer ( tokenize )

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  input <- readFile (head args)
  putStrLn "olang-debug"
  case tokenize input of
    Left e -> putStrLn $ "Lexical error: " ++ show e
    Right t -> do
      putStrLn "Successful tokenization."
      putStrLn $ "Result: " ++ show (fst <$> t)
      case parse t :: ParseResult Program of
        Left e -> putStrLn ("Parse error: " ++ show e)
        Right pro -> do
          putStrLn $ "Successful parsing.\nResult: " ++ show pro
          case generate pro of
            Left e -> putStrLn $ "Compilation error: " ++ e
            Right cmds -> do
              putStrLn $ "Successful compilation.\nResult: " ++ show cmds
              runDebug cmds
  putStrLn "Program terminated."
