module Main (main) where

import System.Environment (getArgs)
import System.Exit (die)
import System.IO (hPutStrLn, stderr)

import qualified Language.Python.Version3 as Py3
import qualified ConvertAST
import qualified PMMAST as PMM

main :: IO ()
main = do
  args <- getArgs
  path <- parseArgs args
  input <- readFile path
  runPipeline path input

parseArgs :: [String] -> IO FilePath
parseArgs args =
  case args of
    [path] -> pure path
    _ ->
      die "Usage: run-pmm <python-file>"

runPipeline :: FilePath -> String -> IO ()
runPipeline path input =
  case Py3.parseModule input path of
    Left parseErr ->
      die $
        "Python parse failed for " ++ path ++ ":\n" ++ show parseErr

    Right (pyModule, comments) -> do
      hPutStrLn stderr $
        "Parsed Python module successfully."
      hPutStrLn stderr $
        "Comment token count: " ++ show (length comments)

      case ConvertAST.convertModule pyModule of
        Left convertErr ->
          die $
            "PMM conversion failed:\n" ++ convertErr

        Right pmmModule -> do
          hPutStrLn stderr "Converted to PythonMinusMinus AST successfully."
          printPMM pmmModule

          -- Next step:
          -- mlirModule <- lowerModule pmmModule
          -- putStrLn (renderMLIR mlirModule)
          return ()

printPMM :: Show a => PMM.Module a -> IO ()
printPMM = print
