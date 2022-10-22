module Lib
    ( getProgram ) where
import Language.Abs (Program)
import Language.Par (pProgram, myLexer)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

getProgram :: FilePath -> IO (Either String Program)
getProgram x = fmap (pProgram . myLexer . T.unpack) (TIO.readFile x)