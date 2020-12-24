module Main where

import Assembler (compile)
import Translator (translateVM)
import qualified Data.Text.IO as I
import qualified System.IO

main :: IO ()
main = do
    fileName <- getLine

    contents <- I.readFile fileName
    -- let dat = compile contents
    let dat = translateVM fileName contents

    I.writeFile (fileName ++ ".asm") dat