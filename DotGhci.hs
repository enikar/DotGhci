module DotGhci
  (escapeShellArg
  ,lb
  ,escapeShellHTMLArg
  ,pwd
  ,myPrint
  ,docs
  ,pkg
  )
  where

import qualified Language.Haskell.HsColour as HSC
import qualified Language.Haskell.HsColour.Colourise as HSC
import Language.Haskell.HsColour.Output (TerminalType(..))
import qualified Text.Show.Pretty
import qualified GHC.Paths
import System.Directory (getCurrentDirectory)

escapeShellArg :: Foldable t => t Char -> String
escapeShellArg arg = "'" ++ concatMap trans arg ++ "'"
  where trans '\'' = "'\\''"
        trans c    = [c]

escapeShellHTMLArg :: Foldable t => t Char -> String
escapeShellHTMLArg arg = "'" ++ concatMap trans arg ++ "'"
  where
    trans c = case c of  
                '\'' -> "'\\''"
                '>'  -> "&gt;"
                '<'  -> "&lt;"
                '&'  -> "&amp;"
                _   -> [c]


lb :: (Monad m, Foldable t) => t Char -> t Char -> m String
lb s1 s2 = return $
  ":!lambdabot -n -e "
  ++ escapeShellArg s1
  ++ "\\ "
  ++ escapeShellArg s2

docs :: (Monad m, Foldable t) => t Char -> m String
docs s = return $
  ":!echo file://"
  ++ GHC.Paths.docdir
  ++ "/../$(tr \\< \\\\n < "
  ++ GHC.Paths.docdir
  ++ "/../doc-index-All.html | grep -A100 -F \\>"
  ++ escapeShellHTMLArg s
  ++ " | grep href | head -1 | cut -d\\\" -f2)"

pwd  :: IO String
pwd = getCurrentDirectory 

-- pretty printer with colors:
-- Colourise ghci output (use :nopretty to disable)
-- Required libraries: pretty-show hscolour

myPrint :: (Show a) => a -> IO ()
myPrint a = putStrLn $
  HSC.hscolour (HSC.TTYg XTerm256Compatible)
               myColourPrefs
               False
               False
               ""
               False
               (Text.Show.Pretty.ppShow a)

myColourPrefs :: HSC.ColourPrefs
-- myColourPrefs = HSC.defaultColourPrefs
--   { HSC.conop    = [HSC.Foreground HSC.Yellow]
--   , HSC.conid    = [HSC.Foreground $ HSC.Rgb  70 130 180, HSC.Bold]
--   , HSC.string   = [HSC.Foreground $ HSC.Rgb 255 160 122]
--   , HSC.char     = [HSC.Foreground HSC.Cyan]
--   , HSC.number   = [HSC.Foreground $ HSC.Rgb 30 144 255]
--   , HSC.keyglyph = [HSC.Foreground HSC.Yellow]
--   , HSC.variantselection = [HSC.Foreground $ HSC.Rgb 29 193 57]
--   , HSC.cpp = [ HSC.Foreground HSC.Magenta  ]
--   }

myColourPrefs = HSC.defaultColourPrefs
  { HSC.conop    = [HSC.Foreground HSC.Yellow]
  , HSC.varop    = [HSC.Foreground $ HSC.Rgb 255 99 71] -- tomato
  , HSC.conid    = [HSC.Foreground $ HSC.Rgb  70 130 180] -- ligth steel blue
  , HSC.string   = [HSC.Foreground $ HSC.Rgb 250 160 132] -- salmon
  , HSC.char     = [HSC.Foreground $ HSC.Rgb 250 160 132] -- salmon
  , HSC.number   = [HSC.Foreground $ HSC.Rgb 30 144 255]  -- hodger blue 
  , HSC.keyglyph = [HSC.Foreground HSC.Yellow]
  , HSC.variantselection = [HSC.Foreground $ HSC.Rgb 255 99 71] -- tomato
  , HSC.cpp = [HSC.Foreground $ HSC.Rgb 32 178 170] -- light sea green
  }

pkg :: String
pkg = ":!" ++ GHC.Paths.ghc_pkg ++ " "
