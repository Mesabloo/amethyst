module Amethyst.REPL.Logger where

import System.Console.ANSI

red :: IO a -> IO a
red action = setSGR [SetColor Foreground Vivid Red] *> action <* setSGR [Reset]

green :: IO a -> IO a
green action = setSGR [SetColor Foreground Vivid Green] *> action <* setSGR [Reset]

blue :: IO a -> IO a
blue action = setSGR [SetColor Foreground Vivid Blue] *> action <* setSGR [Reset]

white :: IO a -> IO a
white action = setSGR [SetColor Foreground Vivid White] *> action <* setSGR [Reset]

underline :: IO a -> IO a
underline action = setSGR [SetUnderlining SingleUnderline] *> action <* setSGR [Reset]

bold :: IO a -> IO a
bold action = setSGR [SetConsoleIntensity BoldIntensity] *> action <* setSGR [Reset]