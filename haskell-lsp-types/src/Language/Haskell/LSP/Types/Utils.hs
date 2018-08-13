module Language.Haskell.LSP.Types.Utils where

-- ---------------------------------------------------------------------

rdrop :: Int -> [a] -> [a]
rdrop cnt = reverse . drop cnt . reverse
