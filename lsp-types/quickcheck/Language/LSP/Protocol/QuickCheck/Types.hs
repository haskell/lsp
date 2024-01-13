{-# LANGUAGE TemplateHaskell #-}

module Language.LSP.Protocol.QuickCheck.Types where

import Data.Traversable
import Language.Haskell.TH
import Language.LSP.Protocol.Meta
import Language.LSP.Protocol.QuickCheck.Common
import Language.LSP.Protocol.Types
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic

$( let
    sderiving = for structNames $ \sname ->
      let viaT = ConT ''GenericArbitrary `AppT` ConT sname
          arb = ConT ''Arbitrary `AppT` ConT sname
       in pure $ StandaloneDerivD (Just (ViaStrategy viaT)) [] arb
    aderiving = for aliasNames $ \aname ->
      let arb = ConT ''Arbitrary `AppT` ConT aname
       in pure $ StandaloneDerivD (Just NewtypeStrategy) [] arb
    ederiving = for enumNames $ \ename ->
      let viaT = ConT ''AsLspEnum `AppT` ConT ename
          arb = ConT ''Arbitrary `AppT` ConT ename
       in pure $ StandaloneDerivD (Just (ViaStrategy viaT)) [] arb
    in
    mconcat <$> sequence [sderiving, aderiving, ederiving]
 )
