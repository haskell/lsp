{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Language.LSP.Types.Internal.Lenses where

import           Language.LSP.MetaModel.CodeGen        (genLenses)
import           Language.LSP.Types.Internal.Generated

$(genLenses structNames)
