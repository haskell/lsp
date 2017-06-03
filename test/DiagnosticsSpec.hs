{-# LANGUAGE OverloadedStrings #-}
module DiagnosticsSpec where


import qualified Data.Map as Map
-- import qualified Data.Text as T
import           Data.Text ( Text )
import           Language.Haskell.LSP.Diagnostics
import qualified Language.Haskell.LSP.TH.DataTypesJSON as J

import           Test.Hspec

-- ---------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Diagnostics functions" diagnosticsSpec

-- -- |Used when running from ghci, and it sets the current directory to ./tests
-- tt :: IO ()
-- tt = do
--   cd ".."
--   hspec spec

-- ---------------------------------------------------------------------

mkDiagnostic :: Maybe J.DiagnosticSource -> Text -> J.Diagnostic
mkDiagnostic ms str = J.Diagnostic (J.Range (J.Position 0 1) (J.Position 3 0)) Nothing Nothing ms str

-- ---------------------------------------------------------------------

diagnosticsSpec :: Spec
diagnosticsSpec = do
  describe "constructs a new store" $ do
    it "constructs a store with no doc version and a single source" $ do
      let
        diags =
          [ mkDiagnostic (Just "hlint") "a"
          , mkDiagnostic (Just "hlint") "b"
          ]
      (updateDiagnostics Map.empty "uri" Nothing (partitionBySource diags)) `shouldBe`
        Map.fromList
          [ ("uri",StoreItem Nothing $ Map.fromList [(Just "hlint", reverse diags) ] )
          ]

    -- ---------------------------------

    it "constructs a store with no doc version and multiple sources" $ do
      let
        diags =
          [ mkDiagnostic (Just "hlint") "a"
          , mkDiagnostic (Just "ghcmod") "b"
          ]
      (updateDiagnostics Map.empty "uri" Nothing (partitionBySource diags)) `shouldBe`
        Map.fromList
          [ ("uri",StoreItem Nothing $ Map.fromList
                [(Just "hlint",  [mkDiagnostic (Just "hlint")  "a"])
                ,(Just "ghcmod", [mkDiagnostic (Just "ghcmod") "b"])
                ])
          ]

    -- ---------------------------------

    it "constructs a store with doc version and multiple sources" $ do
      let
        diags =
          [ mkDiagnostic (Just "hlint") "a"
          , mkDiagnostic (Just "ghcmod") "b"
          ]
      (updateDiagnostics Map.empty "uri" (Just 1) (partitionBySource diags)) `shouldBe`
        Map.fromList
          [ ("uri",StoreItem (Just 1) $ Map.fromList
                [(Just "hlint",  [mkDiagnostic (Just "hlint")  "a"])
                ,(Just "ghcmod", [mkDiagnostic (Just "ghcmod") "b"])
                ])
          ]

    -- ---------------------------------

  describe "updates a store for same document version" $ do
    it "updates a store without a document version, single source only" $ do
      let
        diags1 =
          [ mkDiagnostic (Just "hlint") "a1"
          , mkDiagnostic (Just "hlint") "b1"
          ]
        diags2 =
          [ mkDiagnostic (Just "hlint") "a2"
          ]
      let origStore = updateDiagnostics Map.empty "uri" Nothing (partitionBySource diags1)
      (updateDiagnostics origStore "uri" Nothing (partitionBySource diags2)) `shouldBe`
        Map.fromList
          [ ("uri",StoreItem Nothing $ Map.fromList [(Just "hlint", diags2) ] )
          ]

    -- ---------------------------------

    it "updates just one source of a 2 source store" $ do
      let
        diags1 =
          [ mkDiagnostic (Just "hlint") "a1"
          , mkDiagnostic (Just "ghcmod") "b1"
          ]
        diags2 =
          [ mkDiagnostic (Just "hlint") "a2"
          ]
      let origStore = updateDiagnostics Map.empty "uri" Nothing (partitionBySource diags1)
      (updateDiagnostics origStore "uri" Nothing (partitionBySource diags2)) `shouldBe`
        Map.fromList
          [ ("uri",StoreItem Nothing $ Map.fromList
                [(Just "hlint",  [mkDiagnostic (Just "hlint")  "a2"])
                ,(Just "ghcmod", [mkDiagnostic (Just "ghcmod") "b1"])
              ] )
          ]

    -- ---------------------------------

    it "updates just one source of a 2 source store, with empty diags" $ do
      let
        diags1 =
          [ mkDiagnostic (Just "hlint") "a1"
          , mkDiagnostic (Just "ghcmod") "b1"
          ]
      let origStore = updateDiagnostics Map.empty "uri" Nothing (partitionBySource diags1)
      (updateDiagnostics origStore "uri" Nothing (Map.fromList [(Just "ghcmod", [])])) `shouldBe`
        Map.fromList
          [ ("uri",StoreItem Nothing $ Map.fromList
                [(Just "ghcmod", [])
                ,(Just "hlint",  [mkDiagnostic (Just "hlint")  "a1"])
                ] )
          ]


    -- ---------------------------------

  describe "updates a store for a new document version" $ do
    it "updates a store without a document version, single source only" $ do
      let
        diags1 =
          [ mkDiagnostic (Just "hlint") "a1"
          , mkDiagnostic (Just "hlint") "b1"
          ]
        diags2 =
          [ mkDiagnostic (Just "hlint") "a2"
          ]
      let origStore = updateDiagnostics Map.empty "uri" (Just 1) (partitionBySource diags1)
      (updateDiagnostics origStore "uri" (Just 2) (partitionBySource diags2)) `shouldBe`
        Map.fromList
          [ ("uri",StoreItem (Just 2) $ Map.fromList [(Just "hlint", diags2) ] )
          ]

    -- ---------------------------------

    it "updates a store for a new doc version, removing all priot sources" $ do
      let
        diags1 =
          [ mkDiagnostic (Just "hlint") "a1"
          , mkDiagnostic (Just "ghcmod") "b1"
          ]
        diags2 =
          [ mkDiagnostic (Just "hlint") "a2"
          ]
      let origStore = updateDiagnostics Map.empty "uri" (Just 1) (partitionBySource diags1)
      (updateDiagnostics origStore "uri" (Just 2) (partitionBySource diags2)) `shouldBe`
        Map.fromList
          [ ("uri",StoreItem (Just 2) $ Map.fromList
                [(Just "hlint",  [mkDiagnostic (Just "hlint")  "a2"])
              ] )
          ]

    -- ---------------------------------

  describe "retrieves all the diagnostics for a given uri" $ do

    it "gets diagnostics for multiple sources" $ do
      let
        diags =
          [ mkDiagnostic (Just "hlint") "a"
          , mkDiagnostic (Just "ghcmod") "b"
          ]
      let ds = updateDiagnostics Map.empty "uri" (Just 1) (partitionBySource diags)
      (getDiagnosticParamsFor ds "uri") `shouldBe`
        Just (J.PublishDiagnosticsParams "uri" (J.List $ reverse diags))

    -- ---------------------------------
