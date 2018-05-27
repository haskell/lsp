{-# LANGUAGE OverloadedStrings #-}
module DiagnosticsSpec where


import qualified Data.Map                              as Map
import qualified Data.SortedList                       as SL
import           Data.Text                             (Text)
import           Language.Haskell.LSP.Diagnostics
import qualified Language.Haskell.LSP.Types            as J

import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

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
mkDiagnostic ms str =
  let
    rng = J.Range (J.Position 0 1) (J.Position 3 0)
    loc = J.Location (J.Uri "file") rng
  in
    J.Diagnostic rng Nothing Nothing ms str (Just (J.List [J.DiagnosticRelatedInformation loc str]))

mkDiagnostic2 :: Maybe J.DiagnosticSource -> Text -> J.Diagnostic
mkDiagnostic2 ms str =
  let
    rng = J.Range (J.Position 4 1) (J.Position 5 0)
    loc = J.Location (J.Uri "file") rng
  in J.Diagnostic rng Nothing Nothing ms str (Just (J.List [J.DiagnosticRelatedInformation loc str]))

-- ---------------------------------------------------------------------

diagnosticsSpec :: Spec
diagnosticsSpec = do
  describe "constructs a new store" $ do
    it "constructs a store with no doc version and a single source" $ do
      let
        diags =
          [ SD NoCodeAction (mkDiagnostic (Just "hlint") "a")
          , SD NoCodeAction (mkDiagnostic (Just "hlint") "b")
          ]
      (updateDiagnostics Map.empty (J.Uri "uri") Nothing (partitionBySource diags)) `shouldBe`
        Map.fromList
          [ ((J.Uri "uri"),StoreItem Nothing $ Map.fromList [(Just "hlint", SL.toSortedList diags) ] )
          ]

    -- ---------------------------------

    it "constructs a store with no doc version and multiple sources" $ do
      let
        diags =
          [ SD NoCodeAction (mkDiagnostic (Just "hlint") "a")
          , SD NoCodeAction (mkDiagnostic (Just "ghcmod") "b")
          ]
      (updateDiagnostics Map.empty (J.Uri "uri") Nothing (partitionBySource diags)) `shouldBe`
        Map.fromList
          [ ((J.Uri "uri"),StoreItem Nothing $ Map.fromList
                [(Just "hlint",  SL.singleton (SD NoCodeAction (mkDiagnostic (Just "hlint")  "a")))
                ,(Just "ghcmod", SL.singleton (SD NoCodeAction (mkDiagnostic (Just "ghcmod") "b")))
                ])
          ]

    -- ---------------------------------

    it "constructs a store with doc version and multiple sources" $ do
      let
        diags =
          [ SD NoCodeAction (mkDiagnostic (Just "hlint") "a")
          , SD NoCodeAction (mkDiagnostic (Just "ghcmod") "b")
          ]
      (updateDiagnostics Map.empty (J.Uri "uri") (Just 1) (partitionBySource diags)) `shouldBe`
        Map.fromList
          [ ((J.Uri "uri"),StoreItem (Just 1) $ Map.fromList
                [(Just "hlint",  SL.singleton (SD NoCodeAction (mkDiagnostic (Just "hlint")  "a")))
                ,(Just "ghcmod", SL.singleton (SD NoCodeAction (mkDiagnostic (Just "ghcmod") "b")))
                ])
          ]

    -- ---------------------------------

  describe "updates a store for same document version" $ do
    it "updates a store without a document version, single source only" $ do
      let
        diags1 =
          [ SD NoCodeAction (mkDiagnostic (Just "hlint") "a1")
          , SD NoCodeAction (mkDiagnostic (Just "hlint") "b1")
          ]
        diags2 =
          [ SD NoCodeAction (mkDiagnostic (Just "hlint") "a2")
          ]
      let origStore = updateDiagnostics Map.empty (J.Uri "uri") Nothing (partitionBySource diags1)
      (updateDiagnostics origStore (J.Uri "uri") Nothing (partitionBySource diags2)) `shouldBe`
        Map.fromList
          [ ((J.Uri "uri"),StoreItem Nothing $ Map.fromList [(Just "hlint", SL.toSortedList diags2) ] )
          ]

    -- ---------------------------------

    it "updates just one source of a 2 source store" $ do
      let
        diags1 =
          [ SD NoCodeAction (mkDiagnostic (Just "hlint") "a1")
          , SD NoCodeAction (mkDiagnostic (Just "ghcmod") "b1")
          ]
        diags2 =
          [ SD NoCodeAction (mkDiagnostic (Just "hlint") "a2")
          ]
      let origStore = updateDiagnostics Map.empty (J.Uri "uri") Nothing (partitionBySource diags1)
      (updateDiagnostics origStore (J.Uri "uri") Nothing (partitionBySource diags2)) `shouldBe`
        Map.fromList
          [ ((J.Uri "uri"),StoreItem Nothing $ Map.fromList
                [(Just "hlint",  SL.singleton (SD NoCodeAction (mkDiagnostic (Just "hlint")  "a2")))
                ,(Just "ghcmod", SL.singleton (SD NoCodeAction (mkDiagnostic (Just "ghcmod") "b1")))
              ] )
          ]

    -- ---------------------------------

    it "updates just one source of a 2 source store, with empty diags" $ do
      let
        diags1 =
          [ SD NoCodeAction (mkDiagnostic (Just "hlint") "a1")
          , SD NoCodeAction (mkDiagnostic (Just "ghcmod") "b1")
          ]
      let origStore = updateDiagnostics Map.empty (J.Uri "uri") Nothing (partitionBySource diags1)
      (updateDiagnostics origStore (J.Uri "uri") Nothing (Map.fromList [(Just "ghcmod",SL.toSortedList [])])) `shouldBe`
        Map.fromList
          [ ((J.Uri "uri"),StoreItem Nothing $ Map.fromList
                [(Just "ghcmod", SL.toSortedList [])
                ,(Just "hlint",  SL.singleton (SD NoCodeAction (mkDiagnostic (Just "hlint")  "a1")))
                ] )
          ]


    -- ---------------------------------

  describe "updates a store for a new document version" $ do
    it "updates a store without a document version, single source only" $ do
      let
        diags1 =
          [ SD NoCodeAction (mkDiagnostic (Just "hlint") "a1")
          , SD NoCodeAction (mkDiagnostic (Just "hlint") "b1")
          ]
        diags2 =
          [ SD NoCodeAction (mkDiagnostic (Just "hlint") "a2")
          ]
      let origStore = updateDiagnostics Map.empty (J.Uri "uri") (Just 1) (partitionBySource diags1)
      (updateDiagnostics origStore (J.Uri "uri") (Just 2) (partitionBySource diags2)) `shouldBe`
        Map.fromList
          [ ((J.Uri "uri"),StoreItem (Just 2) $ Map.fromList [(Just "hlint", SL.toSortedList diags2) ] )
          ]

    -- ---------------------------------

    it "updates a store for a new doc version, removing all priot sources" $ do
      let
        diags1 =
          [ SD NoCodeAction (mkDiagnostic (Just "hlint") "a1")
          , SD NoCodeAction (mkDiagnostic (Just "ghcmod") "b1")
          ]
        diags2 =
          [ SD NoCodeAction (mkDiagnostic (Just "hlint") "a2")
          ]
      let origStore = updateDiagnostics Map.empty (J.Uri "uri") (Just 1) (partitionBySource diags1)
      (updateDiagnostics origStore (J.Uri "uri") (Just 2) (partitionBySource diags2)) `shouldBe`
        Map.fromList
          [ ((J.Uri "uri"),StoreItem (Just 2) $ Map.fromList
                [(Just "hlint", SL.singleton (SD NoCodeAction (mkDiagnostic (Just "hlint")  "a2")))
              ] )
          ]

    -- ---------------------------------

  describe "retrieves all the diagnostics for a given uri" $ do

    it "gets diagnostics for multiple sources" $ do
      let
        diags =
          [ SD NoCodeAction (mkDiagnostic (Just "hlint") "a")
          , SD NoCodeAction (mkDiagnostic (Just "ghcmod") "b")
          ]
      let ds = updateDiagnostics Map.empty (J.Uri "uri") (Just 1) (partitionBySource diags)
      (getDiagnosticParamsFor 10 ds (J.Uri "uri")) `shouldBe`
        Just (J.PublishDiagnosticsParams (J.Uri "uri") (J.List $ map sdDiagnostic $ reverse diags))

    -- ---------------------------------

  describe "limits the number of diagnostics retrieved, in order" $ do

    it "gets diagnostics for multiple sources" $ do
      let
        diags =
          [ SD NoCodeAction (mkDiagnostic2 (Just "hlint") "a")
          , SD NoCodeAction (mkDiagnostic2 (Just "ghcmod") "b")
          , SD NoCodeAction (mkDiagnostic  (Just "hlint") "c")
          , SD NoCodeAction (mkDiagnostic  (Just "ghcmod") "d")
          ]
      let ds = updateDiagnostics Map.empty (J.Uri "uri") (Just 1) (partitionBySource diags)
      (getDiagnosticParamsFor 2 ds (J.Uri "uri")) `shouldBe`
        Just (J.PublishDiagnosticsParams (J.Uri "uri")
              (J.List [
                    mkDiagnostic  (Just "ghcmod") "d"
                  , mkDiagnostic  (Just "hlint") "c"
                  ]))

      (getDiagnosticParamsFor 1 ds (J.Uri "uri")) `shouldBe`
        Just (J.PublishDiagnosticsParams (J.Uri "uri")
              (J.List [
                    mkDiagnostic  (Just "ghcmod") "d"
                  ]))

    -- ---------------------------------

  describe "flushes the diagnostics for a given source" $ do

    it "gets diagnostics for multiple sources" $ do
      let
        diags =
          [ SD NoCodeAction (mkDiagnostic2 (Just "hlint") "a")
          , SD NoCodeAction (mkDiagnostic2 (Just "ghcmod") "b")
          , SD NoCodeAction (mkDiagnostic  (Just "hlint") "c")
          , SD NoCodeAction (mkDiagnostic  (Just "ghcmod") "d")
          ]
      let ds = updateDiagnostics Map.empty (J.Uri "uri") (Just 1) (partitionBySource diags)
      (getDiagnosticParamsFor 100 ds (J.Uri "uri")) `shouldBe`
        Just (J.PublishDiagnosticsParams (J.Uri "uri")
              (J.List [
                    mkDiagnostic  (Just "ghcmod") "d"
                  , mkDiagnostic  (Just "hlint") "c"
                  , mkDiagnostic2 (Just "ghcmod") "b"
                  , mkDiagnostic2 (Just "hlint") "a"
                  ]))

      let ds' = flushBySource ds (Just "hlint")
      (getDiagnosticParamsFor 100 ds' (J.Uri "uri")) `shouldBe`
        Just (J.PublishDiagnosticsParams (J.Uri "uri")
              (J.List [
                     mkDiagnostic  (Just "ghcmod") "d"
                  ,  mkDiagnostic2 (Just "ghcmod") "b"
                  ]))

    -- ---------------------------------
