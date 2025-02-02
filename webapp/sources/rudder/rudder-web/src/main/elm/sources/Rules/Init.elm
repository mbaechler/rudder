module Rules.Init exposing (..)

import Dict

import Rules.ApiCalls exposing (..)
import Rules.DataTypes exposing (..)
import Compliance.DataTypes exposing (..)

init : { contextPath : String, hasWriteRights : Bool } -> ( Model, Cmd Msg )
init flags =
  let
    initCategory = Category "" "" "" (SubCategories []) []
    initFilters  = Filters (TableFilters Name Asc "" []) (TreeFilters "" [] (Tag "" "") [])
    initUI       = UI initFilters initFilters initFilters (ComplianceFilters False False []) NoModal flags.hasWriteRights True False False Nothing
    initModel    = Model flags.contextPath Loading "" initCategory initCategory initCategory Dict.empty Dict.empty Dict.empty Dict.empty initUI

    listInitActions =
      [ getPolicyMode      initModel
      , getNodesList       initModel
      , getRulesCompliance initModel
      , getGroupsTree      initModel
      , getTechniquesTree  initModel
      , getRulesTree       initModel
      , getRuleChanges     initModel
      , getCrSettings      initModel
      ]

  in

    ( initModel
    , Cmd.batch listInitActions
    )