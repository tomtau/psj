-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.ModuleDependencies
-- Copyright   :  (c) 2013-15 Phil Freeman, (c) 2014-15 Gary Burgess
-- License     :  MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- | Provides the ability to sort modules based on module dependencies
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module Language.PureScript.ModuleDependencies (
  sortModules,
  ModuleGraph
) where

import Control.Monad.Error.Class (MonadError(..))

import Data.Graph
import Data.List (nub)
import Data.Maybe (fromMaybe)

import Language.PureScript.Crash
import Language.PureScript.AST
import Language.PureScript.Names
import Language.PureScript.Errors

-- | A list of modules with their transitive dependencies
type ModuleGraph = [(ModuleName, [ModuleName])]

-- | Sort a collection of modules based on module dependencies.
--
-- Reports an error if the module graph contains a cycle.
--
sortModules :: (MonadError MultipleErrors m) => [ModuleHeader] -> m ([ModuleHeader], ModuleGraph)
sortModules ms = do
  let verts = map (\header -> (header, mhModuleName header, nub (usedModules (mhImports header)))) ms
  ms' <- mapM toModule $ stronglyConnComp verts
  let (graph, fromVertex, toVertex) = graphFromEdges verts
      moduleGraph = do (_, mn, _) <- verts
                       let v       = fromMaybe (internalError "sortModules: vertex not found") (toVertex mn)
                           deps    = reachable graph v
                           toKey i = case fromVertex i of (_, key, _) -> key
                       return (mn, filter (/= mn) (map toKey deps))
  return (ms', moduleGraph)
  where
  usedModules :: [ImportDeclaration] -> [ModuleName]
  usedModules = map (\(ImportDeclaration mn _ _) -> mn)

  -- | Convert a strongly connected component of the module graph to a module
  toModule :: (MonadError MultipleErrors m) => SCC ModuleHeader -> m ModuleHeader
  toModule (AcyclicSCC h) = return h
  toModule (CyclicSCC [h]) = return h
  toModule (CyclicSCC hs) = throwError . errorMessage $ CycleInModules (map mhModuleName hs)
