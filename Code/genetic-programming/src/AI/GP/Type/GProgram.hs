{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module AI.GP.Type.GProgram where

import Data.Eq (Eq)
import Data.Int (Int)
import Text.Show (Show)

import Control.Lens (makeLenses)
import Data.Default (Default(def))


data GProgram op t
    = Node
        { _height :: Int
        , _nodeData :: op
        , _lSubTree :: GProgram op t
        , _rSubTree :: GProgram op t
        }
    | Leaf
        { _height :: Int
        , _val :: t
        }
    deriving (Eq, Show)
makeLenses ''GProgram

instance Default (GProgram op t) where
    def = Leaf 0 undefined
      where
        undefined = undefined

type Individual op t = GProgram op t
type IndividualPair op t = (GProgram op t, GProgram op t)

withOperation :: (op -> op) -> GProgram op t -> GProgram op t
withOperation f (Node h o l r) = Node h (f o) l r
withOperation _ leaf = leaf
