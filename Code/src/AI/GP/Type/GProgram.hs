{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module AI.GP.Type.GProgram where

import Data.Eq (Eq)
import Data.Int (Int)
import Text.Show (Show)

import Control.Lens (makeLenses)


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
