{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module AI.GP.Type.GProgram where

import Prelude

import Data.Word (Word32)
import Data.Proxy (Proxy)
import Data.Dynamic

type Forest a = [a]

data GPData
    = forall a . GPData
        { _operation :: Dynamic
        , _retType :: Proxy a
        , _height :: Word32
        , _depth :: Word32
        }

instance Show GPData where
    show _ = "SomeData"

data GPTree
    = Node
        { _data :: GPData
        , _subForest :: Forest GPTree
        }
    deriving (Show)

tree1 = Node undefined
    [ Node undefined []
    , Node undefined []
    ]

data GPTreeFocus
    = Focus
        { _focus :: GPData
        , _context :: [(Forest GPTree, GPData, Forest GPTree)]
        , _lSiblings :: Forest GPTree
        , _rSiblings :: Forest GPTree
        }
    deriving (Show)

focus Node{..} = Focus _data [] [] _subForest

down (Focus d c ls (r:rs)) = Focus (_data r) ((ls,d,rs):c) [] (_subForest r)

up (Focus d ((pls,pd,prs):ctxs) ls rs) = Focus pd ctxs [] (pls ++ mkNode:prs)
  where
    mkNode = Node d (reverse ls ++ rs)
