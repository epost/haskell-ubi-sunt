module RelationalAlgebra (
    Rel(..)
  , Row(..)
  , AttrVal(..)

  , RelExp(..)
  , RelFilter(..)
  , eval

  , project
  , select
  , natJoin
  ) where

import Control.Applicative
import Control.Monad ((<=<))

import Data.List
import Data.Maybe (fromJust)
import Types
import Common

type AttrName = String

-- | A relational algebra expression.
data RelExp = RelExp  Rel
            | NatJoin Rel        RelExp
            | Select  RelFilter  RelExp
            | Project [AttrName] RelExp
            deriving Show

data RelFilter = AttrEq AttrName AttrVal
               | AtomEq AtomId
               deriving Show

-- | Relation encoded as a list of rows and attribute names. An index into
--   'relAttrNames' should correspond to the index into the 'Row's in 'relRows'.
--
--   Law: length of the var names list should equal that of the length of
--   every 'Row'.
--
data Rel = Rel { relAttrNames :: [AttrName]
               , relRows      :: [Row]
               } deriving Show

type Row = [AttrVal]

data AttrVal = AAtom AtomId
             | AStr  String
             | AInt  Int
             | ABool Bool
             deriving (Eq, Show)

type ColNum = Int
type RowNum = Int

eval :: RelExp -> Rel
eval (RelExp rel)          = rel
eval (NatJoin   rel   exp) = rel `natJoin` (eval exp)
eval (Select    f     exp) = select f (eval exp)
eval (Project   attrs exp) = project attrs (eval exp)

natJoin :: Rel -> Rel -> Rel
natJoin (Rel names_l rows_l) (Rel names_r rows_r)
      | names_unifiable == [] = error "cartesian join not yet supported"
      | otherwise             = Rel (names_l ++ names_r) joinedRows
  where
    joinedRows :: [Row]
    joinedRows = [row_l ++ row_r  | row_l               <- rows_l
                                  , row_r               <- rows_r
                                  , col@(Just (il, ir)) <- overlapPosOfNames names_l names_r <$> names_unifiable
                                  , (row_l!?il) == (row_r!?ir) -- this passes on nonexistent indices on both sides, which i think indicates a logic error; alternative?
                                  ]

    names_unifiable = intersect names_l names_r

select :: RelFilter -> Rel -> Rel
select (AttrEq attrName (AStr strToMatch)) (Rel names rows) =
  Rel names (filter keepRow rows)
    where
      keepRow row = Just (AStr strToMatch) == findValueByAttrName attrName names row
select f _ =
  error $ "filtering not yet supported for " ++ show f

project :: [AttrName] -> Rel -> Rel
project names' (Rel names rows) = (Rel names' rows')
  where
    rows' = keep colsToKeep <$> rows

    keep :: [ColNum] -> Row -> Row
    keep cols row = findValueByColIdxUnsafe row <$> cols

    colsToKeep :: [ColNum]
    colsToKeep = flip findColIdxUnsafe names <$> names'

findColIdx :: AttrName -> [AttrName] -> Maybe ColNum
findColIdx = elemIndex

findColIdxUnsafe :: AttrName -> [AttrName] -> ColNum
findColIdxUnsafe n = fromJust . findColIdx n

findValueByColIdxUnsafe :: Row -> ColNum -> AttrVal
findValueByColIdxUnsafe = (!!)

findValueByAttrName :: AttrName -> [AttrName] -> Row -> Maybe AttrVal
findValueByAttrName attrName names row =
  findElemIndex row <=< findColIdx attrName $ names

overlapPosOfNames :: [AttrName] -> [AttrName] -> AttrName -> Maybe (ColNum, ColNum)
overlapPosOfNames lhs rhs attrName = (,) <$> findColIdx attrName lhs <*> findColIdx attrName rhs

overlapPos :: Rel -> Rel -> AttrName -> Maybe (ColNum, ColNum)
overlapPos lhs rhs = overlapPosOfNames (relAttrNames lhs) (relAttrNames rhs)
