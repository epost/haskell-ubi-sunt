import Control.Applicative
import Types
import Common
import RelationalAlgebra


main = do
  let
    relExp1 :: RelExp
    relExp1 =
      Project ["fname", "mname", "type"] $
      Select  (AttrEq "type" (AStr "function")) $
      NatJoin (Rel ["f", "type" ] typeAttr) $
      NatJoin (Rel ["m", "mname"] nameAttr) $
      NatJoin (Rel ["f", "m"    ] definedInAttr) $
      RelExp  (Rel ["f", "fname"] nameAttr)

  putStrLn . dumpRel . eval $ relExp1


dumpRel :: Rel -> String
dumpRel rs =
  "attrs = " ++ (show . relAttrNames $ rs) ++ "\n" ++
  "rows =\n" ++ (concat $  (\r -> "  " ++ show r ++ "\n") <$> relRows rs)


-- example data ----------------------------------------------------------------

nameAttr =
  [ [AAtom  1, AStr "map"                    ]
  , [AAtom  2, AStr "ap"                     ]
  , [AAtom  3, AStr "lift2"                  ]
  , [AAtom  4, AStr "lift3"                  ]
  , [AAtom 10, AStr "Control.Functor"        ]
  , [AAtom 11, AStr "Control.Applicative"    ]
  , [AAtom 20, AStr "purescript-functors"    ]
  , [AAtom 21, AStr "purescript-applicative" ]
  ]

definedInAttr =
  [ [AAtom  1, AAtom 10]
  , [AAtom  2, AAtom 11]
  , [AAtom  3, AAtom 11]
  , [AAtom  4, AAtom 11]
  , [AAtom 10, AAtom 20]
  , [AAtom 11, AAtom 21]
  ]

typeAttr =
  [ [AAtom  1, AStr "function" ]
  , [AAtom  2, AStr "function" ]
  , [AAtom  3, AStr "function" ]
  , [AAtom  4, AStr "function" ]
  , [AAtom 10, AStr "module"   ]
  , [AAtom 11, AStr "module"   ]
  , [AAtom 20, AStr "library"  ]
  , [AAtom 21, AStr "library"  ]
  ]
