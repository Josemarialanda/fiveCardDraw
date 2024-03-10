{-# LANGUAGE TemplateHaskell #-}

module FiveCardDraw.Utils.OneOfN where

import Data.Data           (Data, Typeable)
import Language.Haskell.TH (mkName, varP, match,
                            clause, varE, appE,
                            caseE, normalB, funD,
                            Q, Con(..), Type(..),
                            Dec(..), DerivClause(..),
                            conP, Bang(..),
                            SourceStrictness(..),
                            SourceUnpackedness(..),
                            TyVarBndr(..) )

$( do
     let ofPrefix :: Int -> String
         ofPrefix 1 = "One"
         ofPrefix 2 = "Two"
         ofPrefix 3 = "Three"
         ofPrefix 4 = "Four"
         ofPrefix 5 = "Five"
         ofPrefix 6 = "Six"
         ofPrefix 7 = "Seven"
         ofPrefix 8 = "Eight"
         ofPrefix 9 = "Nine"
         ofPrefix 10 = "Ten"
         ofPrefix 11 = "Eleven"
         ofPrefix 12 = "Twelve"
         ofPrefix 13 = "Thirteen"
         ofPrefix 14 = "Fourteen"
         ofPrefix 15 = "Fifteen"
         ofPrefix 16 = "Sixteen"
         ofPrefix 17 = "Seventeen"
         ofPrefix 18 = "Eighteen"
         ofPrefix 19 = "Nineteen"
         ofPrefix 20 = "Twenty"
        
         oneOfN :: Int -> Q [Dec]
         oneOfN n 
           | n > 20    = error "OneOf20 is the largest supported type."
           | otherwise = do
             let names = (\i -> mkName $ "a" <> show i) <$> [1..n]
                 vars  = ($ ()) . PlainTV <$> names
                 deriv = DerivClause Nothing $ ConT <$> [''Eq, ''Read, ''Show, ''Data, ''Typeable]
                 cons = (\(PlainTV varName (),i) -> NormalC (mkName $ ofPrefix i <> "Of" <> show n) [(Bang SourceNoUnpack SourceStrict, VarT varName)]) <$> zip vars [1..n]
             return [ DataD [] (mkName $ "OneOf" <> show n) vars Nothing cons [deriv] ]
     
         foldOneOfN :: Int -> Q [Dec]
         foldOneOfN n = do
           let fFnArgs m = mkName ("f" <> show m)
               fItArg = mkName "it"
               x   = mkName "x"
               dtName i j = mkName $ ofPrefix i <> "Of" <> show j
           fun <- funD (mkName $ "foldOneOf" <> show n)
                    [ clause
                        ((varP . fFnArgs <$> [1..n]) <> [varP fItArg])
                        (normalB (caseE (varE fItArg)
                            [ match (conP (dtName k n) [varP x])
                                (normalB (appE (varE (fFnArgs k)) (varE x))) []
                            | k <- [1..n]
                            ])) []
                    ]
           return [fun]
     
     oneOfNDecs <- concat <$> traverse oneOfN [1..20]
     foldOneOfNDecs <- concat <$> traverse foldOneOfN [1..20]
     return $ oneOfNDecs <> foldOneOfNDecs
 )