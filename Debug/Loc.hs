{-# LANGUAGE StandaloneDeriving, TemplateHaskell #-}
module Debug.Loc where

import Control.Applicative
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

instance Lift Loc where
    lift x = recConE 'Loc [ (,) <$> (pure 'loc_filename) <*> litE (stringL (loc_filename x))
                          , (,) <$> (pure 'loc_package) <*> litE (stringL (loc_package x))
                          , (,) <$> (pure 'loc_module) <*> litE (stringL (loc_module x))
                          , (,) <$> (pure 'loc_start) <*> [|($(litE (integerL (fromIntegral (fst (loc_start x))))),
                                                             $(litE (integerL (fromIntegral (snd (loc_start x)))))) :: (Int, Int)|]
                          , (,) <$> (pure 'loc_end) <*> [|($(litE (integerL (fromIntegral (fst (loc_end x))))),
                                                           $(litE (integerL (fromIntegral (snd (loc_end x)))))) :: (Int, Int)|] ]

__LOC__ :: Q Exp
__LOC__ = lift =<< location
