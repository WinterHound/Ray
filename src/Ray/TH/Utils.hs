module Ray.TH.Utils where


import           Data.Char           (isUpper)
import           Language.Haskell.TH
import           Lens.Micro.Platform


-- Suffixed Abrev

goA :: Name -> DecsQ
goA nameVar = makeLensesWith (lensRules & lensField .~ (\_ _ n -> [TopName $ mkName (nameBase n <> "_" <> getSuffix (nameBase nameVar))])) nameVar
      where
        getSuffix :: String -> String
        -- getSuffix = filter isUpper
        getSuffix x = filter isUpper x <> [last x]


-- Suffixed Name

goN :: Name -> DecsQ
goN nameVar = makeLensesWith (lensRules & lensField .~ (\_ _ n -> [TopName $ mkName (nameBase n <> "_" <> nameBase nameVar)])) nameVar


-- Suffixed Custom Name

gof :: (String, Name) -> DecsQ
gof (suffix, nameVar) = makeLensesWith (lensRules & lensField .~ (\_ _ n -> [TopName $ mkName (nameBase n <> "_" <> suffix)])) nameVar


-- Suffixed Name

go_ :: Name -> DecsQ
go_ = makeLensesWith (lensRules & lensField .~ (\_ _ n -> [TopName $ mkName ("_" <> nameBase n)]))




