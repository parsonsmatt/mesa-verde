{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}

module MesaVerde.Internal where

import           Data.String            (IsString (..))
import           Database.Persist.Types (EntityDef (..))
import           GHC.Exts               (Constraint)
import           Language.Haskell.TH    (Name)

newtype EntityDefine a = EntityDefine (IO a)
    deriving (Functor, Applicative, Monad)

newtype SchemaDefiner a = SchemaDefiner (IO a)
    deriving (Functor, Applicative, Monad)

type SchemaDefine = SchemaDefiner ()

newtype FieldLabel i = FieldLabel String

instance IsString (FieldLabel i) where
    fromString = FieldLabel

example :: SchemaDefine
example = do
    userRef <- model "User" $ do
        "name"   ''String
        "age"    ''Int

        derive @[Eq, Ord, Show]

    let theUsual = derive @[Eq, Ord, Show]

    model_ "Dog" $ do
        "owner"  userRef
        "name"   ''String

        theUsual


data EntityRef

model :: String -> EntityDefine a -> SchemaDefiner EntityRef
model = undefined

model_ :: String -> EntityDefine a -> SchemaDefiner ()
model_ = undefined

field :: forall fieldType a. FieldLabel fieldType -> EntityDefine a
field = undefined

fldname :: String -> Name -> EntityDefine ()
fldname = undefined

fldref :: String -> EntityRef -> EntityDefine ()
fldref = undefined

derive :: forall (xs :: [* -> Constraint]) a. EntityDefine a
derive = undefined

fieldRef :: EntityRef -> String -> EntityDefine a
fieldRef = undefined

instance (() ~ a) => IsString (Name -> EntityDefine a) where
    fromString = fldname

instance (() ~ a) => IsString (EntityRef -> EntityDefine a) where
    fromString = fldref
