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

-- the desired syntax, blessed may it be
example :: Schema
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

    model_ "Friend" $ do
        "name"   ''String sql { colName = "friend_name" }
        "foobar" ''Int    sql { sqlType = "why" }
        "ugh"    ''Int    sql { colName = "no_u", sqlType = "why" }

        theUsual

-- the end result of running the schema definitions will be a @['EntityDef']@.
newtype SchemaDefine a = SchemaDefine (IO a)
    deriving (Functor, Applicative, Monad)

type Schema = SchemaDefine ()

-- introduce a model
model :: String -> EntityDefine a -> SchemaDefine EntityRef
model = undefined

-- silence the @-funused-do-bind@ or whatever
model_ :: String -> EntityDefine a -> SchemaDefine ()
model_ = undefined

--------------------------------------------------------------------------------
-- entity definition
--------------------------------------------------------------------------------

-- the end result of running an 'EntityDefine' will be a single 'EntityDef'.
newtype EntityDefine a = EntityDefine (IO a)
    deriving (Functor, Applicative, Monad)

data FieldSpec = FieldSpec
    { sqlType :: OptionalField
    , colName :: OptionalField
    }

newtype OptionalField = OptionalField { unOptionalField :: Maybe String }

instance IsString OptionalField where
    fromString = OptionalField . Just

sql :: FieldSpec
sql = FieldSpec
    { sqlType = OptionalField Nothing
    , colName = OptionalField Nothing
    }

data EntityRef

-- god, this is really awful, isn't it?
derive :: forall (xs :: [* -> Constraint]) a. EntityDefine a
derive = undefined

--------------------------------------------------------------------------------
-- nasty syntax hacks
--------------------------------------------------------------------------------

instance (() ~ a) => IsString (Name -> EntityDefine a) where
    fromString = fieldWithType

fieldWithType :: String -> Name -> EntityDefine ()
fieldWithType = undefined

instance (() ~ a) => IsString (EntityRef -> EntityDefine a) where
    fromString = fieldWithRef

fieldWithRef :: String -> EntityRef -> EntityDefine ()
fieldWithRef = undefined

instance (() ~ a) => IsString (EntityRef -> FieldSpec -> EntityDefine a) where
    fromString = undefined

instance (() ~ a) => IsString (Name -> FieldSpec -> EntityDefine a) where
    fromString = undefined
