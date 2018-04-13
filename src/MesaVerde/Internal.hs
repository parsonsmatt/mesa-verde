{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}

module MesaVerde.Internal where

import           Data.String            (IsString (..))
import           Database.Persist.Types (EntityDef (..))
import           GHC.Exts               (Constraint)
import           GHC.TypeLits
import           Language.Haskell.TH    (Name)

-- the desired syntax, blessed may it be
example :: Schema
example = do
    userRef <- table "User" $ do
        "name"   ''String
        "age"    ''Int

        derive @[Eq, Ord, Show]

    let theUsual = derive @[Eq, Ord, Show]

    table_ "Dog" $ do
        "owner"  userRef
        "name"   ''String                 sql { sqlType = "lmao" }

        theUsual

    table_ "Friend" $ do
        field           "foobar" ''Int    sql { sqlType = "why" }
        name <- fieldRef "name"   ''String sql { colName = "friend_name" }
        ugh  <- fieldRef "ugh"    ''Int    sql { colName = "x", sqlType = "y" }

        unique "uniq_by_name" [name]
        primary ugh

-- the end result of running the schema definitions will be a @['EntityDef']@.
newtype SchemaDefine a = SchemaDefine (IO a)
    deriving (Functor, Applicative, Monad)

type Schema = SchemaDefine ()

-- introduce a table
table :: String -> EntityDefine a -> SchemaDefine EntityRef
table = undefined

-- silence the @-funused-do-bind@ or whatever
table_ :: String -> EntityDefine a -> SchemaDefine ()
table_ = undefined

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

data FieldRef

-- god, this is really awful, isn't it?
derive :: forall (xs :: [* -> Constraint]) a. EntityDefine a
derive = undefined

unique :: String -> [FieldRef] -> EntityDefine x
unique = undefined

primary :: FieldRef -> EntityDefine x
primary = compositePrimary . pure

compositePrimary :: [FieldRef] -> EntityDefine x
compositePrimary = undefined

-- digusting classes

class WithFieldRef ret where
    fieldRef :: NameOrEntityRef x => String -> x -> ret

instance WithFieldRef (EntityDefine FieldRef) where
    fieldRef :: NameOrEntityRef x => String -> x -> EntityDefine FieldRef
    fieldRef = undefined

instance WithFieldRef (FieldSpec -> EntityDefine FieldRef) where
    fieldRef :: NameOrEntityRef x => String -> x -> FieldSpec -> EntityDefine FieldRef
    fieldRef = undefined

instance {-# OVERLAPPABLE #-} (TypeError (Text "you did it wrong")) => WithFieldRef a where
    fieldRef = undefined


class Field ret where
    field :: NameOrEntityRef x => String -> x -> ret

instance (() ~ a) => Field (EntityDefine a) where
    field = undefined

instance (() ~ a) => Field (FieldSpec -> EntityDefine a) where
    field = undefined

instance {-# OVERLAPPABLE #-} (TypeError (Text "you did it wrong, in a field")) => Field a where
    field = undefined

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

-- do i even have shame anymore
instance (() ~ a) => IsString (EntityRef -> FieldSpec -> EntityDefine a) where
    fromString = undefined

instance (() ~ a) => IsString (Name -> FieldSpec -> EntityDefine a) where
    fromString = undefined

-- gross hack so i can lighten the syntax load
class NameOrEntityRef x where
    disambiguate :: x -> Either Name EntityRef

instance NameOrEntityRef Name where
    disambiguate = Left

instance NameOrEntityRef EntityRef where
    disambiguate = Right

instance {-# OVERLAPPABLE #-} (TypeError (Text "No.")) => NameOrEntityRef a where
    disambiguate = undefined
