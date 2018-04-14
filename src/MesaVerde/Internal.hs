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
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module MesaVerde.Internal where

import           Control.Monad.Fix
import           Data.String            (IsString (..))
import           Data.Typeable
import           Database.Persist.Types (EntityDef (..))
import           GHC.Exts               (Constraint)
import           GHC.TypeLits
import           Language.Haskell.TH    (Name)

-- the desired syntax, blessed may it be
example :: Schema
example = mdo
    table_ "Dog" $ do
        owner <- fieldRef "owner"  userRef
        field            "name"   ''String      sql { sqlType = "lmao" }

        foreignKey userRef "fk_dog_user" [owner]

        derive @[Eq, Ord, Show]

    let theUsual = derive @[Eq, Ord, Show]

    userRef <- table "User" $ do
        field "name"   @String
        field "age"    @Int      ! "do people use attribute?" ! "i hope so"
        theUsual

    table_ "Friend" $ do
        field           "foobar" @Int    sql { sqlType = "why" }
        name <- fieldRef "name"  @String sql { colName = "friend_name" }
        ugh  <- fieldRef "ugh"   @Int    sql { colName = "x", sqlType = "y" }

        wtf "lmao" @Int

        unique "FriendName" name
        primary ugh

    -- copped from persistent-test/src/Recursive.hs:
    subtype <- table "SubType" $ do
        "object" object
        derive @[Show, Eq]

    object <- table "Object" $ do
        "sub" subtype
        derive @[Show, Eq]

    table_ "MaybeField" $ do
        field "some_maybe" @(Maybe Int)

    done

done :: Applicative f => f ()
done = pure ()

-- the end result of running the schema definitions will be a @['EntityDef']@.
newtype SchemaDefine a = SchemaDefine (IO a)
    deriving (Functor, Applicative, Monad, MonadFix)

type Schema = SchemaDefine ()

-- introduce a table
table :: String -> (forall s. EntityDefine s a) -> SchemaDefine EntityRef
table str _ = undefined

-- silence the @-funused-do-bind@ or whatever
table_ :: String -> (forall s. EntityDefine s a) -> SchemaDefine ()
table_ str _ = undefined

--------------------------------------------------------------------------------
-- entity definition
--------------------------------------------------------------------------------

-- the end result of running an 'EntityDefine' will be a single 'EntityDef'.
newtype EntityDefine s a = EntityDefine (IO a)
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

data FieldRef s

--------------------------------------------------------------------------------
-- deriving
--------------------------------------------------------------------------------

derive :: forall (xs :: [* -> Constraint]) s. DeriveImpl xs => EntityDefine s ()
derive = undefined
  where
    strs = deriveImpl (Proxy @xs)

class DeriveImpl (xs :: [* -> Constraint]) where
    deriveImpl :: proxy xs -> [String]

instance DeriveImpl '[] where
    deriveImpl _ = []

instance (DeriveImpl xs, Typeable x) => DeriveImpl (x ': xs) where
    deriveImpl _ = show (typeRep (Proxy @x)) : deriveImpl (Proxy @xs)

--------------------------------------------------------------------------------
-- keys and references
--------------------------------------------------------------------------------

primary :: FieldRef s -> EntityDefine s x
primary = compositePrimary . pure

compositePrimary :: [FieldRef s] -> EntityDefine s x
compositePrimary = undefined

foreignKey :: EntityRef -> String -> [FieldRef s] -> EntityDefine s ()
foreignKey = undefined

attribute :: EntityDefine s a -> String -> EntityDefine s a
attribute = undefined

(!) :: EntityDefine s a -> String -> EntityDefine s a
(!) = attribute

infixl 5 !

--------------------------------------------------------------------------------
-- uniquenes
--------------------------------------------------------------------------------

uniqueImpl
    :: String
    -> [FieldRef s]
    -> Maybe FieldSpec
    -> EntityDefine s ()
uniqueImpl = undefined

class Unique ret where
    unique :: String -> ret

instance (a ~ ()) => Unique (FieldRef s -> EntityDefine s a) where
    unique str ref = uniqueImpl str [ref] Nothing

instance (a ~ ()) => Unique ([FieldRef s] -> EntityDefine s a) where
    unique str refs = uniqueImpl str refs Nothing

instance (a ~ ()) => Unique (FieldRef s -> FieldSpec -> EntityDefine s a) where
    unique str ref spec = uniqueImpl str [ref] (Just spec)

instance (a ~ ()) => Unique ([FieldRef s] -> FieldSpec -> EntityDefine s a) where
    unique str refs spec = uniqueImpl str refs (Just spec)

instance {-# OVERLAPPABLE #-} (TypeError (Text "you did it wrong, in a Unique")) => Unique a where
    unique = undefined

--------------------------------------------------------------------------------
-- fields
--------------------------------------------------------------------------------

fieldImpl
    :: forall fieldType x s
    . (TypeOrEntityRef x)
    => String
    -> x
    -> Maybe FieldSpec
    -> EntityDefine s ()
fieldImpl name x mfield = undefined

class Field ret where
    field :: String -> ret

instance (() ~ a) => Field (EntityDefine s a) where
    field :: String -> forall m. EntityDefine s a
    field s = fieldImpl s (typ @m) Nothing

instance (() ~ a, TypeOrEntityRef s) => Field (s -> FieldSpec -> EntityDefine s a) where
    field s x r = fieldImpl s x (Just r)

instance {-# OVERLAPPABLE #-} (TypeError (Text "you did it wrong, in a field")) => Field a where
    field = undefined


class WithFieldRef ret where
    fieldRef :: TypeOrEntityRef x => String -> x -> ret

instance WithFieldRef (EntityDefine s (FieldRef s)) where
    fieldRef :: TypeOrEntityRef x => String -> x -> EntityDefine s (FieldRef s)
    fieldRef = undefined

instance (r ~ FieldRef s) => WithFieldRef (FieldSpec -> EntityDefine s r) where
    fieldRef :: TypeOrEntityRef x => String -> x -> FieldSpec -> EntityDefine s (FieldRef s)
    fieldRef = undefined

instance {-# OVERLAPPABLE #-} (TypeError (Text "you did it wrong")) => WithFieldRef a where
    fieldRef = undefined

--------------------------------------------------------------------------------
-- nasty syntax hacks
--------------------------------------------------------------------------------

fieldWithRef :: String -> EntityRef -> EntityDefine s ()
fieldWithRef = undefined

-- do i even have shame anymore
instance (() ~ a) => IsString (EntityRef -> FieldSpec -> EntityDefine s a) where
    fromString = undefined

instance (() ~ a) => IsString (Name -> FieldSpec -> EntityDefine s a) where
    fromString = undefined

-- gross hack so i can lighten the syntax load
class TypeOrEntityRef x where
    disambiguate :: x -> Either Typ EntityRef

instance TypeOrEntityRef Typ where
    disambiguate = Left

instance TypeOrEntityRef EntityRef where
    disambiguate = Right

instance {-# OVERLAPPABLE #-} (TypeError (Text "No.")) => TypeOrEntityRef a where
    disambiguate = undefined

data Typ where
    Typ :: Typeable t => Proxy t -> Typ

typ :: forall t. Typeable t => Typ
typ = Typ (Proxy @t)
