{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MesaVerde.Internal where

import Database.Persist.Types (EntityDef(..))

newtype EntityDefine i a = EntityDefine (IO a)
    deriving (Functor, Applicative, Monad)

newtype SchemaDefine a = SchemaDefine (IO a)
    deriving (Functor, Applicative, Monad)

example :: SchemaDefine a
example = do
    model "User" $ do
        "name" @String
        "age" @Int
        derive [''Eq, ''Ord, ''Show]

    model "Dog" $ do
        "name" @String
        "owner" @UserId
