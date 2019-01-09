{-# LANGUAGE TemplateHaskell #-}

module Kore.Domain
  ( External (..)
  , CommonExternalPattern
  , Builtin (..)
  ) where

import           Control.DeepSeq
                 ( NFData (..) )
import           Data.Deriving
                 ( deriveEq1, deriveOrd1, deriveShow1 )
import qualified Data.Foldable as Foldable
import           Data.Functor.Const
                 ( Const )
import           Data.Hashable
                 ( Hashable, hashWithSalt )
import           Data.Map
                 ( Map )
import qualified Data.Map as Map
import           Data.Sequence
                 ( Seq )
import           Data.Set
                 ( Set )
import           Data.Void
                 ( Void )
import           GHC.Generics
                 ( Generic )

import Kore.AST.Pure
import Kore.Annotation.Valid

-------------------------------
-- Un-Verified Domain Values --
{-| 'External' denotes a domain value not yet verified. Represented in Kore
abstract syntax rather than a built-in datatype.
-}
newtype External child =
    External { getExternal :: CommonPurePattern Meta (Const Void) }
    deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)

deriveEq1 ''External
deriveOrd1 ''External
deriveShow1 ''External

instance NFData (External child)

instance Hashable (External child)

type CommonExternalPattern level = CommonPurePattern level External

----------------------------
-- Verified Domain Values --
type Key = PurePattern Object Builtin Concrete (Valid Object)

{- | 'Builtin' denotes a domain value after it has been decoded from Kore
abstract syntax to the built-in datatype
TODO(Nick): remove BuiltinPattern constructor
-}
data Builtin child
    = BuiltinPattern !(ParsedPurePattern Meta (Const Void))  -- ^ deprecated, see 'External'
    | BuiltinMap !(Map Key child)
    | BuiltinList !(Seq child)
    | BuiltinSet !(Set Key)
    | BuiltinInteger Integer
    | BuiltinBool Bool
    deriving (Foldable, Functor, Generic, Traversable)

deriving instance Eq child => Eq (Builtin child)

deriving instance Ord child => Ord (Builtin child)

deriving instance Show child => Show (Builtin child)

deriveEq1 ''Builtin
deriveOrd1 ''Builtin
deriveShow1 ''Builtin

instance Hashable child => Hashable (Builtin child) where
    hashWithSalt salt =
        \case
            BuiltinPattern pat ->
                salt `hashWithSalt` (0::Int) `hashWithSalt` pat
            BuiltinMap (Map.toAscList -> map') ->
                salt `hashWithSalt` (1::Int) `hashWithSalt` map'
            BuiltinList (Foldable.toList -> list) ->
                salt `hashWithSalt` (2::Int) `hashWithSalt` list
            BuiltinSet (Foldable.toList -> set) ->
                salt `hashWithSalt` (3::Int) `hashWithSalt` set
            BuiltinInteger int ->
                salt `hashWithSalt` (4::Int) `hashWithSalt` int
            BuiltinBool bool ->
                salt `hashWithSalt` (5::Int) `hashWithSalt` bool

instance NFData child => NFData (Builtin child)
