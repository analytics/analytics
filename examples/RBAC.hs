{-# LANGUAGE TypeFamilies, TemplateHaskell, DeriveDataTypeable, FlexibleContexts, GADTs, Rank2Types, DeriveFunctor, DeriveFoldable, DeriveTraversable, OverloadedStrings #-}
module Examples.RBAC where

import Control.Applicative
import Data.Analytics.Datalog
import Data.Text
import Data.Typeable

data User = User Text deriving (Eq,Ord,Show,Typeable)
instance Term User

data UserVar = U | V deriving (Eq,Ord,Show,Typeable)
instance Term UserVar where
  type Entity UserVar = User
  term = var

data Role = Role Text deriving (Eq,Ord,Show,Typeable)
instance Term Role

data RoleVar = R | S | T deriving (Eq,Ord,Show,Typeable)
instance Term RoleVar where
  type Entity RoleVar = Role
  term = var

data Permission = Permission Text deriving (Eq,Ord,Show,Typeable)
instance Term Permission

data PermissionVar = P | Q deriving (Eq,Ord,Show,Typeable)
instance Term PermissionVar where
  type Entity PermissionVar = Permission
  term = var

userRole :: T2 (User, Role) User Role ()
userRole = t2 (Table 0 const) (\x y () -> (x,y))

rolePermissions :: T2 (Role, Permission) Role Permission ()
rolePermissions = t2 (Table 1 const) (\x y () -> (x,y))

userPermissions :: T2 (User, Permission) User Permission ()
userPermissions = t2 (Table 2 const) (\x y () -> (x,y))

roleRole :: T2 (Role, Role) Role Role ()
roleRole = t2 (Table 3 const) (\x y () -> (x,y))

test :: Monad m => DatalogT m [(User,Permission)]
test = do

  userRole (User "Oz") (Role "Admin")
  userRole (User "Doug") (Role "Lackey")
  roleRole (Role "Admin") (Role "Lackey")

  rolePermissions (Role "Lackey") (Permission "ScrubToilets")
  rolePermissions (Role "Admin") (Permission "MakeMoney")

  userPermissions U P :- userRole U R <* rolePermissions R P
  rolePermissions R P :- roleRole R S <* rolePermissions S P -- <* filtering R (\(Role r) -> length r >= 4)

  -- query $ row (userRole (User "Oz") R)
  query $ row (userPermissions U (Permission "ScrubToilets"))

test' :: Datalog [(User,Permission)]
test' = test
