{-# LANGUAGE TypeFamilies, TemplateHaskell, DeriveDataTypeable, FlexibleContexts, GADTs, Rank2Types, DeriveFunctor, DeriveFoldable, DeriveTraversable, TypeSynonymInstances, FlexibleInstances #-}
module Examples.RBAC where

import Control.Applicative
import Data.Analytics.Datalog
import Data.Text
import Data.Typeable

instance Term String
data StringVar = P | Q | R | S | T | U | V deriving (Eq,Ord,Show,Typeable)
instance Term StringVar where
  type Entity StringVar = String
  term = var

userRole, roleRole, rolePermissions, userPermissions :: T2 (String, String) String String ()
userRole        = t2 (Table 0 const) (\x y () -> (x :: String,y :: String))
roleRole        = t2 (Table 1 const) (\x y () -> (x :: String,y :: String))
rolePermissions = t2 (Table 2 const) (\x y () -> (x :: String,y :: String))
userPermissions = t2 (Table 3 const) (\x y () -> (x :: String,y :: String))

test :: Monad m => DatalogT m [(String,String)]
test = do
  userRole "Oz"    "Admin"
  userRole "Doug"  "Lackey"

  roleRole "Admin" "Lackey"

  rolePermissions "Lackey" "ScrubToilets"
  rolePermissions "Admin" "MakeMoney"

  userPermissions "Oz" "DoubleSecretAccess"

  userRole U S        :- userRole U R <* roleRole R S
  userPermissions U P :- userRole U R <* rolePermissions R P
  rolePermissions R P :- roleRole R S <* rolePermissions S P

  query $ row (userPermissions U "ScrubToilets")
