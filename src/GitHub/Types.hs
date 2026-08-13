module GitHub.Types where

data User = User
  { username :: String
  , followers :: Int
  , following :: Int
  , stars :: Int
  }
  deriving (Show)
