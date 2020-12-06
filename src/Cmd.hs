module Cmd
  ( Cmd(..)
  ) where

import           Data.Default.Class             ( Default(..) )
import qualified Network.AWS.ECS.DescribeServices
                                               as ECS

data Cmd = DescribeServices ECS.DescribeServices
  deriving (Eq, Show)

instance Default Cmd where
  def = DescribeServices ECS.describeServices
