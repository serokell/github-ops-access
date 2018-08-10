module Postlude
    ( module M
    ) where

import Prelude as M

import Control.Monad as M
import Control.Monad.IO.Class as M (liftIO)
import Control.Monad.Reader as M
import Control.Monad.State as M
import Control.Monad.Trans as M (lift)
import Data.Semigroup as M ((<>))
import Data.Proxy as M (Proxy (Proxy))
import GHC.Generics as M
