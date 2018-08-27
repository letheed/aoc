module Error
    ( IsError (..)
    ) where

import Data.Typeable (Typeable, cast)
import TextShow


class (Typeable e, TextShow e) => IsError e where
    toError   :: e -> Error
    fromError :: Error -> Maybe e

    toError = Error
    fromError = cast

data Error = forall e . IsError e => Error e

instance TextShow Error where
    showbPrec p (Error e) = showbPrec p e
