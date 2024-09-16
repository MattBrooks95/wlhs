{-# LANGUAGE EmptyDataDecls, PatternSynonyms #-}
module WLR.Backend.Session (
    wlr_session_create
    , wlr_session_destroy
    , WLR_session
    ) where

import WL.ServerProtocol (WL_display)

import Foreign.C.Types (CBool(..))
import Foreign.Ptr (Ptr)
import Foreign (Word32)

data WLR_session

foreign import capi "wlr/backend/session.h wlr_session_create"
    wlr_session_create :: Ptr WL_display -> IO (Ptr WLR_session)

foreign import capi "wlr/backend/session.h wlr_session_destroy"
    wlr_session_destroy :: Ptr WLR_session -> IO ()
