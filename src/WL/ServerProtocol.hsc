{-# LANGUAGE PatternSynonyms #-}
module WL.ServerProtocol where

#include <wayland-server-protocol.h>

import Foreign.C.Types (CInt)

type WL_data_device_manager_dnd_action = CInt

pattern WL_DATA_DEVICE_MANAGER_DND_ACTION_NONE :: (Eq a, Num a) => a
pattern WL_DATA_DEVICE_MANAGER_DND_ACTION_NONE = 0

pattern WL_DATA_DEVICE_MANAGER_DND_ACTION_COPY :: (Eq a, Num a) => a
pattern WL_DATA_DEVICE_MANAGER_DND_ACTION_COPY = 1

pattern WL_DATA_DEVICE_MANAGER_DND_ACTION_MOVE :: (Eq a, Num a) => a
pattern WL_DATA_DEVICE_MANAGER_DND_ACTION_MOVE = 2

pattern WL_DATA_DEVICE_MANAGER_DND_ACTION_ASK :: (Eq a, Num a) => a
pattern WL_DATA_DEVICE_MANAGER_DND_ACTION_ASK = 4
