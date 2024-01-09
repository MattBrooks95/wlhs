{-# LANGUAGE EmptyDataDeriving, PatternSynonyms #-}

module WLR.Types.KeyboardGroup where

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_keyboard_group.h>

import Foreign.Ptr (Ptr)

import WL.Utils (WL_list)
import WL.ServerCore (WL_signal)
import {-# SOURCE #-} WLR.Types.Keyboard (WLR_keyboard)

data {-# CTYPE "wlr/types/wlr_keyboard_group.h" "struct wlr_keyboard_group" #-} WLR_keyboard_group
    = WLR_keyboard_group
    { wlr_keyboard_group_keyboard :: Ptr WLR_keyboard
    -- keyboard_group_device.link
    , wlr_keyboard_group_devices :: WL_list
    -- keyboard_group_key.link
    , wlr_keyboard_group_keys :: WL_list

    {-
     - Sent when a keyboard has entered the group with keys currently
     - pressed that are not pressed by any other keyboard in the group. The
     - data for this signal will be a struct wl_array containing the key
     - codes. This should be used to update the compositor's internal state.
     - Bindings should not be triggered based off of these key codes and
     - they should also not notify any surfaces of the key press.
    -}
    , wlr_keyboard_group_events_enter :: WL_signal
    {-
     - Sent when a keyboard has left the group with keys currently pressed
     - that are not pressed by any other keyboard in the group. The data for
     - this signal will be a struct wl_array containing the key codes. This
     - should be used to update the compositor's internal state. Bindings
     - should not be triggered based off of these key codes. Additionally,
     - surfaces should only be notified if they received a corresponding key
     - press for the key code.
    -}
    , wlr_keyboard_group_events_leave :: WL_signal

    , wlr_keyboard_group_data :: Ptr ()
    }

foreign import capi "wlr/types/wlr_keyboard_group.h wlr_keyboard_group_create"
    wlr_keyboard_group_create :: IO (Ptr WLR_keyboard_group)

foreign import capi "wlr/types/wlr_keyboard_group.h wlr_keyboard_group_from_wlr_keyboard"
    wlr_keyboard_group_from_wlr_keyboard :: Ptr WLR_keyboard -> Ptr WLR_keyboard_group

-- TODO I looked at the C code for this function and it looks pure, but I'm not sure if it actually is
-- put it in IO for now
foreign import capi "wlr/types/wlr_keyboard_group.h wlr_keyboard_group_add_keyboard"
    wlr_keyboard_group_add_keyboard :: Ptr WLR_keyboard_group -> Ptr WLR_keyboard -> IO (Bool)

foreign import capi "wlr/types/wlr_keyboard_group.h wlr_keyboard_group_remove_keyboard"
    wlr_keyboard_group_remove_keyboard :: Ptr WLR_keyboard_group -> Ptr WLR_keyboard -> IO ()

foreign import capi "wlr/types/wlr_keyboard_group.h wlr_keyboard_group_destroy"
    wlr_keyboard_group_destroy :: Ptr WLR_keyboard_group -> IO ()
