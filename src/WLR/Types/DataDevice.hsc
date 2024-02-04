{-# LANGUAGE PatternSynonyms #-}
module WLR.Types.DataDevice where

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_data_device.h>

import Foreign.Ptr (Ptr)
import Foreign.C.Types (CUInt, CBool, CInt)
import Foreign.Storable (Storable(..))

import WL.Utils (WL_array)
import WL.ServerProtocol (WL_data_device_manager_dnd_action)
import WL.ServerCore (WL_signal, WL_listener)

import WLR.Types.Compositor (WLR_surface)

-- TODO break up this import cycle with a hs-boot file
import {-# SOURCE #-} WLR.Types.Seat (
    WLR_seat_keyboard_grab
    , WLR_seat_pointer_grab
    , WLR_seat_touch_grab
    , WLR_seat_client
    , WLR_seat
    , WLR_drag_icon
    )

{{ struct wlr/types/wlr_data_device.h, wlr_data_source_impl }}

{{ struct
    wlr/types/wlr_data_device.h,
    wlr_data_source,
    impl, Ptr WLR_data_source_impl,
    mime_types, WL_array,
    actions, CUInt,
    accepted, CBool,
    current_dnd_action, WL_data_device_manager_dnd_action,
    compositor_action, CUInt,
    events destroy, WL_signal
}}

{{ enum
    WLR_drag_grab_type,
    WLR_DRAG_GRAB_KEYBOARD,
    WLR_DRAG_GRAB_KEYBOARD_POINTER,
    WLR_DRAG_GRAB_KEYBOARD_TOUCH
}}

{{ struct
    wlr/types/wlr_data_device.h,
    wlr_drag,
    grab_type, WLR_drag_grab_type,
    keyboard_grab, WLR_seat_keyboard_grab,
    pointer_grab, WLR_seat_pointer_grab,
    touch_grab, WLR_seat_touch_grab,
    seat, Ptr WLR_seat,
    seat_client, Ptr WLR_seat_client,
    focus_client, Ptr WLR_seat_client,
    icon, Maybe (Ptr WLR_drag_icon),
    focus, Maybe (Ptr WLR_surface),
    source, Maybe (Ptr WLR_data_source),
    started, CBool,
    dropped, CBool,
    cancelling, CBool,
    grab_touch_id, CUInt,
    touch_id, CUInt,
    events focus, WL_signal,
    events motion, WL_signal,
    events drop, WL_signal,
    events destroy, WL_signal,
    source_destroy, WL_listener,
    seat_client_destroy, WL_listener,
    icon_destroy, WL_listener,
    data, Ptr ()
}}
