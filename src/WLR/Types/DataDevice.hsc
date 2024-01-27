module WLR.Types.DataDevice where

import Foreign.Ptr (Ptr)
import Foreign.C.Types (CUInt, CBool)

import WL.Utils (WL_array)

{{ struct wlr/types/wlr_data_device.h, wlr_data_source_impl }}

{{ struct
    wlr/types/wlr_data_device.h,
    wlr_data_source,
    impl, Ptr WLR_data_source_impl,
    mime_types, WL_array,
    actions, CUInt,
    accepted, Bool,
    current_dnd_action, WL_data_device_manager_dnd_action,
    compositor_action, CUInt,
    events wl_signal_destroy, WL_signal
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
