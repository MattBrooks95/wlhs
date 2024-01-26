module WLR.Types.DataDevice where

import Foreign.Ptr (Ptr)

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
    //TODO resume here
}}
--	int32_t grab_touch_id, touch_id; // if WLR_DRAG_GRAB_TOUCH
--
--	struct {
--		struct wl_signal focus;
--		struct wl_signal motion; // struct wlr_drag_motion_event
--		struct wl_signal drop; // struct wlr_drag_drop_event
--		struct wl_signal destroy;
--	} events;
--
--	struct wl_listener source_destroy;
--	struct wl_listener seat_client_destroy;
--	struct wl_listener icon_destroy;
--
--	void *data;
--};
