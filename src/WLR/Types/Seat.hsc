module WLR.Types.Seat where

#include <time.h>

import Foreign.C.String (CString)
import Foreign.C.Types (CUInt)

{{ struct
    wlr/types/wlr_seat.h,
    WLR_seat,
    global, Ptr WL_global,
    display, Ptr WL_display,
    clients, Ptr WL_list,
    name, CString,
    capabilities, CUInt,
    accumulated_capabilities, CUInt,
    last_event, timespec,
    selection_source, Ptr WLR_data_source,
    selection_serial, CUInt,
    selection_offers, WL_list,
    primary_selection_source, WLR_primary_selection_source
    primary_selection_serial, CUInt,
}}
--	// `drag` goes away before `drag_source`, when the implicit grab ends
--	struct wlr_drag *drag;
--	struct wlr_data_source *drag_source;
--	uint32_t drag_serial;
--	struct wl_list drag_offers; // wlr_data_offer.link
--
--	struct wlr_seat_pointer_state pointer_state;
--	struct wlr_seat_keyboard_state keyboard_state;
--	struct wlr_seat_touch_state touch_state;
--
--	struct wl_listener display_destroy;
--	struct wl_listener selection_source_destroy;
--	struct wl_listener primary_selection_source_destroy;
--	struct wl_listener drag_source_destroy;
--
--	struct {
--		struct wl_signal pointer_grab_begin;
--		struct wl_signal pointer_grab_end;
--
--		struct wl_signal keyboard_grab_begin;
--		struct wl_signal keyboard_grab_end;
--
--		struct wl_signal touch_grab_begin;
--		struct wl_signal touch_grab_end;
--
--		// struct wlr_seat_pointer_request_set_cursor_event
--		struct wl_signal request_set_cursor;
--
--		// Called when an application _wants_ to set the selection (user copies some data).
--		// Compositors should listen to this event and call wlr_seat_set_selection()
--		// if they want to accept the client's request.
--		struct wl_signal request_set_selection; // struct wlr_seat_request_set_selection_event
--		// Called after the data source is set for the selection.
--		struct wl_signal set_selection;
--
--		// Called when an application _wants_ to set the primary selection (user selects some data).
--		// Compositors should listen to this event and call wlr_seat_set_primary_selection()
--		// if they want to accept the client's request.
--		struct wl_signal request_set_primary_selection; // struct wlr_seat_request_set_primary_selection_event
--		// Called after the primary selection source object is set.
--		struct wl_signal set_primary_selection;
--
--		// struct wlr_seat_request_start_drag_event
--		struct wl_signal request_start_drag;
--		struct wl_signal start_drag; // struct wlr_drag
--
--		struct wl_signal destroy;
--	} events;
--
--	void *data;
--};
