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
    drag, Ptr WLR_drag,
    drag_source, Ptr WLR_data_source,
    drag_serial, CUInt,
    drag_offers, WL_list,
    pointer_state, WLR_seat_pointer_state,
    keyboard_state, WLR_seat_keyboard_state,
    touch_state, WLR_seat_touch_state,
    display_destroy, WL_listener,
    selection_source_destroy, WL_listener,
    primary_selection_source_destroy, WL_listener,
    drag_source_destroy, WL_listener,
    events pointer_grab_begin, WL_signal,
    events pointer_grab_end, WL_signal,
    events keyboard_grab_begin, WL_signal,
    events keyboard_grab_end, WL_signal,
    events touch_grab_begin, WL_signal,
    events touch_grab_end, WL_signal,
    events request_set_cursor, WL_signal,
    events request_set_selection, WL_signal,
    events set_selection, WL_signal,
    events request_set_primary_selection, WL_signal,
    events set_primary_selection, WL_signal,
    events request_start_drag, WL_signal,
    events start_drag, WL_signal,
    events destroy, WL_signal,
    data, Ptr ()
}}
