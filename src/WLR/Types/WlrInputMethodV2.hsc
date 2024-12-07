module WLR.Types.WlrInputMethodV2 where

import Foreign.C.String (CString)
import Foreign.C.Types (CInt, CBool)
import Foreign (Word32, Storable(..))
import Foreign.Ptr (Ptr)
import WL.ServerProtocol (WL_display)
import WL.ServerCore (WL_resource, WL_signal, WL_listener, WL_global)
import WL.Utils (WL_list)
import WLR.Types.Keyboard (WLR_keyboard, WLR_keyboard_modifiers)
import WLR.Types.Seat (WLR_seat, WLR_seat_client)
import WLR.Types.Compositor (WLR_surface)
import WLR.Util.Box (WLR_box)

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_input_method_v2.h>


{{ struct wlr/types/wlr_input_method_v2.h,
    wlr_input_method_v2_preedit_string,
    text, CString,
    cursor_begin, CInt,
    cursor_end, CInt
}}

{{ struct wlr/types/wlr_input_method_v2.h,
    wlr_input_method_v2_delete_surrounding_text,
    before_length, Word32,
    after_length, Word32
}}

{{ struct wlr/types/wlr_input_method_v2.h,
    wlr_input_method_v2_state,
    preedit, WLR_input_method_v2_preedit_string,
    commit_text, CString,
    delete, WLR_input_method_v2_delete_surrounding_text
}}

 {- | `active` -pending compositor-side state
  - `client_active` - state known to the client
  - `current_serial` - received in last commit call
  - 'events' fields look like they'll need type casted, C things
  -     commit            - struct wlr_input_method_v2
  -     new_popup_surface - struct wlr_input_popup_surface_v2
  -     grab_keyboard     - struct wlr_input_method_keyboard_grab_v2
  -     destroy           - struct wlr_input_method_v2
  -}
{{ struct wlr/types/wlr_input_method_v2.h,
    wlr_input_method_v2,
    resource, Ptr WL_resource,

    seat, Ptr WLR_seat,
    seat_client, Ptr WLR_seat_client,

    pending, WLR_input_method_v2_state,
    current, WLR_input_method_v2_state,
    active, CBool,
    client_active, CBool,
    current_serial, Word32,

    popup_surfaces, WL_list,
    keyboard_grab, Ptr WLR_input_method_keyboard_grab_v2,

    link, WL_list,

    seat_client_destroy, WL_listener,

    events commit, WL_signal,
    events new_popup_surface, WL_signal,
    events grab_keyboard, WL_signal,
    events destroy, WL_signal
}}


{{ struct wlr/types/wlr_input_method_v2.h,
    wlr_input_popup_surface_v2,
    resource, Ptr WL_resource,
    input_method, Ptr WLR_input_method_v2,
    link, WL_list,

    surface, Ptr WLR_surface,

    events destroy, WL_signal,

    data, Ptr ()
}}


{- | events destroy - struct wlr_input_method_keyboard_grab_v2
 -}
{{ struct wlr/types/wlr_input_method_v2.h,
    wlr_input_method_keyboard_grab_v2,
    resource, Ptr WL_resource,
    input_method, Ptr WLR_input_method_v2,
    keyboard, Ptr WLR_keyboard,

    keyboard_keymap, WL_listener,
    keyboard_repeat_info, WL_listener,
    keyboard_destroy, WL_listener,

    events destroy, WL_signal
}}

{- | input_methods - struct wlr_input_method_v2.link
 - events destroy - struct wlr_input_method_manager_v2
 - events input_method - struct wlr_input_method_v2
 -}
{{ struct wlr/types/wlr_input_method_v2.h,
    wlr_input_method_manager_v2,
    global, Ptr WL_global,
    input_methods, WL_list,

    display_destroy, WL_listener,

    events input_method, WL_signal,
    events destroy, WL_signal 
}}

foreign import capi "wlr/types/wlr_input_method_v2.h wlr_input_method_manager_v2_create"
    wlr_input_method_manager_v2_create ::
        Ptr WL_display
        -> IO (Ptr WLR_input_method_manager_v2)

foreign import capi "wlr/types/wlr_input_method_v2.h wlr_input_method_v2_send_activate"
    wlr_input_method_v2_send_activate :: Ptr WLR_input_method_v2 -> IO ()

foreign import capi "wlr/types/wlr_input_method_v2.h wlr_input_method_v2_send_deactivate"
    wlr_input_method_v2_send_deactivate :: Ptr WLR_input_method_v2 -> IO ()

foreign import capi "wlr/types/wlr_input_method_v2.h wlr_input_method_v2_send_surrounding_text"
    wlr_input_method_v2_send_surrounding_text ::
        Ptr WLR_input_method_v2
        -> CString
        -> Word32
        -> Word32
        -> IO ()

foreign import capi "wlr/types/wlr_input_method_v2.h wlr_input_method_v2_send_content_type"
    wlr_input_method_v2_send_content_type ::
        Ptr WLR_input_method_v2
        -> Word32
        -> Word32
        -> IO ()

foreign import capi "wlr/types/wlr_input_method_v2.h wlr_input_method_v2_send_text_change_cause"
    wlr_input_method_v2_send_text_change_cause ::
        Ptr WLR_input_method_v2
        -> Word32
        -> IO ()

foreign import capi "wlr/types/wlr_input_method_v2.h wlr_input_method_v2_send_done"
    wlr_input_method_v2_send_done :: Ptr WLR_input_method_v2 -> IO ()

foreign import capi "wlr/types/wlr_input_method_v2.h wlr_input_method_v2_send_unavailable"
    wlr_input_method_v2_send_unavailable :: Ptr WLR_input_method_v2 -> IO ()

{- |
 - Get a struct wlr_input_popup_surface_v2 from a struct wlr_surface.
 -
 - Returns NULL if the surface has a different role or if the input popup
 - surface has been destroyed.
 -}
foreign import capi "wlr/types/wlr_input_method_v2.h wlr_input_popup_surface_v2_try_from_wlr_surface"
    wlr_input_popup_surface_v2_try_from_wlr_surface ::
        Ptr WLR_surface
        -> IO (Ptr WLR_input_popup_surface_v2)

foreign import capi "wlr/types/wlr_input_method_v2.h wlr_input_popup_surface_v2_send_text_input_rectangle"
    wlr_input_popup_surface_v2_send_text_input_rectangle ::
        Ptr WLR_input_popup_surface_v2
        -> Ptr WLR_box
        -> IO ()

-- | ... w32 time, w32 key, w32 state ...
foreign import capi "wlr/types/wlr_input_method_v2.h wlr_input_method_keyboard_grab_v2_send_key"
    wlr_input_method_keyboard_grab_v2_send_key ::
        Ptr WLR_input_method_keyboard_grab_v2
        -> Word32
        -> Word32
        -> Word32
        -> IO ()
foreign import capi "wlr/types/wlr_input_method_v2.h wlr_input_method_keyboard_grab_v2_send_modifiers"
    wlr_input_method_keyboard_grab_v2_send_modifiers ::
        Ptr WLR_input_method_keyboard_grab_v2
        -> Ptr WLR_keyboard_modifiers
        -> IO ()
foreign import capi "wlr/types/wlr_input_method_v2.h wlr_input_method_keyboard_grab_v2_set_keyboard"
    wlr_input_method_keyboard_grab_v2_set_keyboard ::
        Ptr WLR_input_method_keyboard_grab_v2
        -> Ptr WLR_keyboard
        -> IO ()
foreign import capi "wlr/types/wlr_input_method_v2.h wlr_input_method_keyboard_grab_v2_destroy"
    wlr_input_method_keyboard_grab_v2_destroy ::
        Ptr WLR_input_method_keyboard_grab_v2
        -> IO ()
