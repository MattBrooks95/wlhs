{-# LANGUAGE PatternSynonyms #-}
module WLR.Types.XdgShell where

#define WLR_USE_UNSTABLE
#include<wlr/types/wlr_xdg_shell.h>

import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr)
import Foreign.C.Types (CBool(..), CInt(..), CDouble(..))
import Foreign.C.String (CString)
import Foreign (Word32, Int32)

import WL.ServerProtocol (WL_display)
import WL.ServerCore (WL_signal, WL_listener, WL_event_source, WL_global)
import WL.Utils (WL_list)
import WL.ServerCore (WL_resource, WL_signal, WL_listener)
import WL.Client (WL_client)

import WLR.Types.Seat (WLR_seat, WLR_seat_client)
import WLR.Types.Compositor (WLR_surface, WLR_surface_iterator_func_t)
import WLR.Types.Output (WLR_output)
import WLR.Util.Box (WLR_box)

{{ enum XDG_positioner_gravity,
    XDG_POSITIONER_GRAVITY_NONE,
    XDG_POSITIONER_GRAVITY_TOP,
    XDG_POSITIONER_GRAVITY_BOTTOM,
    XDG_POSITIONER_GRAVITY_LEFT,
    XDG_POSITIONER_GRAVITY_RIGHT,
    XDG_POSITIONER_GRAVITY_TOP_LEFT,
    XDG_POSITIONER_GRAVITY_BOTTOM_LEFT,
    XDG_POSITIONER_GRAVITY_TOP_RIGHT,
    XDG_POSITIONER_GRAVITY_BOTTOM_RIGHT
}}

{{ struct wlr/types/wlr_xdg_shell.h,
    wlr_xdg_positioner,
    resource, Ptr WL_resource,
    rules, Ptr WLR_xdg_positioner_rules,
}}

{{ struct wlr/types/wlr_xdg_shell,
    wlr_xdg_toplevel_state,
	maximized, CBool,
    fullscreen, CBool,
    resizing, CBool,
    activated, CBool,
    suspended, CBool,
	tiled, Word32,
	width, Int32,
    height, Int32,
	max_width, Int32,
    max_height, Int32,
    min_width, Int32,
    min_height, Int32
}}

{{ struct wlr/types/wlr_xdg_shell,
    wlr_xdg_popup_state,
    geometry, WLR_box,
    reactive, CBool
}}

{{ struct wlr/types/wlr_xdg_shell.h,
    wlr_xdg_toplevel_requested,
    maximized, CBool,
    minimized, CBool,
    fullscreen, CBool,
    fullscreen_output, Ptr WLR_output,
    fullscreen_output_destroy, WL_listener
}}

{{ struct wlr/types/wlr_xdg_shell.h,
    wlr_xdg_toplevel_configure,
    fields, Word32,
    maximized, CBool,
    fullscreen, CBool,
    resizing, CBool,
    activated, CBool,
    suspended, CBool,
    tiled, Word32,
    width, Int32,
    height, Int32,
    bounds width, Int32,
    bounds height, Int32,
    wm_capabilities, Word32
}}

{{ enum XDG_positioner_anchor,
    XDG_POSITIONER_ANCHOR_NONE,
    XDG_POSITIONER_ANCHOR_TOP,
    XDG_POSITIONER_ANCHOR_BOTTOM,
    XDG_POSITIONER_ANCHOR_LEFT,
    XDG_POSITIONER_ANCHOR_RIGHT,
    XDG_POSITIONER_ANCHOR_TOP_LEFT,
    XDG_POSITIONER_ANCHOR_BOTTOM_LEFT,
    XDG_POSITIONER_ANCHOR_TOP_RIGHT,
    XDG_POSITIONER_ANCHOR_BOTTOM_RIGHT
}}

{-
 - @ingroup iface_xdg_positioner
 - constraint adjustments
 -
 - The constraint adjustment value define ways the compositor will adjust
 - the position of the surface, if the unadjusted position would result
 - in the surface being partly constrained.
 -
 - Whether a surface is considered 'constrained' is left to the compositor
 - to determine. For example, the surface may be partly outside the
 - compositor's defined 'work area', thus necessitating the child surface's
 - position be adjusted until it is entirely inside the work area.
 -
 - The adjustments can be combined, according to a defined precedence: 1)
 - Flip, 2) Slide, 3) Resize.
 -}
{{ enum XDG_positioner_constraint_adjustment,
    XDG_POSITIONER_CONSTRAINT_ADJUSTMENT_NONE,
    XDG_POSITIONER_CONSTRAINT_ADJUSTMENT_SLIDE_X,
    XDG_POSITIONER_CONSTRAINT_ADJUSTMENT_SLIDE_Y,
    XDG_POSITIONER_CONSTRAINT_ADJUSTMENT_FLIP_X,
    XDG_POSITIONER_CONSTRAINT_ADJUSTMENT_FLIP_Y,
    XDG_POSITIONER_CONSTRAINT_ADJUSTMENT_RESIZE_X,
    XDG_POSITIONER_CONSTRAINT_ADJUSTMENT_RESIZE_Y
}}

{{ struct wlr/types/wlr_xdg_shell.h,
    wlr_xdg_positioner_rules,
    anchor_rect, WLR_box,
    anchor, XDG_positioner_anchor,
    gravity, XDG_positioner_gravity,
    constraint_adjustment, XDG_positioner_constraint_adjustment,
    reactive, CBool,
    has_parent_configure_serial, CBool,
    parent_configure_serial, Word32,
    size width, Int32,
    size height, Int32,
    parent_size width, Int32,
    parent_size height, Int32,
    offset x, Int32,
    offset y, Int32
}}

{{ struct wlr/types/wlr_xdg_shell.h,
    wlr_xdg_popup_configure,
    fields, Word32,
    geometry, WLR_box,
    rules, WLR_xdg_positioner_rules,
    reposition_token, Word32
}}

{-
 - on the events:
 - Note: as per xdg-shell protocol, the compositor has to
 - handle state requests by sending a configure event,
 - even if it didn't actually change the state. Therefore,
 - every compositor implementing xdg-shell support *must*
 - listen to these signals and schedule a configure event
 - immediately or at some time in the future; not doing so
 - is a protocol violation.
 -}
{- commented out until I figure out xdg_surface C union type
{{ struct wlr/types/wlr_xdg_shell.h,
    wlr_xdg_toplevel,
	resource, Ptr WL_resource,
    base, Ptr WLR_xdg_surface,

    parent, Ptr WLR_xdg_toplevel,
    parent_unmap, WL_listener,

    current, WLR_xdg_toplevel_state,
    pending, WLR_xdg_toplevel_state,

    scheduled, WLR_xdg_toplevel_configure,

    requested, WLR_xdg_toplevel_requested,

    title, CString,
    app_id, CString,

    events request_maximize, WL_signal,
    events request_fullscreen, WL_signal,

    events request_minimize, WL_signal,
    events request_move, WL_signal,
    events request_resize, WL_signal,
    events request_show_window_menu, WL_signal,
    events set_parent, WL_signal,
    events set_title, WL_signal,
    events set_app_id, WL_signal
}}
-}

{-| this enum tells you which element of wlr_xdg_surfaces union type exists
-}
{{ enum WLR_xdg_surface_role,
    WLR_XDG_SURFACE_ROLE_NONE,
    WLR_XDG_SURFACE_ROLE_TOPLEVEL,
    WLR_XDG_SURFACE_ROLE_POPUP
}}

{{ struct wlr/types/wlr_xdg_shell.h,
    wlr_xdg_surface_state,
    configure_serial, Word32,
    geometry, WLR_box
}}

{{ struct wlr/types/wlr_xdg_shell.h,
    wlr_xdg_client,
    shell, Ptr WLR_xdg_shell,
    resource, Ptr WL_resource,
    client, Ptr WL_client,
    surfaces, WL_list,
    link, WL_list,
    ping_serial, Word32,
    ping_timer, Ptr WL_event_source
}}

{{ struct wlr/types/wlr_xdg_shell.h,
    wlr_xdg_shell,
    global, Ptr WL_global,
    version, Word32,
    clients, WL_list,
    popup_grabs, WL_list,
    ping_timeout, Word32,
    display_destroy, WL_listener,
    events new_surface, WL_signal,
    events destroy, WL_signal,
    data, Ptr ()
}}

{-
// Note: as per xdg-shell protocol, the compositor has to
// handle state requests by sending a configure event,
// even if it didn't actually change the state. Therefore,
// every compositor implementing xdg-shell support *must*
// listen to these signals and schedule a configure event
// immediately or at some time in the future; not doing so
// is a protocol violation.
-}
{{ struct wlr/types/wlr_xdg_shell.h,
    wlr_xdg_toplevel,
    resource, Ptr WL_resource,
    base, Ptr WLR_xdg_surface,
    parent, Ptr WLR_xdg_toplevel,
    parent_unmap, WL_listener,
    current, WLR_xdg_toplevel_state,
    pending, WLR_xdg_toplevel_state,
    scheduled, WLR_xdg_toplevel_configure,
    requested, WLR_xdg_toplevel_requested,
    title, CString,
    app_id, CString,

    events request_maximize, WL_signal,
    events request_fullscreen, WL_signal,
    events request_minimize, WL_signal,
    events request_move, WL_signal,
    events request_resize, WL_signal,
    events request_show_window_menu, WL_signal,
    events set_parent, WL_signal,
    events set_title, WL_signal,
    events set_app_id, WL_signal,
}}

{{ struct wlr/types/wlr_xdg_shell.h,
    wlr_xdg_popup,
    base, Ptr WLR_xdg_surface,
    link, WL_list,
    resource, Ptr WL_resource,
    sent_initial_configure, CBool,
    parent, Ptr WLR_surface,
    seat, Ptr WLR_seat,
    scheduled, WLR_xdg_popup_configure,
    current, WLR_xdg_popup_state,
    pending, WLR_xdg_popup_state,
    events reposition, WL_signal,
    grab_link, WL_list
}}

{-
 - An xdg-surface is a user interface element requiring management by the
 - compositor. An xdg-surface alone isn't useful, a role should be assigned to
 - it in order to map it.
 -
 - role
 - The lifetime-bound role of the xdg_surface. WLR_XDG_SURFACE_ROLE_NONE
 - if the role was never set.
 -
 - role_resource
 - The role object representing the role. NULL if the object was destroyed.
 -
 - NOTE hsroots used empty data declarations for the C union types xdg_toplevel
 - xdg_popup. Our templating solution doesn't seem to mention unions, hsc2hs doesn't support unions either
 - 
 - therefore the 'toplevel' field of this data declaration is cursed
 - in this case, both values of the union are pointers, so users of this data type will
 - need to use unsafeCastPtr (or something BurningWitness suggested) based on the value of the role field
    union {
        struct wlr_xdg_toplevel *toplevel;
        struct wlr_xdg_popup *popup;
    };
 -}
{{ struct wlr/types/wlr_xdg_shell.h,
    wlr_xdg_surface,
    client, Ptr WLR_xdg_client,
    resource, Ptr WL_resource,
    surface, Ptr WLR_surface,
    link, WL_list,
    role, WLR_xdg_surface_role,
    role_resource, Ptr WL_resource,
    toplevel, Ptr WLR_xdg_toplevel,
    popups, WL_list,
    added, CBool,
    configured, CBool,
    configure_idle, Ptr WL_event_source,
    scheduled_serial, Word32,
    configure_list, WL_list,
    current, WLR_xdg_surface_state,
    pending, WLR_xdg_surface_state,
    initialized, CBool,
    initial_commit, CBool,
    events destroy, WL_signal,
    events ping_timeout, WL_signal,
    events new_popup, WL_signal,
    events configure, WL_signal,
    events ack_configure, WL_signal,
    data, Ptr (),
    role_resource_destroy, WL_listener
}}

{{ struct wlr/types/wlr_xdg_shell.h,
    wlr_xdg_toplevel_move_event,
    toplevel, Ptr WLR_xdg_toplevel,
    seat, Ptr WLR_seat,
    serial, Word32
}}

{{ struct wlr/types/wlr_xdg_shell.h,
    wlr_xdg_toplevel_resize_event,
    toplevel, Ptr WLR_xdg_toplevel,
    seat, Ptr WLR_seat_client,
    serial, Word32,
    edges, Word32
}}

{{ struct wlr/types/wlr_xdg_shell.h,
    wlr_xdg_toplevel_show_window_menu_event,
    toplevel, Ptr WLR_xdg_toplevel,
    seat, Ptr WLR_seat_client,
    serial, Word32,
    x, Int32,
    y, Int32
}}

{-|
 - Create the xdg_wm_base global with the specified version.
 -}
foreign import capi "wlr/types/wlr_xdg_shell.h wlr_xdg_shell_create"
    wlr_xdg_shell_create :: Ptr WL_display -> Word32 -> IO (Ptr WLR_xdg_shell)

{-| Get the corresponding struct wlr_xdg_surface from a resource.
 -
 - Aborts if the resource doesn't have the correct type. Returns NULL if the
 - resource is inert.
 -}
foreign import capi "wlr/types/wlr_xdg_shell.h wlr_xdg_surface_from_resource"
    wlr_xdg_surface_from_resource :: Ptr WL_resource -> IO (Ptr WLR_xdg_surface)

{-| Get the corresponding struct wlr_xdg_popup from a resource.
 -
 - Aborts if the resource doesn't have the correct type. Returns NULL if the
 - resource is inert.
 -}
foreign import capi "wlr/types/wlr_xdg_shell.h wlr_xdg_popup_from_resource"
    wlr_xdg_popup_from_resource :: Ptr WL_resource -> IO (Ptr WLR_xdg_popup)

{- Get the corresponding struct wlr_xdg_toplevel from a resource.
 -
 - Aborts if the resource doesn't have the correct type. Returns NULL if the
 - resource is inert.
 -}
foreign import capi "wlr/types/wlr_xdg_shell.h wlr_xdg_toplevel_from_resource"
    wlr_xdg_toplevel_from_resource :: Ptr WL_resource -> IO (Ptr WLR_xdg_toplevel)

{-| Get the corresponding struct wlr_xdg_positioner from a resource.
 -
 - Aborts if the resource doesn't have the correct type.
 -}
foreign import capi "wlr/types/wlr_xdg_shell.h wlr_xdg_positioner_from_resource"
    wlr_xdg_positioner_from_resource :: Ptr WL_resource -> IO (Ptr WLR_xdg_positioner)

{-|
 - Send a ping to the surface. If the surface does not respond in a reasonable
 - amount of time, the ping_timeout event will be emitted.
 -}
foreign import capi "wlr/types/wlr_xdg_shell.h wlr_xdg_surface_ping"
    wlr_xdg_surface_ping :: Ptr WLR_xdg_surface -> IO ()

{-|
 - Request that this toplevel surface be the given size. Returns the associated
 - configure serial.
 - toplevel -> width -> height ->
 -}
foreign import capi "wlr/types/wlr_xdg_shell.h wlr_xdg_toplevel_set_size"
    wlr_xdg_toplevel_set_size :: Ptr WLR_xdg_toplevel -> Int32 -> Int32 -> IO Word32

{-|
 - Request that this toplevel show itself in an activated or deactivated
 - state. Returns the associated configure serial.
 -}
foreign import capi "wlr/types/wlr_xdg_shell.h wlr_xdg_toplevel_set_activated"
    wlr_xdg_toplevel_set_activated :: Ptr WLR_xdg_toplevel -> CBool -> IO Word32

{-|
 - Request that this toplevel consider itself maximized or not
 - maximized. Returns the associated configure serial.
 -}
foreign import capi "wlr/types/wlr_xdg_shell.h wlr_xdg_toplevel_set_maximized"
    wlr_xdg_toplevel_set_maximized :: Ptr WLR_xdg_toplevel -> CBool -> IO Word32

{-|
 - Request that this toplevel consider itself fullscreen or not
 - fullscreen. Returns the associated configure serial.
 -}
foreign import capi "wlr/types/wlr_xdg_shell.h wlr_xdg_toplevel_set_fullscreen"
    wlr_xdg_toplevel_set_fullscreen :: Ptr WLR_xdg_toplevel -> CBool -> IO Word32

{-|
 - Request that this toplevel consider itself to be resizing or not
 - resizing. Returns the associated configure serial.
 -}
foreign import capi "wlr/types/wlr_xdg_shell.h wlr_xdg_toplevel_set_resizing"
    wlr_xdg_toplevel_set_resizing :: Ptr WLR_xdg_toplevel -> CBool -> IO Word32

{-|
 - Request that this toplevel consider itself in a tiled layout and some
 - edges are adjacent to another part of the tiling grid. `tiled_edges` is a
 - bitfield of enum wlr_edges. Returns the associated configure serial.
 -}
foreign import capi "wlr/types/wlr_xdg_shell.h wlr_xdg_toplevel_set_tiled"
    wlr_xdg_toplevel_set_tiled :: Ptr WLR_xdg_toplevel -> Word32 -> IO Word32

{-|
 -  Configure the recommended bounds for the client's window geometry size.
 -  Returns the associated configure serial.
 -  toplevel -> width -> height ->
 -}
foreign import capi "wlr/types/wlr_xdg_shell.h wlr_xdg_toplevel_set_bounds"
    wlr_xdg_toplevel_set_bounds :: Ptr WLR_xdg_toplevel -> Int32 -> Int32 -> IO Word32

{-|
 -  Configure the window manager capabilities for this toplevel. `caps` is a
 -  bitfield of `enum wlr_xdg_toplevel_wm_capabilities`. Returns the associated
 -  configure serial.
 -}
foreign import capi "wlr/types/wlr_xdg_shell.h wlr_xdg_toplevel_set_wm_capabilities"
    wlr_xdg_toplevel_set_wm_capabilities :: Ptr WLR_xdg_toplevel -> Word32 -> IO Word32

{-|
 -  Request that this toplevel consider itself suspended or not
 -  suspended. Returns the associated configure serial.
 -}
foreign import capi "wlr/types/wlr_xdg_shell.h wlr_xdg_toplevel_set_suspended"
    wlr_xdg_toplevel_set_suspended :: Ptr WLR_xdg_toplevel -> CBool -> IO Word32

{-|
 -  Request that this toplevel closes.
 -}
foreign import capi "wlr/types/wlr_xdg_shell.h wlr_xdg_toplevel_send_close"
    wlr_xdg_toplevel_send_close :: Ptr WLR_xdg_toplevel -> IO ()

{-|
 -  Sets the parent of this toplevel. Parent can be NULL.
 - 
 -  Returns true on success, false if setting the parent would create a loop.
 -}
foreign import capi "wlr/types/wlr_xdg_shell.h wlr_xdg_toplevel_set_parent"
    wlr_xdg_toplevel_set_parent :: Ptr WLR_xdg_toplevel -> Ptr WLR_xdg_toplevel -> IO CBool

{-|
 -  Notify the client that the popup has been dismissed and destroy the
 -  struct wlr_xdg_popup, rendering the resource inert.
 -}
foreign import capi "wlr/types/wlr_xdg_shell.h wlr_xdg_popup_destroy"
    wlr_xdg_popup_destroy :: Ptr WLR_xdg_popup -> IO ()

{-|
 -  Get the position for this popup in the surface parent's coordinate system.
 -  popup -> sx -> sy ->
 -}
foreign import capi "wlr/types/wlr_xdg_shell.h wlr_xdg_popup_get_position"
    wlr_xdg_popup_get_position :: Ptr WLR_xdg_popup -> Ptr CDouble -> Ptr CDouble -> IO ()

{-|
 -  Get the geometry based on positioner rules.
 -}
foreign import capi "wlr/types/wlr_xdg_shell.h wlr_xdg_positioner_rules_get_geometry"
    wlr_xdg_positioner_rules_get_geometry :: Ptr WLR_xdg_positioner_rules -> Ptr WLR_box -> IO ()

{-|
 -  Unconstrain the box from the constraint area according to positioner rules.
 -}
foreign import capi "wlr/types/wlr_xdg_shell.h wlr_xdg_positioner_rules_unconstrain_box"
    wlr_xdg_positioner_rules_unconstrain_box :: Ptr WLR_xdg_positioner_rules -> Ptr WLR_box -> Ptr WLR_box -> IO ()

{-|
 -  Convert the given coordinates in the popup coordinate system to the toplevel
 -  surface coordinate system.
 -  popup -> popup sx -> popup sy -> toplevel sx -> toplevel sy ->
 -}
foreign import capi "wlr/types/wlr_xdg_shell.h wlr_xdg_popup_get_toplevel_coords"
    wlr_xdg_popup_get_toplevel_coords :: Ptr WLR_xdg_popup -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> IO ()

{-|
 -  Set the geometry of this popup to unconstrain it according to its
 -  xdg-positioner rules. The box should be in the popup's root toplevel parent
 -  surface coordinate system.
 -}
foreign import capi "wlr/types/wlr_xdg_shell.h wlr_xdg_popup_unconstrain_from_box"
    wlr_xdg_popup_unconstrain_from_box :: Ptr WLR_xdg_popup -> Ptr WLR_box -> IO ()

{-|
 -  Find a surface within this xdg-surface tree at the given surface-local
 -  coordinates. Returns the surface and coordinates in the leaf surface
 -  coordinate system or NULL if no surface is found at that location.
 -}
foreign import capi "wlr/types/wlr_xdg_shell.h wlr_xdg_surface_surface_at"
    wlr_xdg_surface_surface_at :: Ptr WLR_xdg_surface -> CDouble -> CDouble -> Ptr CDouble -> Ptr CDouble -> IO (Ptr WLR_surface)

{-|
 -  Find a surface within this xdg-surface's popup tree at the given
 -  surface-local coordinates. Returns the surface and coordinates in the leaf
 -  surface coordinate system or NULL if no surface is found at that location.
 -  x -> y -> x -> y
 -}
foreign import capi "wlr/types/wlr_xdg_shell.h wlr_xdg_surface_popup_surface_at"
    wlr_xdg_surface_popup_surface_at :: Ptr WLR_xdg_surface -> CDouble -> CDouble -> Ptr CDouble -> Ptr CDouble -> IO (Ptr WLR_surface)

{-|
 -  Get a struct wlr_xdg_surface from a struct wlr_surface.
 - 
 -  Returns NULL if the surface doesn't have the xdg_surface role or
 -  if the xdg_surface has been destroyed.
 -}
foreign import capi "wlr/types/wlr_xdg_shell.h wlr_xdg_surface_try_from_wlr_surface"
    wlr_xdg_surface_try_from_wlr_surface :: Ptr WLR_surface -> IO (Ptr WLR_xdg_surface)

{-|
 -  Get a struct wlr_xdg_toplevel from a struct wlr_surface.
 - 
 -  Returns NULL if the surface doesn't have the xdg_surface role, the
 -  xdg_surface is not a toplevel, or the xdg_surface/xdg_toplevel objects have
 -  been destroyed.
 -}
foreign import capi "wlr/types/wlr_xdg_shell.h wlr_xdg_toplevel_try_from_wlr_surface"
    wlr_xdg_toplevel_try_from_wlr_surface :: Ptr WLR_surface -> IO (Ptr WLR_xdg_toplevel)

{-|
 -  Get a struct wlr_xdg_popup from a struct wlr_surface.
 - 
 -  Returns NULL if the surface doesn't have the xdg_surface role, the
 -  xdg_surface is not a popup, or the xdg_surface/xdg_popup objects have
 -  been destroyed.
 -}
foreign import capi "wlr/types/wlr_xdg_shell.h wlr_xdg_popup_try_from_wlr_surface"
    wlr_xdg_popup_try_from_wlr_surface :: Ptr WLR_surface -> IO (Ptr WLR_xdg_popup)

{-|
 -  Get the surface geometry.
 - 
 -  This is either the geometry as set by the client, or defaulted to the bounds
 -  of the surface + the subsurfaces (as specified by the protocol).
 - 
 -  The x and y value can be < 0.
 -}
foreign import capi "wlr/types/wlr_xdg_shell.h wlr_xdg_surface_get_geometry"
    wlr_xdg_surface_get_geometry :: Ptr WLR_xdg_surface -> Ptr WLR_box -> IO ()

{-|
 -  Call `iterator` on each mapped surface and popup in the xdg-surface tree
 -  (whether or not this xdg-surface is mapped), with the surface's position
 -  relative to the root xdg-surface. The function is called from root to leaves
 -  (in rendering order).
 -  `Ptr ()` is user_data
 -}
foreign import capi "wlr/types/wlr_xdg_shell.h wlr_xdg_surface_for_each_surface"
    wlr_xdg_surface_for_each_surface :: Ptr WLR_xdg_surface -> WLR_surface_iterator_func_t -> Ptr () -> IO ()

{-|
 -  Call `iterator` on each mapped popup's surface and popup's subsurface in the
 -  xdg-surface tree (whether or not this xdg-surface is mapped), with the
 -  surfaces's position relative to the root xdg-surface. The function is called
 -  from root to leaves (in rendering order).
 -  `Ptr ()` is user_data
 -}
foreign import capi "wlr/types/wlr_xdg_shell.h wlr_xdg_surface_for_each_popup_surface"
    wlr_xdg_surface_for_each_popup_surface :: Ptr WLR_xdg_surface -> WLR_surface_iterator_func_t -> Ptr () -> IO ()

{-|
 -  Schedule a surface configuration. This should only be called by protocols
 -  extending the shell.
 -}
foreign import capi "wlr/types/wlr_xdg_shell.h wlr_xdg_surface_schedule_configure"
    wlr_xdg_surface_schedule_configure :: Ptr WLR_xdg_surface -> IO Word32
