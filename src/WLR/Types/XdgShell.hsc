{-# LANGUAGE PatternSynonyms #-}
module WLR.Types.XdgShell where

#define WLR_USE_UNSTABLE
#include<wlr/types/wlr_xdg_shell.h>
#include "xdg-shell-protocol.h"

import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr)
import Foreign.C.Types (CBool, CInt)
import Foreign.C.String (CString)
import Foreign (Word32, Int32)

import WL.ServerCore (WL_signal, WL_listener)
import WL.Utils (WL_list)
import WL.ServerCore (WL_resource, WL_signal, WL_listener)

import WLR.Types.Seat (WLR_seat)
import WLR.Types.Compositor (WLR_surface)
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

-- TODO this is a dummy export until I can figure out how to declare
-- the xdg_surface's C union type field
data WLR_xdg_surface

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

{- commented out until I figure out xdg_surface C union type
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
-}

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
 - xdg_popup. Our templating solution doesn't seem to mention unions TODO how to do this
 -}
{-
{{ struct wlr/types/wlr_xdg_shell.h,
    wlr_xdg_surface,
    client, Ptr WLR_xdg_client,
    resource, Ptr WL_resource,
    surface, Ptr WLR_surface,
    link, WL_list,
    role, WLR_xdg_surface_role,
    role_resource, Ptr WL_resource,


    // NULL if the role resource is inert
    union {
        struct wlr_xdg_toplevel *toplevel;
        struct wlr_xdg_popup *popup;
    };

    struct wl_list popups; // wlr_xdg_popup.link

    bool added, configured;
    struct wl_event_source *configure_idle;
    uint32_t scheduled_serial;
    struct wl_list configure_list;

    struct wlr_xdg_surface_state current, pending;

    // Whether the surface is ready to receive configure events
    bool initialized;
    // Whether the latest commit is an initial commit
    bool initial_commit;

    struct {
        struct wl_signal destroy;
        struct wl_signal ping_timeout;
        struct wl_signal new_popup;

        // for protocol extensions
        struct wl_signal configure; // struct wlr_xdg_surface_configure
        struct wl_signal ack_configure; // struct wlr_xdg_surface_configure
    } events;

    void *data;

    // private state

    struct wl_listener role_resource_destroy;
}}
-}
