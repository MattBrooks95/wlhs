module WLR.Types.XdgShell where

#define WLR_USE_UNSTABLE
#include<wlr/types/wlr_xdg_shell.h>

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
{{ struct wlr/types/wlr_xdg_shell.h,
    wlr_xdg_toplevel,
	resource, Ptr WL_resource,
    base, Ptr WLR_xdg_surface,

    parent, Ptr WLR_xdg_toplevel,
    parent_unmap, WL_listener,

    current, WLR_xdg_toplevel_state,
    pending, WLR_xdg_toplevel_state,

    scheduled, WLR_xdg_toplevel_connfigure,

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
    grab_link, WL_list,
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
