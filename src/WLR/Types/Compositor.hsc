{-# LANGUAGE PatternSynonyms #-}
module WLR.Types.Compositor where

import WLR.Types.Buffer (WLR_client_buffer)
import WL.ServerCore (WL_resource)

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_compositor.h>

--WLR_surface
-- the 'resource' field had a source comment, "//may be NULL"
{{ struct
    wlr/types/wlr_compositor.h,
    wlr_surface,
    resource, Ptr WL_resource,
    renderer, Maybe (Ptr WLR_renderer),
    buffer, WLR_client_buffer,
    buffer_damage, -- pixman_region32_t
    external_damage, pixman_region_32,
    opaque_region, pixman_region_32,
    input_region, pixman_region_32,
    current, WLR_surface_state,
    pending, WLR_surface_state,
    cached, WL_list,
    mapped, CBool,
    role, Ptr WLR_surface_role,
    role_resource, Ptr WL_resource,


    struct {
        struct wl_signal client_commit;
        struct wl_signal precommit; // const struct wlr_surface_state *
        struct wl_signal commit;

        /**
         * The `map` event signals that the surface has a non-null buffer
         * committed and is ready to be displayed.
         */
        struct wl_signal map;
        /**
         * The `unmap` event signals that the surface shouldn't be displayed
         * anymore. This can happen when a null buffer is committed,
         * the associated role object is destroyed, or when the role-specific
         * conditions for the surface to be mapped no longer apply.
         */
        struct wl_signal unmap;

        struct wl_signal new_subsurface;
        struct wl_signal destroy;
    } events;

    struct wl_list current_outputs; // wlr_surface_output.link

    struct wlr_addon_set addons;
    void *data;

    // private state

    struct wl_listener renderer_destroy;
    struct wl_listener role_resource_destroy;

    struct {
        int32_t scale;
        enum wl_output_transform transform;
        int width, height;
        int buffer_width, buffer_height;
    } previous;

    bool unmap_commit;

    bool opaque;
    bool has_buffer;

    int32_t preferred_buffer_scale;
    bool preferred_buffer_transform_sent;
    enum wl_output_transform preferred_buffer_transform;
 };
