{- LANGUAGE PatternSynonyms -}
module WLR.Types.Compositor where
import WLR.Types.Buffer (WLR_client_buffer)
import WL.ServerCore (WL_resource)

#include <wlr/types/wlr_types_compositor.h>

--WLR_surface
-- the 'resource' field had a source comment, "//may be NULL"
{{ struct
    wlr/types/wlr_types_compositor.h,
    wlr_surface,

    resource, Ptr WL_resource,
    renderer, Maybe (Ptr WLR_renderer),
    buffer, WLR_client_buffer,
    buffer_damage, -- pixman_region32_t
    /**
     * The last commit's damage caused by surface and its subsurfaces'
     * movement, in surface-local coordinates.
     */
    pixman_region32_t external_damage;
    /**
     * The current opaque region, in surface-local coordinates. It is clipped to
     * the surface bounds. If the surface's buffer is using a fully opaque
     * format, this is set to the whole surface.
     */
    pixman_region32_t opaque_region;
    /**
     * The current input region, in surface-local coordinates. It is clipped to
     * the surface bounds.
     *
     * If the protocol states that the input region is ignored, this is empty.
     */
    pixman_region32_t input_region;
    /**
     * `current` contains the current, committed surface state. `pending`
     * accumulates state changes from the client between commits and shouldn't
     * be accessed by the compositor directly.
     */
    struct wlr_surface_state current, pending;

    struct wl_list cached; // wlr_surface_state.cached_link

    /**
     * Whether the surface is ready to be displayed.
     */
    bool mapped;

    /**
     * The lifetime-bound role of the surface. NULL if the role was never set.
     */
    const struct wlr_surface_role *role;

    /**
     * The role object representing the role. NULL if the role isn't
     * represented by any object or the object was destroyed.
     */
    struct wl_resource *role_resource;

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
