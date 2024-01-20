{- LANGUAGE PatternSynonyms -}
module WLR.Types.Compositor where

#include <wlr/types/wlr_types_compositor.h>

--WLR_surface
{{ struct
    wlr/types/wlr_types_compositor.h,
    wlr_surface,

	struct wl_resource *resource;
	struct wlr_renderer *renderer; // may be NULL
	/**
	 * The surface's buffer, if any. A surface has an attached buffer when it
	 * commits with a non-null buffer in its pending state. A surface will not
	 * have a buffer if it has never committed one, has committed a null buffer,
	 * or something went wrong with uploading the buffer.
	 */
	struct wlr_client_buffer *buffer;
	/**
	 * The last commit's buffer damage, in buffer-local coordinates. This
	 * contains both the damage accumulated by the client via
	 * `wlr_surface_state.surface_damage` and `wlr_surface_state.buffer_damage`.
	 * If the buffer has been resized, the whole buffer is damaged.
	 *
	 * This region needs to be scaled and transformed into output coordinates,
	 * just like the buffer's texture. In addition, if the buffer has shrunk the
	 * old size needs to be damaged and if the buffer has moved the old and new
	 * positions need to be damaged.
	 */
	pixman_region32_t buffer_damage;
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
