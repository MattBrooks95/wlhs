{-# LANGUAGE PatternSynonyms #-}
module WLR.Types.Compositor where

import Foreign (Word32, Int32)
import Foreign.C.Types (CBool(..), CDouble(..), CInt(..), CSize)
import Foreign.C.String (CString)
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, FunPtr)

import WL.Utils (WL_list)
import WL.ServerCore (WL_resource, WL_signal, WL_listener, WL_global)
import WL.ServerProtocol (WL_output_transform, WL_display)

import WLR.Render.Texture (WLR_texture)
import WLR.Render.Renderer (WLR_renderer)
import WLR.Types.Buffer (WLR_client_buffer, WLR_buffer)
import WLR.Types.Output (WLR_output)
import WLR.Util.Box (WLR_box, WLR_fbox)
import WLR.Util.Addon (WLR_addon_set)

import PIXMAN.Pixman (PIXMAN_region32)
import Time.Time (TIMESPEC)


#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_compositor.h>

{-| iterator function
 - surface -> coordinate x -> coordinate y -> user data -> void
 -}
type WLR_surface_iterator_func_t = FunPtr (Ptr WLR_surface -> CInt -> CInt -> Ptr () -> IO ())

--WLR_surface
-- the 'resource' field had a source comment, "//may be NULL"
{{ struct
    wlr/types/wlr_compositor.h,
    wlr_surface,
    resource, Ptr WL_resource,
    renderer, Ptr WLR_renderer,
    buffer, WLR_client_buffer,
    buffer_damage,  PIXMAN_region32,
    external_damage, PIXMAN_region32,
    opaque_region, PIXMAN_region32,
    input_region, PIXMAN_region32,
    current, WLR_surface_state,
    pending, WLR_surface_state,
    cached, WL_list,
    mapped, CBool,
    role, Ptr WLR_surface_role,
    role_resource, Ptr WL_resource,
    events client_commit, WL_signal,
    events precommit, WL_signal,
    events commit, WL_signal,
    events map, WL_signal,
    events unmap, WL_signal,
    events new_subsurface, WL_signal,
    events destroy, WL_signal,
    current_outputs, WL_list,
    addons, WLR_addon_set,
    renderer_destroy, WL_listener,
    role_resource_destroy, WL_listener,
    previous scale, Int32,
    previous transform, WL_output_transform,
    previous width, CInt,
    previous height, CInt,
    previous buffer_width, CInt,
    previous buffer_height, CInt,
    unmap_commit, CBool,
    opaque, CBool,
    has_buffer, CBool,
    preferred_buffer_scale, Int32,
    preferred_buffer_transform_sent, CBool,
    preferred_buffer_transform, WL_output_transform
}}

{{ struct
    wlr/types/wlr_compositor.h,
    wlr_surface_state,
    committed, Word32,
    seq, Word32,
    buffer, Ptr WLR_buffer,
    dx, Int32,
    dy, Int32,
    surface_damage, PIXMAN_region32,
    buffer_damage, PIXMAN_region32,
    opaque, PIXMAN_region32,
    input, PIXMAN_region32,
    transform, WL_output_transform,
    scale, Int32,
    frame_callback_list, WL_list,
    width, CInt,
    height, CInt,
    buffer_width, CInt,
    buffer_height, CInt,
    subsurfaces_below, WL_list,
    subsurfaces_above, WL_list,
    viewport has_src, CBool,
    viewport has_dst, CBool,
    viewport src, WLR_fbox,
    viewport dst_width, CInt,
    viewport dst_height, CInt,
    cached_state_locks, CSize,
    cached_state_link, WL_list
}}

{{ struct
    wlr/types/wlr_compositor.h,
    wlr_surface_role,
    name, CString,
    no_object, CBool,
    commit, FunPtr (Ptr WLR_surface -> IO ()),
    unmap, FunPtr (Ptr WLR_surface -> IO ()),
    destroy, FunPtr (Ptr WLR_surface -> IO())
}}

{{ struct
    wlr/types/wlr_compositor.h,
    wlr_compositor,
    global, Ptr WL_global,
    renderer, Ptr WLR_renderer,
    display_destroy, WL_listener,
    events new_surface, WL_signal,
    events destroy, WL_signal
}}

{-
 - Set the lifetime role for this surface.
 -
 - If the surface already has a different role and/or has a role object set,
 - the function fails and sends an error to the client.
 -
 - Returns true on success, false otherwise.
 - TODO the WLR_surface_role pointer can be const
 -}
foreign import capi "wlr/types/wlr_compositor.h wlr_surface_set_role"
    wlr_surface_set_role ::
        Ptr WLR_surface ->
        Ptr WLR_surface_role ->
        Ptr WL_resource ->
        Word32 ->
        IO (CBool)

type Void = IO ()

{-
 - Set the role object for this surface. The surface must have a role and
 - no already set role object.
 -
 - When the resource is destroyed, the surface is unmapped,
 - wlr_surface_role.destroy is called and the role object is unset.
 -}
foreign import capi "wlr/types/wlr_compositor.h wlr_surface_set_role_object"
    wlr_surface_set_role_object :: Ptr WLR_surface -> Ptr WL_resource -> IO ()

{-
 - Map the surface. If the surface is already mapped, this is no-op.
 -
 - This function must only be used by surface role implementations.
 -}
foreign import capi "wlr/types/wlr_compositor.h wlr_surface_map"
    wlr_surface_map :: Ptr WLR_surface -> IO ()

{-
 - Unmap the surface. If the surface is already unmapped, this is no-op.
 -
 - This function must only be used by surface role implementations.
 -}
foreign import capi "wlr/types/wlr_compositor.h wlr_surface_unmap"
    wlr_surface_unmap :: Ptr WLR_surface -> IO ()

{-
 - Whether or not this surface currently has an attached buffer. A surface has
 - an attached buffer when it commits with a non-null buffer in its pending
 - state. A surface will not have a buffer if it has never committed one, has
 - committed a null buffer, or something went wrong with uploading the buffer.
 -
 - NOTE _func suffix is to disambiguate it with the generated 'wlr_surface_has_buffer'
 - record field for the wlr_surface type
 -}
foreign import capi "wlr/types/wlr_compositor.h wlr_surface_has_buffer"
    wlr_surface_has_buffer_func :: Ptr WLR_surface -> IO (CBool)

{-
 - Get the texture of the buffer currently attached to this surface. Returns
 - NULL if no buffer is currently attached or if something went wrong with
 - uploading the buffer.
 -}
foreign import capi "wlr/types/wlr_compositor.h wlr_surface_get_texture"
    wlr_surface_get_texture :: Ptr WLR_surface -> IO (Ptr WLR_texture)

{-
 - Get the root of the subsurface tree for this surface. Can return NULL if
 - a surface in the tree has been destroyed.
 -}
foreign import capi "wlr/types/wlr_compositor.h wlr_surface_get_root_surface"
    wlr_surface_get_root_surface :: Ptr WLR_surface -> IO (Ptr WLR_surface)

{-
 - Check if the surface accepts input events at the given surface-local
 - coordinates. Does not check the surface's subsurfaces.
 -}
foreign import capi "wlr/types/wlr_compositor.h wlr_surface_point_accepts_input"
    wlr_surface_point_accepts_input :: Ptr WLR_surface -> CDouble -> CDouble -> IO (CBool)

{-
 - Find a surface in this surface's tree that accepts input events and has all
 - parents mapped (except this surface, which can be unmapped) at the given
 - surface-local coordinates. Returns the surface and coordinates in the leaf
 - surface coordinate system or NULL if no surface is found at that location.
 -}
foreign import capi "wlr/types/wlr_compositor.h wlr_surface_surface_at"
    wlr_surface_surface_at ::
        Ptr WLR_surface ->
        CDouble ->
        CDouble ->
        Ptr CDouble ->
        Ptr CDouble ->
        IO (Ptr WLR_surface)

{-
 - Notify the client that the surface has entered an output.
 -
 - This is a no-op if the surface has already entered the output.
 -}
foreign import capi "wlr/types/wlr_compositor.h wlr_surface_send_enter"
    wlr_surface_send_enter :: Ptr WLR_surface -> Ptr WLR_output -> Void

{-
 - Notify the client that the surface has left an output.
 -
 - This is a no-op if the surface has already left the output.
 -}
foreign import capi "wlr/types/wlr_compositor.h wlr_surface_send_leave"
    wlr_surface_send_leave :: Ptr WLR_surface -> Ptr WLR_output -> Void

{-
 - Complete the queued frame callbacks for this surface.
 -
 - This will send an event to the client indicating that now is a good time to
 - draw its next frame.
 - TODO timespec pointer can be const
 -}
foreign import capi "wlr/types/wlr_compositor.h wlr_surface_send_frame_done"
    wlr_surface_send_frame_done :: Ptr WLR_surface -> Ptr TIMESPEC -> Void

{-
 - Get the bounding box that contains the surface and all subsurfaces in
 - surface coordinates.
 - X and y may be negative, if there are subsurfaces with negative position.
 -}
foreign import capi "wlr/types/wlr_compositor.h wlr_surface_get_extends"
    wlr_surface_get_extends :: Ptr WLR_surface -> Ptr WLR_box -> Void

{-
 - Get the struct wlr_surface corresponding to a wl_surface resource.
 -
 - This asserts that the resource is a valid wl_surface resource created by
 - wlroots and will never return NULL.
 -}
foreign import capi "wlr/types/wlr_compositor.h wlr_surface_from_resource"
    wlr_surface_from_resource :: Ptr WL_resource -> IO (Ptr WLR_surface)

{-
 - Call `iterator` on each mapped surface in the surface tree (whether or not
 - this surface is mapped), with the surface's position relative to the root
 - surface. The function is called from root to leaves (in rendering order).
 -}
foreign import capi "wlr/types/wlr_compositor.h wlr_surface_for_each_surface"
    wlr_surface_for_each_surface ::
        Ptr WLR_surface ->
        WLR_surface_iterator_func_t ->
        Ptr () ->
        Void

{-
 - Get the effective surface damage in surface-local coordinate space. Besides
 - buffer damage, this includes damage induced by resizing and moving the
 - surface and its subsurfaces. The resulting damage is not expected to be
 - bounded by the surface itself.
 -}
foreign import capi "wlr/types/wlr_compositor.h wlr_surface_get_effective_damage"
    wlr_surface_get_effective_damage :: Ptr WLR_surface -> Ptr PIXMAN_region32 -> Void

{-
 - Get the source rectangle describing the region of the buffer that needs to
 - be sampled to render this surface's current state. The box is in
 - buffer-local coordinates.
 -
 - If the viewport's source rectangle is unset, the position is zero and the
 - size is the buffer's.
 -}
foreign import capi "wlr/types/wlr_compositor.h wlr_surface_get_buffer_source_box"
    wlr_surface_get_buffer_source_box :: Ptr WLR_surface -> Ptr WLR_fbox -> Void

{-
 - Acquire a lock for the pending surface state.
 -
 - The state won't be committed before the caller releases the lock. Instead,
 - the state becomes cached. The caller needs to use wlr_surface_unlock_cached()
 - to release the lock.
 -
 - Returns a surface commit sequence number for the cached state.
 -}
foreign import capi "wlr/types/wlr_compositor.h wlr_surface_lock_pending"
    wlr_surface_lock_pending :: Ptr WLR_surface -> IO (Word32)

{-
 - Release a lock for a cached state.
 -
 - Callers should not assume that the cached state will immediately be
 - committed. Another caller may still have an active lock.
 -}
foreign import capi "wlr/types/wlr_compositor.h wlr_surface_unlock_cached"
    wlr_surface_unlock_cached :: Ptr WLR_surface -> Word32 -> Void

{-
 - Set the preferred buffer scale for the surface.
 -
 - This sends an event to the client indicating the preferred scale to use for
 - buffers attached to this surface.
 -}
foreign import capi "wlr/types/wlr_compositor.h wlr_surface_set_preferred_buffer_scale"
    wlr_surface_set_preferred_buffer_scale :: Ptr WLR_surface -> Int32 -> Void

{-
 - Set the preferred buffer transform for the surface.
 -
 - This sends an event to the client indicating the preferred transform to use
 - for buffers attached to this surface.
 -}
foreign import capi "wlr/types/wlr_compositor.h wlr_surface_set_preferred_buffer_transform"
    wlr_surface_set_preferred_buffer_transform ::
        Ptr WLR_surface ->
        WL_output_transform ->
        Void

{-
 - Create the wl_compositor global, which can be used by clients to create
 - surfaces and regions.
 -
 - If a renderer is supplied, the compositor will create struct wlr_texture
 - objects from client buffers on surface commit.
 -}
foreign import capi "wlr/types/wlr_compositor.h wlr_compositor_create"
    wlr_compositor_create ::
        Ptr WL_display ->
        Word32 ->
        Ptr WLR_renderer ->
        IO (Ptr WLR_compositor)
