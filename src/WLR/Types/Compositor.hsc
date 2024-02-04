{-# LANGUAGE PatternSynonyms #-}
module WLR.Types.Compositor where

import Foreign.C.Types (CBool, CInt, CSize, CUInt)
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr)

import WL.Utils (WL_list)
import WL.ServerCore (WL_resource, WL_signal, WL_listener)

import WLR.Types.Buffer (WLR_client_buffer, WLR_buffer)
import WLR.Render.Renderer (WLR_renderer)

import Pixman.Types.Region (PIXMAN_region_32)

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_compositor.h>

-- not exactly sure what int32_t should map to, but I think it's just CInt
-- some of the fields in wlr_surface had this `int32_t` type

--WLR_surface
-- the 'resource' field had a source comment, "//may be NULL"
{{ struct
    wlr/types/wlr_compositor.h,
    wlr_surface,
    resource, Ptr WL_resource,
    renderer, Maybe (Ptr WLR_renderer),
    buffer, WLR_client_buffer,
    buffer_damage,  PIXMAN_region_32,
    external_damage, PIXMAN_region_32,
    opaque_region, PIXMAN_region_32,
    input_region, PIXMAN_region_32,
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
    previous scale, CInt,
    previous transform, WL_output_transform,
    previous width, CInt,
    previous height, CInt,
    previous buffer_width, CInt,
    previous buffer_height, CInt,
    unmap_commit, CBool,
    opaque, CBool,
    has_buffer, CBool,
    preferred_buffer_scale, CInt,
    preferred_buffer_transform_sent, CBool,
    preferred_buffer_transform, WL_output_transform
}}

{{ struct
    wlr/types/wlr_compositor.h,
    wlr_surface_state,
    committed, CUInt,
    seq, CUInt,
    buffer, Ptr WLR_buffer,
    dx, CUInt,
    dy, CUInt,
    surface_damage, PIXMAN_region_32,
    buffer_damage, PIXMAN_region_32,
    opaque, PIXMAN_region_32,
    input, PIXMAN_region_32,
    transform, WL_output_transform,
    scale, CInt,
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
    cached_state_link, WL_list,
}}
