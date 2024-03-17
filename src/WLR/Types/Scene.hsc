{-# LANGUAGE PatternSynonyms, EmptyDataDeriving #-}
module WLR.Types.Scene where

#define WLR_USE_UNSTABLE
#include<wlr/types/wlr_scene.h>

import Foreign.Storable (Storable(..))
import Foreign (Word64, Word8)
import Foreign.C.Types (CBool(..), CInt(..), CFloat(..), CDouble(..))
import Foreign.Ptr (Ptr, FunPtr)

import PIXMAN.Pixman (PIXMAN_region32)

import WL.Utils (WL_list, WL_array)
import WL.ServerCore (WL_signal, WL_listener)
import WL.ServerProtocol (WL_output_transform)

import WLR.Util.Addon (WLR_addon_set)
import WLR.Types.PresentationTime (WLR_presentation)
import WLR.Types.LinuxDmabuf_v1 (WLR_linux_dmabuf_v1, WLR_linux_dmabuf_feedback_v1_init_options)
import WLR.Types.Compositor (WLR_surface)
import WLR.Types.Buffer (WLR_buffer)
import WLR.Types.DamageRing (WLR_damage_ring)
import WLR.Types.Output (WLR_output)
import WLR.Util.Box (WLR_box, WLR_fbox)
import WLR.Util.Addon (WLR_addon)
import WLR.Render.Texture (WLR_texture)
import WLR.Render.Pass (WLR_scale_filter_mode)

{{ enum WLR_scene_node_type,
    WLR_SCENE_NODE_TREE,
    WLR_SCENE_NODE_RECT,
    WLR_SCENE_NODE_BUFFER
}}

{- | A sub-tree in the scene-graph. -}
{{ struct wlr/types/wlr_scene.h,
    wlr_scene_tree,
    node, WLR_scene_node,
    children, WL_list
}}

{{ struct wlr/types/wlr_scene.h, wlr_scene_data }}

{- | A node is an object in the scene. -}
{{ struct wlr/types/wlr_scene.h,
    wlr_scene_node,
    type, WLR_scene_node_type,
    parent, Ptr WLR_scene_tree,
    link, WL_list,
    enabled, CBool,
    x, CInt,
    y, CInt,
    events destroy, WL_signal,
    data, Ptr WLR_scene_data,
    addons, WLR_addon_set,
    visible, PIXMAN_region32
}}

{{ enum WLR_scene_debug_damage_option,
    WLR_SCENE_DEBUG_DAMAGE_NONE,
    WLR_SCENE_DEBUG_DAMAGE_RERENDER,
    WLR_SCENE_DEBUG_DAMAGE_HIGHLIGHT
}}

{- The root scene-graph node.
 - fields beneath linux_dmabuf_v1 were marked as 'private state'
 -}
{{ struct wlr/types/wlr_scene.h,
    wlr_scene,
    tree, WLR_scene_tree,
    outputs, WL_list,
    presentation, Ptr WLR_presentation,
    linux_dmabuf_v1, Ptr WLR_linux_dmabuf_v1,
    presentation_destroy, WL_listener,
    linux_dmabuf_v1_destroy, WL_listener,
    debug_damage_option, WLR_scene_debug_damage_option,
    direct_scanout, CBool,
    calculate_visibility, CBool
}}

{- |A viewport for an output in the scene-graph
 - `link` - wlr_scene.outputs
 - everything beneath events.destroy was labeled as "// private state"
 -}
{{ struct wlr/types/scene.h,
    wlr_scene_output,
    output, Ptr WLR_output,
    link, WL_list,
    scene, Ptr WLR_scene,
    addon, WLR_addon,
    damage_ring, WLR_damage_ring,
    x, CInt,
    y, CInt,
    events destroy, WL_signal,
    index, Word8,
    prev_scanout, CBool,
    output_commit, WL_listener,
    output_damage, WL_listener,
    output_needs_frame, WL_listener,
    damage_highlight_regions, WL_list,
    render_list, WL_array,
}}


-- the wlroots source has a typedef for a function pointer that looks like this
-- I'm not sure how to declare this using the `foreign import capi` tool, so I 'inlined' the type in the struct field
--from wlroots:
--    typedef bool (*wlr_scene_buffer_point_accepts_input_func_t)(
--      struct wlr_scene_buffer *buffer, double *sx, double *sy);
--what the capi import could have looked like?
--foreign import capi "wlr/types/wlr_scene.h wlr_scene_buffer_point_accepts_input_func_t"
--    wlr_scene_buffer_point_accepts_input_func_t :: Ptr WLR_scene_buffer -> CDouble -> CDouble -> IO (CBool)
--
--I just wrote the type out in the (point_accepts_input) field of the (wlr_scene_buffer) struct

{- |`buffer` may be NULL
 - point_accepts_input may be NULL
 -}
{{ struct wlr/types/wlr_scene.h,
    wlr_scene_buffer,
    node, WLR_scene_node,
    buffer, Ptr WLR_buffer,
    events outputs_update, WL_signal,
    events output_enter, WL_signal,
    events output_leave, WL_signal,
    events output_sample, WL_signal,
    events frame_done, WL_signal,
    point_accepts_input, FunPtr (Ptr WLR_scene_buffer -> CDouble -> CDouble -> IO (CBool)),
    primary_output, Ptr WLR_scene_output,
    opacity, CFloat,
    filter_mode, WLR_scale_filter_mode,
    src_box, WLR_fbox,
    dst_width, CInt,
    dst_height, CInt,
    transform, WL_output_transform,
    opaque_region, PIXMAN_region32,
    active_outputs, Word64,
    texture, Ptr WLR_texture,
    prev_feedback_options, WLR_linux_dmabuf_feedback_v1_init_options,
}}

{- | A scene-graph node displaying a single surface.
 - everything below 'surface' was labeled "// private state" in the wlroots source
 -}
{{ struct wlr/types/wlr_scene.h,
    wlr_scene_surface,
    buffer, Ptr WLR_scene_buffer,
    surface, Ptr WLR_surface,
    clip, WLR_box,
    addon, WLR_addon,
    outputs_update, WL_listener,
    output_enter, WL_listener,
    output_leave, WL_listener,
    output_sample, WL_listener,
    frame_done, WL_listener,
    surface_destroy, WL_listener,
    surface_commit, WL_listener
}}
