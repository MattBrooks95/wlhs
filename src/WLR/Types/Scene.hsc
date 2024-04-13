{-# LANGUAGE PatternSynonyms, EmptyDataDeriving #-}
module WLR.Types.Scene where

#define WLR_USE_UNSTABLE
#include<wlr/types/wlr_scene.h>

import Foreign.Storable (Storable(..))
import Foreign (Word64, Word8, Int64, peekArray, pokeArray, plusPtr)
import Foreign.C.Types (CBool(..), CInt(..), CFloat(..), CDouble(..), CSize)
import Foreign.Ptr (Ptr, FunPtr)

import PIXMAN.Pixman (PIXMAN_region32)

import WL.Utils (WL_list, WL_array)
import WL.ServerCore (WL_signal, WL_listener)
import WL.ServerProtocol (WL_output_transform)

import WLR.Util.Addon (WLR_addon_set)
import WLR.Types.DataDevice (WLR_drag_icon)
import WLR.Types.PresentationTime (WLR_presentation)
import WLR.Types.LinuxDmabuf_v1 (WLR_linux_dmabuf_v1, WLR_linux_dmabuf_feedback_v1_init_options)
import WLR.Types.Compositor (WLR_surface)
import WLR.Types.Buffer (WLR_buffer)
import WLR.Types.DamageRing (WLR_damage_ring)
import WLR.Types.Output (WLR_output, WLR_output_state)
import WLR.Types.OutputLayout (WLR_output_layout)
-- import WLR.Types.LayerShellV1 (WLR_layer_surface_v1)
import WLR.Types.XdgShell (WLR_xdg_surface)
import WLR.Util.Box (WLR_box, WLR_fbox)
import WLR.Util.Addon (WLR_addon)
import WLR.Render.Texture (WLR_texture)
import WLR.Render.Pass (WLR_scale_filter_mode)
import WLR.Render.Interface (WLR_render_timer)

import Time.Time (TIMESPEC)

-- definitions that only exist in .c files
-- exist as forward struct definitions in the original wlr_scene.h file
data WLR_scene_output_layout
data WLR_scene_output_layout_output

-- including WLR.Types.LayerShellV1 causes the build to fail because it can't find
-- "No such file or directory #include "wlr-layer-shell-unstable-v1-protocol.h"
-- wlr_scene.h uses a forward struct definition, so maybe I should too?
data WLR_layer_surface_v1

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
{{ struct wlr/types/wlr_scene.h,
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

{- |
 - A scene-graph node displaying a solid-colored rectangle
 -}
{{ struct wlr/types/wlr_scene.h,
    wlr_scene_rect,
    node, WLR_scene_node,
    width, CInt,
    height, CInt,
    color, [4]CFloat
}}

{{ struct wlr/types/wlr_scene.h,
    wlr_scene_outputs_update_event,
    active, Ptr (Ptr WLR_scene_output),
    size, CSize,
}}

{{ struct wlr/types/wlr_scene.h,
    wlr_scene_output_sample_event,
    output, Ptr WLR_scene_output,
    direct_scanout, CBool
}}

{{ struct wlr/types/wlr_scene.h,
    wlr_scene_timer,
    pre_render_duration, Int64,
    render_timer, Ptr WLR_render_timer
}}

{- |A layer shell scene helper
 - beneath layer_surface is 'private state'
 -}
{{ struct wlr/types/wlr_scene.h,
    wlr_scene_layer_surface_v1,
    tree, Ptr WLR_scene_tree,
    layer_surface, Ptr WLR_layer_surface_v1,
    tree_destroy, WL_listener,
    layer_surface_destroy, WL_listener,
    layer_surface_map, WL_listener,
    layer_surface_unmap, WL_listener,
}}

{-
 - Immediately destroy the scene-graph node.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_node_destroy"
    wlr_scene_node_destroy :: Ptr WLR_scene_node -> IO ()

{-
 - Enable or disable this node. If a node is disabled, all of its children are
 - implicitly disabled as well.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_node_set_enabled"
    wlr_scene_node_set_enabled :: Ptr WLR_scene_node -> CBool -> IO ()

{-
 - Set the position of the node relative to its parent.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_node_set_position"
    wlr_scene_node_set_position :: Ptr WLR_scene_node -> CInt -> CInt -> IO()
{-
 - Move the node right above the specified sibling.
 - Asserts that node and sibling are distinct and share the same parent.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_node_place_above"
    wlr_scene_node_place_above :: Ptr WLR_scene_node -> Ptr WLR_scene_node -> IO ()

{-
 - Move the node right below the specified sibling.
 - Asserts that node and sibling are distinct and share the same parent.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_node_place_below"
    wlr_scene_node_place_below :: Ptr WLR_scene_node -> Ptr WLR_scene_node -> IO ()
{-
 - Move the node above all of its sibling nodes.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_node_raise_to_top"
    wlr_scene_node_raise_to_top :: Ptr WLR_scene_node -> IO ()

{-
 - Move the node below all of its sibling nodes.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_node_lower_to_bottom"
    wlr_scene_node_lower_to_bottom :: Ptr WLR_scene_node -> IO ()

{-
 - Move the node to another location in the tree.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_node_reparent"
    wlr_scene_node_reparent :: Ptr WLR_scene_node -> Ptr WLR_scene_tree -> IO ()
{-
 - Get the node's layout-local coordinates.
 -
 - True is returned if the node and all of its ancestors are enabled.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_node_coords"
    wlr_scene_node_coords :: Ptr WLR_scene_node -> Ptr CInt -> Ptr CInt -> IO CBool

type WLR_scene_buffer_iterator_func = Ptr WLR_scene_buffer -> CInt -> CInt -> Ptr () -> IO ()
{-
 - Call `iterator` on each buffer in the scene-graph, with the buffer's
 - position in layout coordinates. The function is called from root to leaves
 - (in rendering order).
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_node_for_each_buffer"
    wlr_scene_node_for_each_buffer :: Ptr WLR_scene_node -> FunPtr (WLR_scene_buffer_iterator_func) -> Ptr () -> IO ()

{-
 - Find the topmost node in this scene-graph that contains the point at the
 - given layout-local coordinates. (For surface nodes, this means accepting
 - input events at that point.) Returns the node and coordinates relative to the
 - returned node, or NULL if no node is found at that location.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_node_at"
    wlr_scene_node_at :: Ptr WLR_scene_node -> CDouble -> CDouble -> Ptr CDouble -> Ptr CDouble -> IO (Ptr WLR_scene_node)

{-
 - Create a new scene-graph.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_create"
    wlr_scene_create :: IO (Ptr WLR_scene)

{-
 - Handle presentation feedback for all surfaces in the scene, assuming that
 - scene outputs and the scene rendering functions are used.
 -
 - Asserts that a struct wlr_presentation hasn't already been set for the scene.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_set_presentation"
    wlr_scene_set_presentation :: Ptr WLR_scene -> Ptr WLR_presentation -> IO ()

{-
 - Handles linux_dmabuf_v1 feedback for all surfaces in the scene.
 -
 - Asserts that a struct wlr_linux_dmabuf_v1 hasn't already been set for the scene.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_set_linux_dmabuf_v1"
    wlr_scene_set_linux_dmabuf_v1 :: Ptr WLR_scene -> Ptr WLR_linux_dmabuf_v1 -> IO ()

{-
 - Add a node displaying nothing but its children.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_tree_create"
    wlr_scene_tree_create :: Ptr WLR_scene_tree -> IO (Ptr WLR_scene_tree)

{-
 - Add a node displaying a single surface to the scene-graph.
 -
 - The child sub-surfaces are ignored.
 -
 - wlr_surface_send_enter() and wlr_surface_send_leave() will be called
 - automatically based on the position of the surface and outputs in
 - the scene.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_surface_create"
    wlr_scene_surface_create :: Ptr WLR_scene_tree -> Ptr WLR_surface -> IO (Ptr WLR_scene_surface)

{-
 - If this node represents a wlr_scene_buffer, that buffer will be returned. It
 - is not legal to feed a node that does not represent a wlr_scene_buffer.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_buffer_from_node"
    wlr_scene_buffer_from_node :: Ptr WLR_scene_node -> IO (Ptr WLR_scene_buffer)

{-
 - If this node represents a wlr_scene_tree, that tree will be returned. It
 - is not legal to feed a node that does not represent a wlr_scene_tree.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_tree_from_node"
    wlr_scene_tree_from_node ::Ptr WLR_scene_node -> IO (Ptr WLR_scene_tree)

{-
 - If this node represents a wlr_scene_rect, that rect will be returned. It
 - is not legal to feed a node that does not represent a wlr_scene_rect.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_rect_from_node"
    wlr_scene_rect_from_node :: Ptr WLR_scene_node -> IO (Ptr WLR_scene_rect)

{-
 - If this buffer is backed by a surface, then the struct wlr_scene_surface is
 - returned. If not, NULL will be returned.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_surface_try_from_buffer"
    wlr_scene_surface_try_from_buffer :: Ptr WLR_scene_buffer -> IO (Ptr WLR_scene_surface)

{-
 - Add a node displaying a solid-colored rectangle to the scene-graph.
 -
 - The actual type of the last argument here was `const float color[static 4]`, but I think
 - in C arrays are passed and returned as pointers TODO see if this is correct, quick internet search was incunclusive
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_rect_create"
    wlr_scene_rect_create :: Ptr WLR_scene_tree -> CInt -> CInt -> Ptr [CFloat] -> IO (Ptr WLR_scene_rect)

{-
 - Change the width and height of an existing rectangle node.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_rect_set_size"
    wlr_scene_rect_set_size :: Ptr WLR_scene_rect -> CInt -> CInt -> IO ()

{-
 - Change the color of an existing rectangle node.
 -
 - TODO check the type for a `const float color[static 4]` parameter type
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_rect_set_color"
    wlr_scene_rect_set_color :: Ptr WLR_scene_rect -> Ptr ([CFloat]) -> IO ()

{-
 - Add a node displaying a buffer to the scene-graph.
 -
 - If the buffer is NULL, this node will not be displayed.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_buffer_create"
    wlr_scene_buffer_create :: Ptr WLR_scene_tree -> Ptr WLR_buffer -> IO (Ptr WLR_scene_buffer)

{-
 - Sets the buffer's backing buffer.
 -
 - If the buffer is NULL, the buffer node will not be displayed.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_buffer_set_buffer"
    wlr_scene_buffer_set_buffer :: Ptr WLR_scene_buffer -> Ptr WLR_buffer -> IO ()

{-
 - Sets the buffer's backing buffer with a custom damage region.
 -
 - The damage region is in buffer-local coordinates. If the region is NULL,
 - the whole buffer node will be damaged.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_buffer_set_buffer_with_damage"
    wlr_scene_buffer_set_buffer_with_damage :: Ptr WLR_scene_buffer -> Ptr WLR_buffer -> Ptr PIXMAN_region32 -> IO ()

{-
 - Sets the buffer's opaque region. This is an optimization hint used to
 - determine if buffers which reside under this one need to be rendered or not.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_buffer_set_opaque_region"
    wlr_scene_buffer_set_opaque_region :: Ptr WLR_scene_buffer -> Ptr PIXMAN_region32 -> IO ()

{-
 - Set the source rectangle describing the region of the buffer which will be
 - sampled to render this node. This allows cropping the buffer.
 -
 - If NULL, the whole buffer is sampled. By default, the source box is NULL.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_buffer_set_source_box"
    wlr_scene_buffer_set_source_box :: Ptr WLR_scene_buffer -> Ptr WLR_fbox -> IO ()

{-
 - Set the destination size describing the region of the scene-graph the buffer
 - will be painted onto. This allows scaling the buffer.
 -
 - If zero, the destination size will be the buffer size. By default, the
 - destination size is zero.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_buffer_set_dest_size"
    wlr_scene_buffer_set_dest_size :: Ptr WLR_scene_buffer -> CInt -> CInt -> IO ()

{-
 - Set a transform which will be applied to the buffer.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_buffer_set_transform"
    wlr_scene_buffer_set_transform :: Ptr WLR_scene_buffer -> WL_output_transform -> IO ()

{-
 - Sets the opacity of this buffer
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_buffer_set_opacity"
    wlr_scene_buffer_set_opacity :: Ptr WLR_scene_buffer -> CFloat -> IO ()

{-
 - Sets the filter mode to use when scaling the buffer
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_buffer_set_filter_mode"
    wlr_scene_buffer_set_filter_mode :: Ptr WLR_scene_buffer -> WLR_scale_filter_mode -> IO ()

{-
 - Calls the buffer's frame_done signal.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_buffer_send_frame_done"
    wlr_scene_buffer_send_frame_done :: Ptr WLR_scene_buffer -> Ptr TIMESPEC -> IO ()

{-
 - Add a viewport for the specified output to the scene-graph.
 -
 - An output can only be added once to the scene-graph.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_output_create"
    wlr_scene_output_create :: Ptr WLR_scene -> Ptr WLR_output -> IO (Ptr WLR_scene_output)

{-
 - Destroy a scene-graph output.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_output_destroy"
    wlr_scene_output_destroy :: Ptr WLR_scene_output -> IO ()

{-
 - Set the output's position in the scene-graph.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_output_set_position"
    wlr_scene_output_set_position :: Ptr WLR_scene_output -> CInt -> CInt -> IO ()


{{ struct wlr/types/wlr_scene.h,
    wlr_scene_output_state_options,
    timer, Ptr WLR_scene_timer
}}

{-
 - Render and commit an output.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_output_commit"
    wlr_scene_output_commit :: Ptr WLR_scene_output -> Ptr WLR_scene_output_state_options -> IO CBool

{-
 - Render and populate given output state.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_output_build_state"
    wlr_scene_output_build_state :: Ptr WLR_scene_output -> Ptr WLR_output_state -> Ptr WLR_scene_output_state_options -> IO CBool

{-
 - Retrieve the duration in nanoseconds between the last wlr_scene_output_commit() call and the end
 - of its operations, including those on the GPU that may have finished after the call returned.
 -
 - Returns -1 if the duration is unavailable.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_timer_get_duration_ns"
    wlr_scene_timer_get_duration_ns :: Ptr WLR_scene_timer -> IO Int64
foreign import capi "wlr/types/wlr_scene.h wlr_scene_timer_finish"
    wlr_scene_timer_finish :: Ptr WLR_scene_timer -> IO ()

{-
 - Call wlr_surface_send_frame_done() on all surfaces in the scene rendered by
 - wlr_scene_output_commit() for which wlr_scene_surface.primary_output
 - matches the given scene_output.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_output_send_frame_done"
    wlr_scene_output_send_frame_done :: Ptr WLR_scene_output -> Ptr TIMESPEC -> IO ()

{-
 - Call `iterator` on each buffer in the scene-graph visible on the output,
 - with the buffer's position in layout coordinates. The function is called
 - from root to leaves (in rendering order).
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_output_for_each_buffer"
    wlr_scene_output_for_each_buffer :: Ptr WLR_scene_output -> FunPtr WLR_scene_buffer_iterator_func -> Ptr () -> IO ()

{-
 - Get a scene-graph output from a struct wlr_output.
 -
 - If the output hasn't been added to the scene-graph, returns NULL.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_get_scene_output"
    wlr_scene_get_scene_output :: Ptr WLR_scene -> Ptr WLR_output -> IO (Ptr WLR_scene_output)

{-
 - Attach an output layout to a scene.
 -
 - The resulting scene output layout allows to synchronize the positions of scene
 - outputs with the positions of corresponding layout outputs.
 -
 - It is automatically destroyed when the scene or the output layout is destroyed.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_attach_output_layout"
    wlr_scene_attach_output_layout :: Ptr WLR_scene -> Ptr WLR_output_layout -> IO (Ptr WLR_scene_output_layout)

{-
 - Add an output to the scene output layout.
 -
 - When the layout output is repositioned, the scene output will be repositioned
 - accordingly.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_output_layout_add_output"
    wlr_scene_output_layout_add_output :: Ptr WLR_scene_output_layout -> Ptr WLR_scene_output_layout_output -> Ptr WLR_scene_output -> IO ()

{-
 - Add a node displaying a surface and all of its sub-surfaces to the
 - scene-graph.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_subsurface_tree_create"
    wlr_scene_subsurface_tree_create :: Ptr WLR_scene_tree -> Ptr WLR_surface -> IO (Ptr WLR_scene_tree)

{-
 - Sets a cropping region for any subsurface trees that are children of this
 - scene node. The clip coordinate space will be that of the root surface of
 - the subsurface tree.
 -
 - A NULL or empty clip will disable clipping
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_subsurface_tree_set_clip"
    wlr_scene_subsurface_tree_set_clip :: Ptr WLR_scene_node -> Ptr WLR_box -> IO ()

{-
 - Add a node displaying an xdg_surface and all of its sub-surfaces to the
 - scene-graph.
 -
 - The origin of the returned scene-graph node will match the top-left corner
 - of the xdg_surface window geometry.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_xdg_surface_create"
    wlr_scene_xdg_surface_create :: Ptr WLR_scene_tree -> Ptr WLR_xdg_surface -> IO (Ptr WLR_scene_tree)

{-
 - Add a node displaying a layer_surface_v1 and all of its sub-surfaces to the
 - scene-graph.
 -
 - The origin of the returned scene-graph node will match the top-left corner
 - of the layer surface.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_layer_surface_v1_create"
    wlr_scene_layer_surface_v1_create :: Ptr WLR_scene_tree -> Ptr WLR_layer_surface_v1 -> IO (Ptr WLR_scene_layer_surface_v1)

{-
 - Configure a layer_surface_v1, position its scene node in accordance to its
 - current state, and update the remaining usable area.
 -
 - full_area represents the entire area that may be used by the layer surface
 - if its exclusive_zone is -1, and is usually the output dimensions.
 - usable_area represents what remains of full_area that can be used if
 - exclusive_zone is >= 0. usable_area is updated if the surface has a positive
 - exclusive_zone, so that it can be used for the next layer surface.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_layer_surface_v1_configure"
    wlr_scene_layer_surface_v1_configure :: Ptr WLR_scene_layer_surface_v1 -> Ptr WLR_box -> Ptr WLR_box -> IO ()

{-
 - Add a node displaying a drag icon and all its sub-surfaces to the
 - scene-graph.
 -}
foreign import capi "wlr/types/wlr_scene.h wlr_scene_drag_icon_create"
    wlr_scene_drag_icon_create :: Ptr WLR_scene_tree -> Ptr WLR_drag_icon -> IO (Ptr WLR_scene_tree)
