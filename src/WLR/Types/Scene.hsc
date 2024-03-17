{-# LANGUAGE PatternSynonyms, EmptyDataDeriving #-}
module WLR.Types.Scene where

#define WLR_USE_UNSTABLE
#include<wlr/types/wlr_scene.h>

import Foreign.Storable (Storable(..))
import Foreign.C.Types (CBool(..), CInt(..))
import Foreign.Ptr (Ptr)
import PIXMAN.Pixman (PIXMAN_region32)
import WL.Utils (WL_list)
import WL.ServerCore (WL_signal, WL_listener)
import WLR.Util.Addon (WLR_addon_set)
import WLR.Types.PresentationTime (WLR_presentation)
import WLR.Types.LinuxDmabuf_v1 (WLR_linux_dmabuf_v1)

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
