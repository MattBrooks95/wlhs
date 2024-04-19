{-# LANGUAGE PatternSynonyms #-}
module WLR.Types.LayerShellV1 where

-- commenting this out because it breaks the repl
-- I need to figure out how to tell Cabal about the local C header files

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_layer_shell_v1.h>

import Foreign (Word32, Int32)
import Foreign.Storable (Storable(..))
import Foreign.C.String (CString)
import Foreign.C.Types (CBool(..), CInt)
import Foreign.Ptr (Ptr)

import WL.Utils (WL_list)
import WL.ServerCore (WL_signal, WL_resource, WL_listener)
import WL.Global (WL_global)
import WLR.Types.Output (WLR_output)
import WLR.Types.Compositor (WLR_surface)


{-
 - wlr_layer_shell_v1 allows clients to arrange themselves in "layers" on the
 - desktop in accordance with the wlr-layer-shell protocol. When a client is
 - added, the new_surface signal will be raised and passed a reference to our
 - struct wlr_layer_surface_v1. At this time, the client will have configured the
 - surface as it desires, including information like desired anchors and
 - margins. The compositor should use this information to decide how to arrange
 - the layer on-screen, then determine the dimensions of the layer and call
 - wlr_layer_surface_v1_configure(). The client will then attach a buffer and
 - commit the surface, at which point the wlr_layer_surface_v1 map signal is
 - raised and the compositor should begin rendering the surface.
 -}
{{ struct wlr/types/wlr_layer_shell_v1.h,
    wlr_layer_shell_v1,
	global, Ptr WL_global,
    display_destroy, WL_listener,
    events new_surface, WL_signal,
    events destroy, WL_signal,
    data, Ptr ()
}}

{-
 - @ingroup iface_zwlr_layer_surface_v1
 - types of keyboard interaction possible for a layer shell surface
 -
 - Types of keyboard interaction possible for layer shell surfaces. The
 - rationale for this is twofold: (1) some applications are not interested
 - in keyboard events and not allowing them to be focused can improve the
 - desktop experience; (2) some applications will want to take exclusive
 - keyboard focus.
 -}
{{ enum
    ZWLR_layer_surface_v1_keyboard_interactivity,
    ZWLR_LAYER_SURFACE_V1_KEYBOARD_INTERACTIVITY_NONE,
    ZWLR_LAYER_SURFACE_V1_KEYBOARD_INTERACTIVITY_EXCLUSIVE,
    ZWLR_LAYER_SURFACE_V1_KEYBOARD_INTERACTIVITY_ON_DEMAND
}}

{-
 - @ingroup iface_zwlr_layer_shell_v1
 - available layers for surfaces
 -
 - These values indicate which layers a surface can be rendered in. They
 - are ordered by z depth, bottom-most first. Traditional shell surfaces
 - will typically be rendered between the bottom and top layers.
 - Fullscreen shell surfaces are typically rendered at the top layer.
 - Multiple surfaces can share a single layer, and ordering within a
 - single layer is undefined.
 -}
{{ enum
    ZWLR_layer_shell_v1_layer,
    ZWLR_LAYER_SHELL_V1_LAYER_BACKGROUND,
    ZWLR_LAYER_SHELL_V1_LAYER_BOTTOM,
    ZWLR_LAYER_SHELL_V1_LAYER_TOP,
    ZWLR_LAYER_SHELL_V1_LAYER_OVERLAY
}}

{{ struct wlr/types/wlr_layer_shell_v1.h,
    wlr_layer_surface_v1_state,
    committed, Word32,
    anchor, Word32,
    exclusive_zone, Int32,
    margin top, Int32,
    margin right, Int32,
    margin bottom, Int32,
    margin left, Int32,
    keyboard_interactive, ZWLR_layer_surface_v1_keyboard_interactivity,
    desired_width, Word32,
    desired_height, Word32,
    layer, ZWLR_layer_shell_v1_layer,
    configure_serial, Word32,
    actual_width, Int32,
    actual_height, Int32
}}

{{ struct wlr/types/wlr_layer_shell_v1.h,
    wlr_layer_surface_v1,
    surface, Ptr WLR_surface,
    output, Ptr WLR_output,
    resource, Ptr WL_resource,
    shell, Ptr WLR_layer_shell_v1,
    popups, WL_list,
    namespace, CString,
    added, CBool,
    configured, CBool,
    configure_list, WL_list,
    current, WLR_layer_surface_v1_state,
    pending, WLR_layer_surface_v1_state,
    initialized, CBool,
    initial_commit, CBool,
    events destroy, WL_signal,
    events new_popup, WL_signal,
    data, Ptr ()
}}
