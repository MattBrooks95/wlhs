module WLR.Types.Subcompositor where

import Foreign (Word32, Int32, Ptr)
import Foreign.C.Types (CBool)
import Foreign.Storable (Storable(..))

import WLR.Types.Compositor (WLR_surface)
import WL.ServerCore (WL_resource, WL_signal, WL_listener, WL_global)
import WL.ServerProtocol (WL_display)
import WL.Utils (WL_list)

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_subcompositor.h>

{{ struct
    wlr/types/wlr_subcompositor.h,
    wlr_subsurface_parent_state,
    x, Int32,
    y, Int32,
    link, WL_list
}}

{{ struct
    wlr/types/wlr_subcompositor.h,
    wlr_subsurface,
    resource, Ptr WL_resource,
    surface, Ptr WLR_surface,
    parent, Ptr WLR_surface,

    current, WLR_subsurface_parent_state,
    pending, WLR_subsurface_parent_state,

    cached_seq, Word32,
    has_cache, CBool,

    synchronized, CBool,
    reordered, CBool,
    added, CBool,

    surface_client_commit, WL_listener,
    parent_destroy, WL_listener,

    events destroy, WL_signal,

    data, Ptr ()
}}

{{ struct
    wlr/types/wlr_subcompositor.h,
    wlr_subcompositor,
    global, Ptr WL_global,

    display_destroy, WL_listener,

    events destroy, WL_signal
}}

{-
 - Get a struct wlr_subsurface from a struct wlr_surface.
 -
 - Returns NULL if the surface doesn't have the subsurface role or if
 - the subsurface has been destroyed.
 -}
foreign import capi "wlr/types/wlr_subcompositor.h wlr_subsurface_try_from_wlr_surface"
    wlr_subsurface_try_from_wlr_surface :: Ptr WLR_surface -> IO (Ptr WLR_subsurface)


foreign import capi "wlr/types/wlr_subcompositor.h wlr_subcompositor_create"
    wlr_subcompositor_create :: Ptr WL_display -> IO (Ptr WLR_subcompositor)
