{-# LANGUAGE PatternSynonyms #-}
module WLR.Types.OutputLayout where

import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import Foreign.C.Types (CInt(..), CBool(..), CDouble(..))
import WL.Utils (WL_list)
import WL.ServerCore (WL_signal, WL_listener)
import WLR.Util.Addon (WLR_addon)
import WLR.Types.Output (WLR_output)
import WLR.Util.Box (WLR_box)

#define WLR_USE_UNSTABLE
#include<wlr/types/wlr_output_layout.h>

{-
 - Helper to arrange outputs in a 2D coordinate space. The output effective
 - resolution is used, see wlr_output_effective_resolution().
 -
 - Outputs added to the output layout are automatically exposed to clients (see
 - wlr_output_create_global()). They are no longer exposed when removed from the
 - layout.
 -}
{{ struct wlr/types/wlr_output_layout.h,
    wlr_output_layout,
    outputs, WL_list,

    events add, WL_signal,
    events change, WL_signal,
    events destroy, WL_signal,

    data, Ptr ()
}}

{{ struct wlr/types/wlr_output_layout.h,
    wlr_output_layout_output,
    layout, Ptr WLR_output_layout,
    output, Ptr WLR_output,
    x, CInt,
    y, CInt,
    link, WL_list,
    auto_configured, CBool,
    events destroy, WL_signal,
    addon, WLR_addon,
    commit, WL_listener
}}

foreign import capi "wlr/types/wlr_output_layout.h wlr_output_layout_destroy"
    wlr_output_layout_destroy :: Ptr WLR_output_layout -> IO ()

{- |
 - Get the output layout for the specified output. Returns NULL if no output
 - matches.
 -}
foreign import capi "wlr/types/wlr_output_layout.h wlr_output_layout_get"
    wlr_output_layout_get :: Ptr WLR_output_layout -> Ptr WLR_output -> IO (Ptr WLR_output_layout_output)

{- |
 - Get the output at the specified layout coordinates. Returns NULL if no
 - output matches the coordinates.
 -}
foreign import capi "wlr/types/wlr_output_layout.h wlr_output_layout_output_at"
    wlr_output_layout_output_at :: Ptr WLR_output_layout -> CDouble -> CDouble -> IO (Ptr WLR_output)

{- |
 - Add the output to the layout at the specified coordinates. If the output is
 - already a part of the output layout, it will become manually configured and
 - will be moved to the specified coordinates.
 -
 - Returns true on success, false on a memory allocation error.
 -}
foreign import capi "wlr/types/wlr_output_layout.h wlr_output_layout_add"
    wlr_output_layout_add ::
        Ptr WLR_output_layout
        -> Ptr WLR_output
        -> CInt
        -> CInt
        -> IO (Ptr WLR_output_layout_output)

{- |
 - Add the output to the layout as automatically configured. This will place
 - the output in a sensible location in the layout. The coordinates of
 - the output in the layout will be adjusted dynamically when the layout
 - changes. If the output is already a part of the layout, it will become
 - automatically configured.
 -
 - Returns true on success, false on a memory allocation error.
 -}
foreign import capi "wlr/types/wlr_output_layout.h wlr_output_layout_add_auto"
    wlr_output_layout_add_auto ::
        Ptr WLR_output_layout
        -> Ptr WLR_output
        -> IO (Ptr WLR_output_layout_output)

{- |
 - Remove the output from the layout. If the output is already not a part of
 - the layout, this function is a no-op.
 -}
foreign import capi "wlr/types/wlr_output_layout.h wlr_output_layout_remove"
    wlr_output_layout_remove ::
        Ptr WLR_output_layout
        -> Ptr WLR_output
        -> IO ()

{- |
 - Given x and y in layout coordinates, adjusts them to local output
 - coordinates relative to the given reference output.
 -}
foreign import capi "wlr/types/wlr_output_layout.h wlr_output_layout_output_coords"
    wlr_output_layout_output_coords ::
        Ptr WLR_output_layout
        -> Ptr WLR_output
        -> Ptr CDouble
        -> Ptr CDouble
        -> IO ()


foreign import capi "wlr/types/wlr_output_layout.h wlr_output_layout_contains_point"
    wlr_output_layout_contains_point ::
        Ptr WLR_output_layout
        -> Ptr WLR_output
        -> CInt
        -> CInt
        -> IO CBool

foreign import capi "wlr/types/wlr_output_layout.h wlr_output_layout_intersects"
    wlr_output_layout_intersects ::
        Ptr WLR_output_layout
        -> Ptr WLR_output
        -> Ptr WLR_box
        -> IO CBool
    

{- |
 - Get the closest point on this layout from the given point from the reference
 - output. If reference is NULL, gets the closest point from the entire layout.
 - If the layout is empty, the result is the given point itself.
 -}
foreign import capi "wlr/types/wlr_output_layout.h wlr_output_layout_closest_point"
    wlr_output_layout_closest_point ::
        Ptr WLR_output_layout
        -> Ptr WLR_output
        -> CDouble
        -> CDouble
        -> Ptr CDouble
        -> Ptr CDouble
        -> IO ()

{- |
 - Get the box of the layout for the given reference output in layout
 - coordinates. If `reference` is NULL, the box will be for the extents of the
 - entire layout. If the output isn't in the layout, the box will be empty.
 -}
foreign import capi "wlr/types/wlr_output_layout.h wlr_output_layout_get_box"
    wlr_output_layout_get_box ::
        Ptr WLR_output_layout
        -> Ptr WLR_output
        -> Ptr WLR_box
        -> IO ()

{- |
 - Get the output closest to the center of the layout extents.
 -}
foreign import capi "wlr/types/wlr_output_layout.h wlr_output_layout_get_center_output"
    wlr_output_layout_get_center_output ::
        Ptr WLR_output_layout
        -> IO (Ptr WLR_output)

type WLR_direction = CInt

--	WLR_DIRECTION_UP = 1 << 0,
pattern WLR_DIRECTION_UP :: (Eq a, Num a) => a
pattern WLR_DIRECTION_UP = #const WLR_DIRECTION_UP

--	WLR_DIRECTION_DOWN = 1 << 1,
pattern WLR_DIRECTION_DOWN :: (Eq a, Num a) => a
pattern WLR_DIRECTION_DOWN = #const WLR_DIRECTION_DOWN

--	WLR_DIRECTION_LEFT = 1 << 2,
pattern WLR_DIRECTION_LEFT :: (Eq a, Num a) => a
pattern WLR_DIRECTION_LEFT = #const WLR_DIRECTION_LEFT

--	WLR_DIRECTION_RIGHT = 1 << 3,
pattern WLR_DIRECTION_RIGHT :: (Eq a, Num a) => a
pattern WLR_DIRECTION_RIGHT = #const WLR_DIRECTION_RIGHT

{- |
 - Get the closest adjacent output to the reference output from the reference
 - point in the given direction.
 -}
foreign import capi "wlr/types/wlr_output_layout.h wlr_output_layout_adjacent_output"
    wlr_output_layout_adjacent_output ::
        Ptr WLR_output_layout
        -> WLR_direction
        -> Ptr WLR_output
        -> CDouble
        -> CDouble
        -> IO (Ptr WLR_output)

foreign import capi "wlr/types/wlr_output_layout.h wlr_output_layout_farthest_output"
    wlr_output_layout_farthest_output ::
        Ptr WLR_output_layout
        -> WLR_direction
        -> Ptr WLR_output
        -> CDouble
        -> CDouble
        -> IO (Ptr WLR_output)
