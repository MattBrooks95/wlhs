module WLR.Types.OutputLayout where

import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import WL.Utils (WL_list)
import WL.ServerCore (WL_signal)

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
