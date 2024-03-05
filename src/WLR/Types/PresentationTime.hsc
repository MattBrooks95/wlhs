module WLR.Types.PresentationTime where
import WL.ServerCore (WL_signal, WL_listener)
import WL.Global (WL_global)

import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr)

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_presentation_time.h>

{{ struct wlr/types/wlr_presentation_time.h,
    wlr_presentation,
    global, Ptr WL_global,
    events destroy, WL_signal,
    display_destroy, WL_listener
}}
