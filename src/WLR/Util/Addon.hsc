module WLR.Util.Addon where

#define WLR_USE_UNSTABLE
#include <wlr/util/addon.h>

import Foreign
import Foreign.C.String (CString)

import WL.Utils

{{ struct
    wlr/util/wlr_addon.h,
    wlr_addon_set,
    addons, WL_list
}}

{{ struct wlr/util/wlr_addon.h,
    wlr_addon_interface,
	name, CString,
    destroy, FunPtr (Ptr WLR_addon -> IO ()),
}}

{- | beneath `impl` is labeled as being "// private state" in the wlroots source
 -}
{{ struct wlr/util/wlr_addon.h,
    wlr_addon,
	impl, Ptr WLR_addon_interface,
    owner, Ptr (),
    link, WL_list,
}}
