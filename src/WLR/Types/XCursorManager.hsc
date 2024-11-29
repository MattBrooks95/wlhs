module WLR.Types.XCursorManager where

import Foreign (Storable(..), Word32)
import Foreign.C.Types (CFloat(..), CBool(..))
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr)

import WL.Utils (WL_list)

import WLR.XCursor (WLR_xcursor_theme, WLR_xcursor)

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_xcursor_manager.h>

{-
 - An XCursor theme at a particular scale factor of the base size.
 -}
{{ struct wlr/types/wlr_xcursor_manager.h,
    wlr_xcursor_manager_theme,
    scale, CFloat,
    theme, Ptr WLR_xcursor_theme,
    link, WL_list
}}

{-
 - struct wlr_xcursor_manager dynamically loads xcursor themes at sizes necessary
 - for use on outputs at arbitrary scale factors. You should call
 - wlr_xcursor_manager_load() for each output you will show your cursor on, with
 - the scale factor parameter set to that output's scale factor.
 -}
{{ struct wlr/types/wlr_xcursor_manager.h,
    wlr_xcursor_manager,
    name, CString,
    size, Word32,
    scaled_themes, WL_list
}}

{-
 - Creates a new XCursor manager with the given xcursor theme name and base size
 - (for use when scale=1).
 -}
foreign import capi "wlr/types/wlr_xcursor_manager.h wlr_xcursor_manager_create"
    wlr_xcursor_manager_create :: CString -> Word32 -> IO (Ptr WLR_xcursor_manager)

foreign import capi "wlr/types/wlr_xcursor_manager.h wlr_xcursor_manager_destroy"
    wlr_xcursor_manager_destroy :: Ptr WLR_xcursor_manager -> IO ()

{-
 - Ensures an xcursor theme at the given scale factor is loaded in the manager.
 -}
foreign import capi "wlr/types/wlr_xcursor_manager.h wlr_xcursor_manager_load"
    wlr_xcursor_manager_load :: Ptr WLR_xcursor_manager -> CFloat -> IO CBool

{-
 - Retrieves a wlr_xcursor reference for the given cursor name at the given
 - scale factor, or NULL if this struct wlr_xcursor_manager has not loaded a
 - cursor theme at the requested scale.
 -}
foreign import capi "wlr/types/wlr_xcursor_manager.h wlr_xcursor_manager_get_xcursor"
    wlr_xcursor_manager_get_xcursor ::
        Ptr WLR_xcursor_manager
        -> CString
        -> CFloat
        -> IO (Ptr WLR_xcursor)
