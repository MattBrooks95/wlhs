{-# LANGUAGE PatternSynonyms #-}
module WLR.Render.Pass where

import Foreign.C.Types (CInt(..))

#define WLR_USE_UNSTABLE
#include<wlr/render/pass.h>

{- |
 - Filter modes.
 -    /* bilinear texture filtering (default) */
 -    WLR_SCALE_FILTER_BILINEAR,
 -    /* nearest texture filtering */
 -    WLR_SCALE_FILTER_NEAREST,
 -}
{{ enum WLR_scale_filter_mode,
    WLR_SCALE_FILTER_BILINEAR,
    WLR_SCALE_FILTER_NEAREST
}}
