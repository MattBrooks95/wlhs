{-# LANGUAGE EmptyDataDecls, EmptyDataDeriving #-}

module WLR.Types.OutputLayer where

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_output_layer.h>

import Foreign
import Foreign.C.Types

import PIXMAN.Pixman
import WL.ServerCore
import WL.Utils
import WLR.Types.Buffer
import WLR.Util.Addon
import WLR.Util.Box
import WLR.Render.DrmFormatSet (WLR_drm_format_set)

{{ struct
    wlr/types/wlr_output_layer.h,
    wlr_output_layer,
    link,            WL_list,
    addons,          WLR_addon_set,
    events feedback, WL_signal,
    data,            Ptr (),
    src_box,         WLR_fbox,
    dst_box,         WLR_box
}}

{{ struct
    wlr/types/wlr_output_layer.h,
    wlr_output_layer_state,
    layer,    Ptr WLR_output_layer,
    buffer,   Ptr WLR_buffer,
    src_box,  WLR_fbox,
    dst_box,  WLR_box,
    damage,   Ptr PIXMAN_region32_t,
    accepted, CBool
}}

-- going to just use an empty data definition
-- this might live in linux/types.h, but I'm not sure
-- https://www.gnu.org/software/libc/manual/html_node/Attribute-Meanings.html
--data Dev_t
--instance Show Dev_t where
--    show = const "dev_t"
--{{ struct linux/types.h, dev_t }}
type DEV_t = CInt

{-
 - Feedback for an output layer.
 -
 - After an output commit, if the backend is not able to display a layer, it
 - can send feedback events. These events can be used to re-allocate the
 - layer's buffers so that they have a higher chance to get displayed.
 -}
{{ struct wlr/types/wlr_output_layer.h,
    wlr_output_layer_feedback_event,
    target_device, DEV_t,
    formats, Ptr WLR_drm_format_set,
}}
