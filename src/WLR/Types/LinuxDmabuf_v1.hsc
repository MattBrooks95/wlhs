module WLR.Types.LinuxDmabuf_v1 where
import WL.ServerCore (WL_signal)
import WL.Global (WL_global)

import WLR.Render.Renderer (WLR_renderer)
import WLR.Types.Output (WLR_output)
import WLR.Types.OutputLayer (WLR_output_layer_feedback_event)

import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_linux_dmabuf_v1.h>

{{ struct wlr/types/wlr_linux_dmabuf_v1.h,
    wlr_linux_dmabuf_feedback_v1_init_options,
    main_renderer, Ptr WLR_renderer,
    scanout_primary_output, Ptr WLR_output,
    output_layer_feedback_event, Ptr WLR_output_layer_feedback_event,
}}

{- the protocol interface
 - everything after events.destroy is "private state" and I can't find the
 - struct definition for wlr_linux_dmabuf_feedback_v1_compiled
 - there is a definition in types/wlr_linux_dmabuf_v1.c, but that's a .c file
 - not sure it's to be used by us
 -
 - going to skip it and see what happens
 -}
{{ struct wlr/types/wlr_linux_dmabuf_v1.h,
    wlr_linux_dmabuf_v1,
    global, Ptr WL_global,
    events destroy, WL_signal,
}}
{-
struct wlr_linux_dmabuf_feedback_v1_compiled *default_feedback;
struct wlr_drm_format_set default_formats; // for legacy clients
struct wl_list surfaces; // wlr_linux_dmabuf_v1_surface.link

int main_device_fd; // to sanity check FDs sent by clients, -1 if unavailable

struct wl_listener display_destroy;
-}
