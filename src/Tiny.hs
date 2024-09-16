{-# LANGUAGE PatternSynonyms #-}
module Tiny (
    main
    ) where

import WLR.Util.Log (wlr_log_init, pattern WLR_DEBUG)
import Foreign.Ptr (nullFunPtr, nullPtr)
import WL.ServerCore (wl_display_create)
import WLR.Backend (wlr_backend_autocreate)
import WLR.Render.Renderer (wlr_renderer_autocreate, wlr_renderer_init_wl_display)
import WLR.Render.Allocator (wlr_allocator_autocreate)
import WLR.Types.Compositor (wlr_compositor_create)

main :: IO ()
main = do
    wlr_log_init WLR_DEBUG nullFunPtr
    display <- wl_display_create
    backend <- wlr_backend_autocreate display nullPtr
    renderer <- wlr_renderer_autocreate backend
    _ <- wlr_renderer_init_wl_display renderer display
    allocator <- wlr_allocator_autocreate backend renderer
    compositor <- wlr_compositor_create display 5 renderer
    undefined
