module TestRepl.TestRepl (
    testScene
    , testCompositor
    ) where

import WLR.Types.Scene
import WL.ServerCore (wl_display_create)
import Foreign (Word32, nullPtr)
import WLR.Types.XdgShell (wlr_xdg_shell_create)
import WLR.Types.Subcompositor (wlr_subcompositor_create)
import WLR.Backend (wlr_backend_autocreate)
import WLR.Render.Renderer (wlr_renderer_autocreate, wlr_renderer_init_wl_display)
import WLR.Render.Allocator (wlr_allocator_autocreate)
import WLR.Types.Compositor (wlr_compositor_create)
import WLR.Types.DataDevice (wlr_data_device_manager_create)
import WLR.Types.OutputLayout (wlr_output_layout_create)

testScene :: IO ()
testScene = do
    wlDisplay <- wl_display_create
    print wlDisplay
    wlrScene <- wlr_scene_create
    print wlrScene
    wlrXdgShell <- wlr_xdg_shell_create wlDisplay (1 :: Word32)
    print wlrXdgShell

-- Following along the steps in wlroots v0.17's tinywl example's main function
testCompositor :: IO ()
testCompositor = do
    wlDisplay <- wl_display_create
    print $ "wlDisplay:" <> show wlDisplay
    -- TODO the wlr_backend_autocreate function has already changed upstream
    -- it takes an event loop now
    -- backend <- flip wlr_backend_autocreate nullPtr =<< wl_display_get_event_loop wlDisplay
    -- TODO this is failing on my Nvidia PC, yay nvidia
    -- https://forums.developer.nvidia.com/t/dri3open-missing-in-the-x11-driver-for-wlroots-compositors/214164/2
    backend <- wlr_backend_autocreate wlDisplay nullPtr
    print $ "backend:" <> show backend
    renderer <- wlr_renderer_autocreate backend
    print $ "renderer" <> show renderer
    initSucc <- wlr_renderer_init_wl_display renderer wlDisplay
    print $ "render init wl display success?:" <> show initSucc
    allocator <- wlr_allocator_autocreate backend renderer
    print $ "allocator:" <> show allocator
    compositor <- wlr_compositor_create wlDisplay 5 renderer
    print $ "compositor:" <> show compositor
    subCompositor <- wlr_subcompositor_create wlDisplay
    print $ "subcompositor:" <> show subCompositor
    dataDeviceManager <- wlr_data_device_manager_create wlDisplay
    print $ "data device manager:" <> show dataDeviceManager
    outputLayout <- wlr_output_layout_create
    print $ "output layout" <> outputLayout
