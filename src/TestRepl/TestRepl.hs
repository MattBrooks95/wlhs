module TestRepl.TestRepl (
    testScene
    ) where

import WLR.Types.Scene
import WL.ServerCore (wl_display_create)
import Foreign (Word32)
import WLR.Types.XdgShell (wlr_xdg_shell_create)
import WLR.Types.Subcompositor (wlr_subcompositor_create)

testScene :: IO ()
testScene = do
    wlDisplay <- wl_display_create
    print wlDisplay
    wlrScene <- wlr_scene_create
    print wlrScene
    wlrXdgShell <- wlr_xdg_shell_create wlDisplay (1 :: Word32)
    print wlrXdgShell

testCompositor :: IO ()
testCompositor = do
    wlDisplay <- wl_display_create
    print $ "wlDisplay:" <> show wlDisplay
    subCompositor <- wlr_subcompositor_create wlDisplay
    print $ "sub compositor:" <> show subCompositor
