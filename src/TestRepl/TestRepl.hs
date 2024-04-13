module TestRepl.TestRepl (
    testScene
    ) where

import WLR.Types.Scene

testScene :: IO ()
testScene = do
    wlrScene <- wlr_scene_create
    print wlrScene
