{-# LANGUAGE PatternSynonyms #-}
module Xkbcommon.Keymap where

import Foreign.C.Types (CInt)

import Data.Bits ((.|.))

import Xkbcommon.Xkbcommon (
    XKB_keymap_compile_flags
    )

--enum xkb_action_controls {
pattern CONTROL_REPEAT :: (Eq a, Num a) => a
pattern CONTROL_REPEAT = 1
pattern CONTROL_SLOW :: (Eq a, Num a) => a
pattern CONTROL_SLOW = 2
pattern CONTROL_DEBOUNCE :: (Eq a, Num a) => a
pattern CONTROL_DEBOUNCE = 4
pattern CONTROL_STICKY :: (Eq a, Num a) => a
pattern CONTROL_STICKY = 8
pattern CONTROL_MOUSEKEYS :: (Eq a, Num a) => a
pattern CONTROL_MOUSEKEYS = 16
pattern CONTROL_MOUSEKEYS_ACCEL :: (Eq a, Num a) => a
pattern CONTROL_MOUSEKEYS_ACCEL = 32
pattern CONTROL_AX :: (Eq a, Num a) => a
pattern CONTROL_AX = 64
pattern CONTROL_AX_TIMEOUT :: (Eq a, Num a) => a
pattern CONTROL_AX_TIMEOUT = 128
pattern CONTROL_AX_FEEDBACK :: (Eq a, Num a) => a
pattern CONTROL_AX_FEEDBACK = 256
pattern CONTROL_BELL :: (Eq a, Num a) => a
pattern CONTROL_BELL = 512
pattern CONTROL_IGNORE_GROUP_LOCK :: (Eq a, Num a) => a
pattern CONTROL_IGNORE_GROUP_LOCK = 1024

-- TODO check that this gets the correct answer
-- it should be all of the flags bitwise OR'd together
control_all :: CInt
control_all = foldl (.|.) 0 [
        CONTROL_REPEAT, CONTROL_SLOW, CONTROL_DEBOUNCE, CONTROL_STICKY,
        CONTROL_MOUSEKEYS, CONTROL_MOUSEKEYS_ACCEL, CONTROL_AX,
        CONTROL_AX_TIMEOUT, CONTROL_AX_FEEDBACK, CONTROL_BELL,
        CONTROL_IGNORE_GROUP_LOCK
    ]

