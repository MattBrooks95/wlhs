{-# LANGUAGE EmptyDataDeriving, PatternSynonyms #-}

module WLR.Types.Keyboard where

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_keyboard.h>

import Foreign (Word32, Storable(..))
import Foreign.C.Types (CSize(..), CInt(..), CBool(..))
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr)

import WLR.Types.InputDevice (WLR_input_device)
import WLR.Types.KeyboardGroup (WLR_keyboard_group)

import WL.ServerCore (WL_signal)
import WL.Keyboard (WL_keyboard_key_state)

pattern WLR_LED_COUNT :: (Eq a, Num a) => a
pattern WLR_LED_COUNT = #const WLR_LED_COUNT

type WLR_keyboard_led = CInt
-- enum wlr_keyboard_led
--WLR_LED_NUM_LOCK = 1 << 0,
pattern WLR_LED_NUM_LOCK :: (Eq a, Num a) => a
pattern WLR_LED_NUM_LOCK = 1
--WLR_LED_CAPS_LOCK = 1 << 1,
pattern WLR_LED_CAPS_LOCK :: (Eq a, Num a) => a
pattern WLR_LED_CAPS_LOCK = 2
--WLR_LED_SCROLL_LOCK = 1 << 2,
pattern WLR_LED_SCROLL_LOCK :: (Eq a, Num a) => a
pattern WLR_LED_SCROLL_LOCK = 4

pattern WLR_MODIFIER_COUNT :: (Eq a, Num a) => a
pattern WLR_MODIFIER_COUNT = 8

type Wlr_keyboard_modifier = CInt
-- enum wlr_keyboard_modifier {
-- WLR_MODIFIER_SHIFT = 1 << 0,
pattern WLR_MODIFIER_SHIFT :: (Eq a, Num a) => a
pattern WLR_MODIFIER_SHIFT = 1
-- WLR_MODIFIER_CAPS = 1 << 1,
pattern WLR_MODIFIER_CAPS :: (Eq a, Num a) => a
pattern WLR_MODIFIER_CAPS = 2
-- WLR_MODIFIER_CTRL = 1 << 2,
pattern WLR_MODIFIER_CTRL :: (Eq a, Num a) => a
pattern WLR_MODIFIER_CTRL = 4
-- WLR_MODIFIER_ALT = 1 << 3,
pattern WLR_MODIFIER_ALT :: (Eq a, Num a) => a
pattern WLR_MODIFIER_ALT = 8
-- WLR_MODIFIER_MOD2 = 1 << 4,
pattern WLR_MODIFIER_MOD2 :: (Eq a, Num a) => a
pattern WLR_MODIFIER_MOD2 = 16
-- WLR_MODIFIER_MOD3 = 1 << 5,
pattern WLR_MODIFIER_MOD3 :: (Eq a, Num a) => a
pattern WLR_MODIFIER_MOD3 = 32
-- WLR_MODIFIER_LOGO = 1 << 6,
pattern WLR_MODIFIER_LOGO :: (Eq a, Num a) => a
pattern WLR_MODIFIER_LOGO = 64
-- WLR_MODIFIER_MOD5 = 1 << 7,
pattern WLR_MODIFIER_MOD5 :: (Eq a, Num a) => a
pattern WLR_MODIFIER_MOD5 = 128

pattern WLR_KEYBOARD_KEYS_CAP :: (Eq a, Num a) => a
pattern WLR_KEYBOARD_KEYS_CAP = 32

data {-# CTYPE "wlr/types/wlr_keyboard.h" "struct wlr_keyboard impl" #-} WLR_keyboard_impl
    deriving (Show)

-- xkbd_mod_mask_t is a type alias for uint32_t

{{ struct
    wlr/types/wlr_keyboard.h,
    wlr_keyboard_modifiers,
    depressed, Word32,
    latched, Word32,
    locked, Word32,
    group, Word32
}}

-- cannot import these types from libxcommon because of their fields which have
-- internal types that aren't exported
data Xkb_keymap
data Xkb_state

-- TODO write this by hand or update the macro to work with arrays
type ArrayType = ()

{{ struct
    wlr/types/wlr_keyboard.h,
    wlr_keyboard,
    base, Ptr WLR_input_device,
    impl, Ptr WLR_keyboard_impl,
    group , Ptr WLR_keyboard_group,
    keymap_string, CString,
    keymap_size, CSize,
    keymap_fd, CInt,
    keymap, Ptr Xkb_keymap,
    xkb_state, Ptr Xkb_state,
    led_indexes, ArrayType,
    mod_indexes, ArrayType,
    leds, CInt,
    keycodes, ArrayType,
    num_keycodes, CSize,
    modifiers, WLR_keyboard_modifiers,
    repeat_info rate, Word32,
    repeat_info delay, Word32,
    events key, WL_signal,
    events modifiers, WL_signal,
    events keymap, WL_signal,
    events repeat_info, WL_signal,
    data, Ptr ()
}}
    --remaining array types
    --xkb_led_index_t led_indexes[WLR_LED_COUNT];
    --xkb_mod_index_t mod_indexes[WLR_MODIFIER_COUNT];
    --uint32_t keycodes[WLR_KEYBOARD_KEYS_CAP];

{{ struct
    wlr/types/wlr_keyboard.h,
    wlr_keyboard_key_event,
    time_msec, Word32,
    keycode, Word32,
    update_state, CBool,
    state, WL_keyboard_key_state
}}

{-
 - Get a struct wlr_keyboard from a struct wlr_input_device.
 -
 - Asserts that the input device is a keyboard.
 -}
foreign import capi "wlr/types/wlr_keyboard.h wlr_keyboard_from_input_device"
    wlr_keyboard_from_input_device :: Ptr WLR_input_device -> IO (Ptr WLR_keyboard)

foreign import capi "wlr/types/wlr_keyboard.h wlr_keyboard_set_keymap"
    wlr_keyboard_set_keymap :: Ptr wLR_keyboard -> Ptr Xkb_keymap -> CBool

foreign import capi "wlr/types/wlr_keyboard.h wlr_keyboard_keymaps_match"
    wlr_keyboard_keymaps_match :: Ptr Xkb_keymap -> Ptr Xkb_keymap -> CBool

{-
 - Set the keyboard repeat info.
 -
 - rate is in key repeats/second and delay is in milliseconds.
 -
 - keyboard -> rate -> delay
 -}
foreign import capi "wlr/types/wlr_keyboard.h wlr_keyboard_set_repeat_info"
    wlr_keyboard_set_repeat_info :: Ptr WLR_keyboard -> CInt -> CInt -> IO ()

{-
 - Update the LEDs on the device, if any.
 -
 - leds is a bitmask of enum wlr_keyboard_led.
 -
 - If the device doesn't have the provided LEDs, this function is a no-op.
 -}

foreign import capi "wlr/types/wlr_keyboard.h wlr_keyboard_led_update"
    wlr_keyboard_led_update :: Ptr WLR_keyboard -> CInt -> IO ()

{-
 - Get the set of currently depressed or latched modifiers.
 -
 - A bitmask of enum wlr_keyboard_modifier is returned.
 -}
foreign import capi "wlr/types/wlr_keyboard.h wlr_keyboard_get_modifiers"
    wlr_keyboard_get_modifiers :: Ptr WLR_keyboard -> IO (Wlr_keyboard_modifier)
