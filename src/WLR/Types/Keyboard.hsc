{-# LANGUAGE EmptyDataDeriving, PatternSynonyms #-}

module WLR.Types.Keyboard where

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_keyboard.h>

import Foreign (Word32, Storable(..))
import Foreign.C.Types (CSize(..), CInt)
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr)

import WLR.Types.InputDevice (WLR_input_device)
import WLR.Types.KeyboardGroup (WLR_keyboard_group)

pattern WLR_LED_COUNT :: (Eq a, Num a) => a
pattern WLR_LED_COUNT = #const WLR_LED_COUNT

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

data Xkb_keymap
data Xkb_state

{{ struct
    wlr/types/wlr_keyboard.h,
    wlr_keyboard,
    base, Ptr WLR_input_device,
    impl, Ptr WLR_keyboard_impl,
    group , Ptr WLR_keyboard_group,
    keymap_string, CString,
    keymap_size, CSize,
    keymap_fd, CInt,
}}
    --struct xkb_keymap *keymap;
    --struct xkb_state *xkb_state;
    --xkb_led_index_t led_indexes[WLR_LED_COUNT];
    --xkb_mod_index_t mod_indexes[WLR_MODIFIER_COUNT];

    --uint32_t leds;
    --uint32_t keycodes[WLR_KEYBOARD_KEYS_CAP];
    --size_t num_keycodes;
    --struct wlr_keyboard_modifiers modifiers;

    --struct {
    --    int32_t rate;
    --    int32_t delay;
    --} repeat_info;

    --struct {
    --    /**
    --     * The `key` event signals with a struct wlr_keyboard_key_event that a
    --     * key has been pressed or released on the keyboard. This event is
    --     * emitted before the xkb state of the keyboard has been updated
    --     * (including modifiers).
    --     */
    --    struct wl_signal key;

    --    /**
    --     * The `modifiers` event signals that the modifier state of the
    --     * struct wlr_keyboard has been updated. At this time, you can read the
    --     * modifier state of the struct wlr_keyboard and handle the updated
    --     * state by sending it to clients.
    --     */
    --    struct wl_signal modifiers;
    --    struct wl_signal keymap;
    --    struct wl_signal repeat_info;
    --} events;

    --void *data;
    --}
