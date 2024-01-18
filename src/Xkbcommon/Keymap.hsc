{-# LANGUAGE PatternSynonyms #-}
module Xkbcommon.Keymap where

-- TODO this import is not working because the file is not
-- in libxkbcommon's include directory, it's under the src directory
--
-- does that mean I have to write this without using hsc2hs????
-- do I need to grab a development build of it with nix??????
#include <xkbcommon/src/keymap.h>

import Foreign.C.Types (CInt)

import Data.Bits ((.|.))

import Xkbcommon.Xkbcommon (
    XKB_keymap_compile_flags
    , Xkb_keycode_t
    )
import Xkbcommon.Context (
    XKB_log_level
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
xkb_action_controls_control_all :: CInt
control_all = foldl (.|.) 0 [
    CONTROL_REPEAT, CONTROL_SLOW, CONTROL_DEBOUNCE, CONTROL_STICKY,
    CONTROL_MOUSEKEYS, CONTROL_MOUSEKEYS_ACCEL, CONTROL_AX,
    CONTROL_AX_TIMEOUT, CONTROL_AX_FEEDBACK, CONTROL_BELL,
    CONTROL_IGNORE_GROUP_LOCK
    ]

--TODO how can I associate these as being related (of the same group)
--while also using PatternSynonyms, like BurningWitness was talking about
--enum xkb_explicit_components {
pattern EXPLICIT_INTERP :: (Eq a, Num a) => a
pattern EXPLICIT_INTERP = 1
pattern EXPLICIT_VMODMAP :: (Eq a, Num a) => a
pattern EXPLICIT_VMODMAP = 2
pattern EXPLICIT_REPEAT :: (Eq a, Num a) => a
pattern EXPLICIT_REPEAT = 4

--enum xkb_range_exceed_type {
--TODO RANGE_WRAP is specified as 0 but the others are not specified
--what does that mean for the bindings
{{ enum
    Xkb_range_exceed_type,
    RANGE_WRAP,
    RANGE_SATURATE,
    RANGE_REDIRECT
}}

--struct xkb_key {
{{ struct
    keymap.h,
    xkb_key,
    keycode, XKB_keycode_t
    name, XKB_atom_t
    explicit, CInt
    modmap, Xkb_mod_mask_t
    vmodmap, Xkb_mod_mask_t
    repeats, Bool

--
--    enum xkb_range_exceed_type out_of_range_group_action;
--    xkb_layout_index_t out_of_range_group_number;
--
--    xkb_layout_index_t num_groups;
--    struct xkb_group *groups;
--};

{{ struct
    keymap.h,
    xkb_keymap,
    ctx, XKB_context,
    refcnt, CInt,
    flags, XKB_keymap_compile_flags
    format, XKB_keymap_format
    enabled_controls, XKB_action_controls
    min_key_code, Xkb_keycode_t
    max_key_code, Xkb_keycode_t
}}
--struct xkb_keymap {
--
--
--    struct xkb_key *keys;
--
--    /* aliases in no particular order */
--    unsigned int num_key_aliases;
--    struct xkb_key_alias *key_aliases;
--
--    struct xkb_key_type *types;
--    unsigned int num_types;
--
--    unsigned int num_sym_interprets;
--    struct xkb_sym_interpret *sym_interprets;
--
--    struct xkb_mod_set mods;
--
--    /* Number of groups in the key with the most groups. */
--    xkb_layout_index_t num_groups;
--    /* Not all groups must have names. */
--    xkb_layout_index_t num_group_names;
--    xkb_atom_t *group_names;
--
--    struct xkb_led leds[XKB_MAX_LEDS];
--    unsigned int num_leds;
--
--    char *keycodes_section_name;
--    char *symbols_section_name;
--    char *types_section_name;
--    char *compat_section_name;
--};
