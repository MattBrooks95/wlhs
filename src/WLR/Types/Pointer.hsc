{-# LANGUAGE EmptyDataDeriving #-}
module WLR.Types.Pointer (
    WLR_pointer(..)
    , WLR_pointer_events(..)
    ) where

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_pointer.h>

import Foreign.Ptr (
    Ptr
    )
import Foreign.C.String (
    CString
    )
import Foreign.Storable (
    Storable(..)
    )

import WLR.Types.InputDevice (
    WLR_input_device
    )
import WL.ServerCore (
    WL_signal
    )

data {-# CTYPE "wlr/types/wlr_pointer.h" "struct wlr_pointer_impl" #-} WLR_pointer_impl
    deriving (Show)

data {-# CTYPE "wlr/types/wlr_pointer.h" "struct wlr_pointer" #-} WLR_pointer
    = WLR_pointer
    { wlr_pointer_base :: Ptr WLR_input_device
    , wlr_pointer_impl :: Ptr WLR_pointer_impl
    , wlr_pointer_output_name :: Ptr CString
    , wlr_pointer_events :: WLR_pointer_events

    -- TODO what type is this? It's a void* in the C source
    -- void *data;
    , wlr_pointer_data :: Ptr ()
    }

instance Storable WLR_pointer where
    alignment _ = #alignment struct wlr_pointer
    sizeOf _ = #size struct wlr_pointer
    peek ptr = WLR_pointer
        <$> (#peek struct wlr_pointer, base) ptr
        <*> (#peek struct wlr_pointer, impl) ptr
        <*> (#peek struct wlr_pointer, output_name) ptr
        -- TODO this 'events' attribute isn't actually a pointer in the source
        -- what should I do?
        <*> (#peek struct wlr_pointer, events) ptr
        <*> (#peek struct wlr_pointer, data) ptr
    poke ptr t = do
        (#poke struct wlr_pointer, base) ptr $ wlr_pointer_base t
        (#poke struct wlr_pointer, impl) ptr $ wlr_pointer_impl t
        (#poke struct wlr_pointer, output_name) ptr $ wlr_pointer_output_name t
        (#poke struct wlr_pointer, events) ptr $ wlr_pointer_events t
        (#poke struct wlr_pointer, data) ptr $ wlr_pointer_data t

{- 
 - TODO there were some source comments here that had some sort of struct
 - type specified, I guess it's a hint about the types???
 - I think I did the comment structure so that `struct wlr_pointer_motion_event` will show on hover
 - need to check that when I get my hls working ~ Matt t
-}
data WLR_pointer_events = WLR_pointer_events {
    -- |struct wlr_pointer_motion_event
    wlr_pointer_events_motion :: WL_signal
    -- |struct wlr_pointer_motion_absolute_event
    , wlr_pointer_events_motion_absolute :: WL_signal
    -- |struct wlr_pointer_button_event
    , wlr_pointer_events_button :: WL_signal
    -- |struct wlr_pointer_axis_event
    , wlr_pointer_events_axis :: WL_signal
    , wlr_pointer_events_frame :: WL_signal

    -- |struct wlr_pointer_swipe_begin_event
    , wlr_pointer_events_swipe_begin :: WL_signal
    -- |struct wlr_pointer_swipe_update_event
    , wlr_pointer_events_swipe_update :: WL_signal
    -- |struct wlr_pointer_swipe_end_event
    , wlr_pointer_events_swipe_end :: WL_signal

    -- |struct wlr_pointer_pinch_begin_event
    , wlr_pointer_events_pinch_begin :: WL_signal
    -- |struct wlr_pointer_pinch_update_event
    , wlr_pointer_events_pinch_update :: WL_signal
    -- |struct wlr_pointer_pinch_end_event
    , wlr_pointer_events_pinch_end :: WL_signal

    -- |struct wlr_pointer_hold_begin_event
    , wlr_pointer_events_hold_begin :: WL_signal
    -- |struct wlr_pointer_hold_end_event
    , wlr_pointer_events_hold_end :: WL_signal
    }

instance Storable WLR_pointer_events where
    -- Because this 'events' struct is defined within the pointer struct,
    -- does that mean that I can't use this #alignment???
    -- I doubt this works, but it compiles without sizeOf and alignment
    -- maybe I have to use the argument to `alignment` TODO look into that,
    -- maybe I can use that to tell it where to find the nested struct definition
    --alignment _ = #alignment struct wlr_pointer.events
    --sizeOf _ = #size struct wlr_pointer.events
    peek ptr = WLR_pointer_events
        <$> (#peek struct wlr_pointer, events.motion) ptr
        <*> (#peek struct wlr_pointer, events.motion_absolute) ptr
        <*> (#peek struct wlr_pointer, events.button) ptr
        <*> (#peek struct wlr_pointer, events.axis) ptr
        <*> (#peek struct wlr_pointer, events.frame) ptr
        <*> (#peek struct wlr_pointer, events.swipe_begin) ptr
        <*> (#peek struct wlr_pointer, events.swipe_update) ptr
        <*> (#peek struct wlr_pointer, events.swipe_end) ptr
        <*> (#peek struct wlr_pointer, events.pinch_begin) ptr
        <*> (#peek struct wlr_pointer, events.pinch_update) ptr
        <*> (#peek struct wlr_pointer, events.pinch_end) ptr
        <*> (#peek struct wlr_pointer, events.hold_begin) ptr
        <*> (#peek struct wlr_pointer, events.hold_end) ptr
    poke ptr t = do
        (#poke struct wlr_pointer, events.motion) ptr $ wlr_pointer_events_motion t
        (#poke struct wlr_pointer, events.motion_absolute) ptr $ wlr_pointer_events_motion_absolute t
        (#poke struct wlr_pointer, events.button) ptr $ wlr_pointer_events_button t
        (#poke struct wlr_pointer, events.axis) ptr $ wlr_pointer_events_axis t
        (#poke struct wlr_pointer, events.frame) ptr $ wlr_pointer_events_frame t
        (#poke struct wlr_pointer, events.swipe_begin) ptr $ wlr_pointer_events_swipe_begin t
        (#poke struct wlr_pointer, events.swipe_update) ptr $ wlr_pointer_events_swipe_update t
        (#poke struct wlr_pointer, events.swipe_end) ptr $ wlr_pointer_events_swipe_end t
        (#poke struct wlr_pointer, events.pinch_begin) ptr $ wlr_pointer_events_pinch_begin t
        (#poke struct wlr_pointer, events.pinch_update) ptr $ wlr_pointer_events_pinch_update t
        (#poke struct wlr_pointer, events.pinch_end) ptr $ wlr_pointer_events_pinch_end t
        (#poke struct wlr_pointer, events.hold_begin) ptr $ wlr_pointer_events_hold_begin t
        (#poke struct wlr_pointer, events.hold_end) ptr $ wlr_pointer_events_hold_end t
