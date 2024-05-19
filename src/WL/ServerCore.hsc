{-# LANGUAGE EmptyDataDeriving #-}

module WL.ServerCore where

#include <wayland-server-core.h>

import Foreign
import Foreign.C.String
import Foreign.Ptr (FunPtr)

import WL.Utils
import WL.ServerProtocol

-- Opaque objects
{{ struct wayland-server-core.h, wl_event_source }}
{{ struct wayland-server-core.h, wl_global }}
{{ struct wayland-server-core.h, wl_resource }}

type WL_notify_func_t
    =  WL_listener
    -> Ptr ()
    -> IO ()

{{ struct
    wayland-server-core.h,
    wl_listener,
    link, Ptr WL_list,
    notify, FunPtr WL_notify_func_t
}}

{{ struct
    wayland-server-core.h,
    wl_signal,
    listener_list, WL_list
}}

foreign import capi "wayland-server-core.h wl_signal_init"
    wl_signal_init :: Ptr WL_signal -> IO ()

foreign import capi "wayland-server-core.h wl_signal_add"
    wl_signal_add :: Ptr WL_signal -> Ptr WL_listener -> IO ()

foreign import capi "wayland-server-core.h wl_signal_get"
    wl_signal_get :: Ptr WL_signal -> FunPtr WL_notify_func_t -> IO ()

foreign import capi "wayland-server-core.h wl_signal_emit"
    wl_signal_emit :: Ptr WL_signal -> Ptr () -> IO ()

foreign import capi "wayland-server-core.h wl_display_create"
    wl_display_create :: IO (Ptr WL_display)

foreign import capi "wayland-server-core.h wl_display_destroy"
    wl_display_destroy :: Ptr WL_display -> IO ()

foreign import capi "wayland-server-core.h wl_display_add_socket_auto"
    wl_display_add_socket_auto :: Ptr WL_display -> IO CString

foreign import capi "wayland-server-core.h wl_display_run"
    wl_display_run :: Ptr WL_display -> IO ()

foreign import capi "wayland-server-core.h wl_display_destroy_clients"
    wl_display_destroy_clients :: Ptr WL_display -> IO ()

type WL_resource_destroy_func_t = FunPtr (Ptr WL_resource -> IO ())
--typedef void (*wl_resource_destroy_func_t)(struct wl_resource *resource);

{- TODO doing an empty data decl until I can find the actual definition -}
data WL_event_loop

foreign import capi "wayland-server-core.h wl_display_get_event_loop"
    wl_display_get_event_loop :: Ptr WL_display -> IO (Ptr WL_event_loop)
