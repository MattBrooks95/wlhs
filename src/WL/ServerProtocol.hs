module WL.ServerProtocol where

data {-# CTYPE "wayland-server-core.h" "struct wl_display" #-} WL_display

{{ enum
    wayland-server-protocol.h,
    wl_data_device_manager_dnd_action,
    WL_DATA_DEVICE_MANAGER_DND_ACTION_NONE,
    WL_DATA_DEVICE_MANAGER_DND_ACTION_COPY,
    WL_DATA_DEVICE_MANAGER_DND_ACTION_MOVE,
    WL_DATA_DEVICE_MANAGER_DND_ACTION_ASK,
}}
