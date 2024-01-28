module WL.ServerProtocol where

#include <wayland-server-protocol.h>

data {-# CTYPE "wayland-server-core.h" "struct wl_display" #-} WL_display

{{ enum
    wayland-server-protocol.h,
    wl_data_device_manager_dnd_action,
    WL_DATA_DEVICE_MANAGER_DND_ACTION_NONE,
    WL_DATA_DEVICE_MANAGER_DND_ACTION_COPY,
    WL_DATA_DEVICE_MANAGER_DND_ACTION_MOVE,
    WL_DATA_DEVICE_MANAGER_DND_ACTION_ASK,
}}

enum wl_data_device_manager_dnd_action {
	/**
	 * no action
	 */
	WL_DATA_DEVICE_MANAGER_DND_ACTION_NONE = 0,
	/**
	 * copy action
	 */
	WL_DATA_DEVICE_MANAGER_DND_ACTION_COPY = 1,
	/**
	 * move action
	 */
	WL_DATA_DEVICE_MANAGER_DND_ACTION_MOVE = 2,
	/**
	 * ask action
	 */
	WL_DATA_DEVICE_MANAGER_DND_ACTION_ASK = 4,
};
