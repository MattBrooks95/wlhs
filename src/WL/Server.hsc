module WL.Server where
import WL.ServerCore (WL_resource_destroy_func_t)

#include <wl-server.h>
-- TODO does 'const' mean anything for the Haskell bindings?
-- const void *implementation;
{{ struct
    wl-server.h,
    wl_object,
    interface, Ptr WL_interface,
    implementation, Ptr (),
    id, CUint
};

{{ struct
    wl-server.h,
    wl_resource,
    object, WL_object,
    destroy, Ptr WL_resource_destroy_func_t
    link, WL_list,
    destroy_signal, WL_signal,
    client, WL_client,
    data, Ptr ()
}}
