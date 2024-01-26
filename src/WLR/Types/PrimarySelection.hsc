module WLR.Types.PrimarySelection where

{{ struct wlr/types/wlr_primary_selection.h, wlr_primary_selection_source_impl }}

{{ struct
    wlr/types/wlr_primary_selection.h,
    wlr_primary_selection_source, Ptr WLR_primary_selection_source_impl,
    mime_types, WL_array,
    events destroy, WL_signal,
    data, Ptr ()
}}
