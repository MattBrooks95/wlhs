module WLR.XCursor where

import Foreign (Word32, Word8, Storable(..))
import Foreign.Ptr (Ptr)
import Foreign.C.Types (CUInt, CInt(..))
import Foreign.C.String (CString)
import WLR.Util.Edges (WLR_edges)

#include <wlr/xcursor.h>

{- | pasted from the wlroots file include/wlr/xcursor.h
 - Copyright © 2012 Intel Corporation
 -
 - Permission is hereby granted, free of charge, to any person obtaining
 - a copy of this software and associated documentation files (the
 - "Software"), to deal in the Software without restriction, including
 - without limitation the rights to use, copy, modify, merge, publish,
 - distribute, sublicense, and/or sell copies of the Software, and to
 - permit persons to whom the Software is furnished to do so, subject to
 - the following conditions:
 -
 - The above copyright notice and this permission notice (including the
 - next paragraph) shall be included in all copies or substantial
 - portions of the Software.
 -
 - THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 - EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 - MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 - NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 - BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 - ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 - CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 - SOFTWARE.
 -}

{-
 - This is a stable interface of wlroots. Future changes will be limited to:
 -
 - - New functions
 - - New struct members
 - - New enum members
 -
 - Note that wlroots does not make an ABI compatibility promise - in the future,
 - the layout and size of structs used by wlroots may change, requiring code
 - depending on this header to be recompiled (but not edited).
 -
 - Breaking changes are announced in the release notes and follow a 1-year
 - deprecation schedule.
 -}

{-
 - A still cursor image.
 -
 - The buffer contains pixels layed out in a packed DRM_FORMAT_ARGB8888 format.
 -}
{{ struct wlr/cursor.h,
    wlr_xcursor_image,
    width, Word32,
    height, Word32,
    hotspot_x, Word32,
    hotspot_y, Word32,
    delay, Word32,
    buffer, Ptr Word8
}}

{-
 - A cursor.
 -
 - If the cursor is animated, it may contain more than a single image.
 -}
{{ struct wlr/xcursor.h,
    wlr_xcursor,
    image_count, CUInt,
    images, Ptr (Ptr WLR_xcursor_image),
    name, CString,
    total_delay, Word32
}}

{-
 - Container for an Xcursor theme.
 -}
{{ struct wlr/xcursor.h,
    wlr_xcursor_theme,
    cursor_count, CUInt,
    cursors, Ptr (Ptr WLR_xcursor),
    name, CString,
    size, CInt
}}

{-
 - Loads the named Xcursor theme.
 -
 - This is useful if you need cursor images for your compositor to use when a
 - client-side cursor is not available or you wish to override client-side
 - cursors for a particular UI interaction (such as using a grab cursor when
 - moving a window around).
 -
 - The size is given in pixels.
 -
 - If a cursor theme with the given name couldn't be loaded, a fallback theme
 - is loaded.
 -
 - On error, NULL is returned.
 -}
foreign import capi "wlr/xcursor.h wlr_xcursor_theme_load"
    wlr_xcursor_theme_load :: CString -> CInt -> IO (Ptr WLR_xcursor_theme)

{-
 - Destroy a cursor theme.
 -
 - This implicitly destroys all child cursors and cursor images.
 -}
foreign import capi "wlr/xcursor.h wlr_xcursor_theme_destroy"
    wlr_xcursor_theme_destroy :: Ptr WLR_xcursor_theme -> IO ()

{-
 - Obtain a cursor for the specified name (e.g. "default").
 -
 - If the cursor could not be found, NULL is returned.
 -}
foreign import capi "wlr/xcursor.h wlr_xcursor_theme_get_cursor"
    wlr_xcursor_theme_get_cursor ::
        Ptr WLR_xcursor_theme
        -> CString
        -> IO (Ptr WLR_xcursor)

{-
 - Find the frame for a given elapsed time in a cursor animation.
 -
 - This function converts a timestamp (in ms) to a cursor image index.
 -}
foreign import capi "wlr/xcursor.h wlr_xcursor_frame"
    wlr_xcursor_frame :: Ptr WLR_xcursor -> Word32 -> IO CInt

{-
 - Get the name of the resize cursor for the given edges.
 -}
foreign import capi "wlr/xcursor.h wlr_xcursor_get_resize_name"
    wlr_xcursor_get_resize_name :: WLR_edges -> IO CString
