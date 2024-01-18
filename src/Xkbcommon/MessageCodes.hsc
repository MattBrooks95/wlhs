module XkbCommon.MessageCodes where

--enum xkb_explicit_components {
pattern EXPLICIT_INTERP :: (Eq a, Num a) => a
pattern EXPLICIT_INTERP = 1
pattern EXPLICIT_VMODMAP :: (Eq a, Num a) => a
pattern EXPLICIT_VMODMAP = 2
pattern EXPLICIT_REPEAT :: (Eq a, Num a) => a
pattern EXPLICIT_REPEAT = 4
