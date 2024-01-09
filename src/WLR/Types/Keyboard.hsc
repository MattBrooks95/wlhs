{-# LANGUAGE EmptyDataDeriving, PatternSynonyms #-}

module WLR.Types.Keyboard where

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_keyboard.h>

pattern WLR_LED_COUNT :: (Eq a, Num a) => a
pattern WLR_LED_COUNT = #const WLR_LED_COUNT
