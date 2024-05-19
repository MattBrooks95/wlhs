# wlhs

This project aims to develop a set of Haskell bindings for `wlroots`
  (and some parts of `libwayland`)
At the moment it focusses on low-level bindings, in the `wlhs-bindings` package.

**Warning: this project has just begun!**
Currently, the bindings are highly incomplete.
Please feel free to help us expand them!

# Development

**We currently target wlroots version `0.17.1`.**

There is a Nix development flake available, which may be accessed via `nix develop`.
For [direnv][ghub:direnv] users, an `.envrc` file is also provided.

[ghub:direnv]: https://github.com/direnv/direnv

## hsc2hs extensions

`wlhs-bindings` contains a custom `Setup.hs`,
  which extends [hsc2hs](https://github.com/haskell/hsc2hs) files with some custom syntax.
*note* be careful about trailing commas in templated sections
```
{{ enum WLR_some_enum,
    ENUM_VAL_1,
    ENUM_VAL_2,
    ENUM_VAL_3,
}}
```
may make the build fail because of the trailing comma after `ENUM_VAL_3`

This is probably best illustrated by example:

<table>
<tr>
<th>Macro call</th>
<th>Equivalent to</th>
</tr>
<tr>
<td>

```
{{ struct
    include.h,
    wl_type_name
}}
```

</td>
<td>

```hs
data {-# CTYPE "include.h" "struct wl_type_name" #-} WL_type_name
    deriving (Show)
```

(Note: requires `{-# LANGUAGE EmptyDataDeriving #-}`)

</td>
</tr>
<td>

```
{{ struct
    include.h,
    wl_type_name,
    field1, Type1,
    field2, Type2,
    nested field, Type2,
    arrayfield, [3]Type3
}}
```

</td>
<td>

```hs
data {-# CTYPE "include.h" "struct wl_type_name" #-} WL_type_name
    = WL_type_name
    { wl_type_name_field1 :: Type1
    , wl_type_name_field2 :: Type2
    , wl_type_name_nested_field :: Type2
    , wl_type_name_arrayfield :: [Type3]
    } deriving (Show)
    
instance Storable WL_type_name where
    alignment _ = #alignment struct wl_type_name
    sizeOf _ = #size struct wl_type_name
    peek ptr = WL_type_name
        <$> (#peek struct wl_type_name, field1) ptr
        <*> (#peek struct wl_type_name, field2) ptr
        <*> (#peek struct wl_type_name, nested.field) ptr
        <*> peekArray 3 ((#ptr struct wl_type_name, arrayfield) ptr)
    poke ptr t = do
        (#poke struct wl_type_name, field1) ptr (wl_type_name_field1 t)
        (#poke struct wl_type_name, field2) ptr (wl_type_name_field2 t)
        (#poke struct wl_type_name, nested.field) ptr (wl_type_name_nested_field t)
        pokeArray ((#ptr struct wl_type_name, nested.field) ptr) (wl_type_name_nested_field t)
```

</td>
</tr>
<tr>
<td>

```
{{ enum
    WL_type_name,
    WLR_ENUM_VALUE_1,
    WLR_ENUM_VALUE_2
}}
```

</td>
<td>

```hs
type WL_type_name = CInt

pattern WLR_ENUM_VALUE_1 :: (Eq a, Num a) => a
pattern WLR_ENUM_VALUE_1 = #const WLR_ENUM_VALUE_1 

pattern WLR_ENUM_VALUE_2 :: (Eq a, Num a) => a
pattern WLR_ENUM_VALUE_2 = #const WLR_ENUM_VALUE_2 
```

</td>
</tr>
</table>

## Gotchas
- be careful about the generated Data declaration's field type names
    - for example, the struct `wlr_surface` has a field `has_buffer`, so the template will generate a field name `wlr_surface_has_buffer`, which conflicts with the name of an actual function in the Wlroots C code
    - I suppose we can fix this just by adding some suffix to the end of the Haskell function name, like '_func', to distinguish it from the record field constructor
    ```haskell
    foreign import capi "wlr/types/wlr_compositor.h wlr_surface_has_buffer"
        wlr_surface_has_buffer_func :: Ptr WLR_surface -> IO (CBool)
    ```
