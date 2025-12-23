# letrs

A library for parsing FIGfonts and rendering text using these fonts.

## Features

- Horizontal kerning/fitting and smushing
- Vertical fitting and smushing (no other rust crate seems to support this!)
- Alignment for multi-line output
- Automatic line breaking depending on maximum width

## Example

```rust
let rendered: String = Font::standard().renderer().render("Hello, world!");
let expected = concat!(
r" _   _      _ _                             _     _ _ ", "\n",
r"| | | | ___| | | ___    __      _____  _ __| | __| | |", "\n",
r"| |_| |/ _ \ | |/ _ \   \ \ /\ / / _ \| '__| |/ _` | |", "\n",
r"|  _  |  __/ | | (_) |   \ V  V / (_) | |  | | (_| |_|", "\n",
r"|_| |_|\___|_|_|\___( )   \_/\_/ \___/|_|  |_|\__,_(_)", "\n",
r"                    |/                                "
);
assert_eq!(rendered, expected);
```
