# letrs

A command line interface for the `letrs` crate, to directly render text using FIGfonts.

## Usage

- Install using `cargo install letrs-cli` (assuming you have cargo installed)
- Run using `letrs "input"`
- See `letrs -h` for optional arguments

```console
user@machine:~$ letrs "Hello, world!"
 _   _      _ _                             _     _ _ 
| | | | ___| | | ___    __      _____  _ __| | __| | |
| |_| |/ _ \ | |/ _ \   \ \ /\ / / _ \| '__| |/ _` | |
|  _  |  __/ | | (_) |   \ V  V / (_) | |  | | (_| |_|
|_| |_|\___|_|_|\___( )   \_/\_/ \___/|_|  |_|\__,_(_)
                    |/                                
```

## Features

- Horizontal kerning/fitting and smushing
- Vertical fitting and smushing (seems no other rust implementation supports this!)
- Alignment for multi-line output
- Automatic line breaking depending on maximum width
