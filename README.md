# letrs

Rust crates for parsing FIGfonts and rendering text using these fonts.

## Features

- Horizontal kerning/fitting and smushing
- Vertical fitting and smushing (seems no other rust implementation supports this!)
- Alignment for multi-line output
- Automatic line breaking depending on maximum width
- CLI tool: `cargo install letrs-cli`, then run `letrs input` (assuming you have cargo installed)

## Possible future features

- Control files
- Performance, both speed and memory usage
- TOIlet fonts
- Loading fonts from zip files
- Interactive mode in the CLI

## Non-features

- Perfectly matching `figlet` output

## Usage

From the command line:

```console
user@machine:~$ letrs "Hello, world!"
 _   _      _ _                             _     _ _ 
| | | | ___| | | ___    __      _____  _ __| | __| | |
| |_| |/ _ \ | |/ _ \   \ \ /\ / / _ \| '__| |/ _` | |
|  _  |  __/ | | (_) |   \ V  V / (_) | |  | | (_| |_|
|_| |_|\___|_|_|\___( )   \_/\_/ \___/|_|  |_|\__,_(_)
                    |/                                
```

Calling library functions in rust code:

```rust
let font = Font::standard();
println!(Renderer::new(&font).render_unbounded("Hello, world!"))
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

## Acknowledgements

Thanks to the [pyfiglet](https://github.com/pwaller/pyfiglet) project for organizing the fonts and their licenses.
