//! Collection of `.flf` fonts to be used by `letrs`

macro_rules! fonts {
    ($($name:ident => $file_name:expr,)*) => {

        /// Included fonts
        #[derive(Debug, Clone, Copy)]
        #[non_exhaustive]
        pub enum FontFile {
            $(
                #[doc = concat!("Font `", $file_name, ".flf`")]
                $name,
            )*
        }

        impl FontFile {
            /// An array with all the included font strings
            pub const ALL: [Self; const{0 $(+ {_ = $file_name; 1} )*}] = [$(Self::$name),*];

            /// Returns the contents of a font file
            #[must_use]
            pub const fn as_bytes(&self) -> &'static [u8] {
                match self {
                    $(Self::$name => include_bytes!(concat!("../fonts/", $file_name, ".flf")),)*
                }
            }

            /// Returns the file stem
            #[must_use]
            pub const fn name(&self) -> &'static str {
                match self {
                    $(Self::$name => $file_name,)*
                }
            }

            /// Matches a font name to an included font
            #[must_use]
            pub fn from_name(name: &str) -> Option<Self> {
                match name {
                    $($file_name => Some(Self::$name),)*
                    _ => None,
                }
            }

        }

    };
}

fonts! {
    _5x7 => "5x7",
    _5x8 => "5x8",
    _6x9 => "6x9",
    Banner => "banner",
    Big => "big",
    Block => "block",
    Bubble => "bubble",
    Chartr => "chartr",
    Chartri => "chartri",
    Clb6x10 => "clb6x10",
    Clb8x8 => "clb8x8",
    Clb8x10 => "clb8x10",
    Cli8x8 => "cli8x8",
    Clr4x6 => "clr4x6",
    Clr5x6 => "clr5x6",
    Clr5x8 => "clr5x8",
    Clr5x10 => "clr5x10",
    Clr6x6 => "clr6x6",
    Clr6x8 => "clr6x8",
    Clr6x10 => "clr6x10",
    Clr7x8 => "clr7x8",
    Clr7x10 => "clr7x10",
    Clr8x8 => "clr8x8",
    Clr8x10 => "clr8x10",
    Cour => "cour",
    Courb => "courb",
    Courbi => "courbi",
    Couri => "couri",
    Digital => "digital",
    Helv => "helv",
    Helvb => "helvb",
    Helvbi => "helvbi",
    Helvi => "helvi",
    Ivrit => "ivrit",
    Lean => "lean",
    Mini => "mini",
    Mnemonic => "mnemonic",
    Sbook => "sbook",
    Sbookb => "sbookb",
    Sbookbi => "sbookbi",
    Sbooki => "sbooki",
    Script => "script",
    Shadow => "shadow",
    Slant => "slant",
    Small => "small",
    Smscript => "smscript",
    Smshadow => "smshadow",
    Smslant => "smslant",
    Standard => "standard",
    Term => "term",
    Times => "times",
    Xchartr => "xchartr",
    Xchartri => "xchartri",
    Xcour => "xcour",
    Xcourb => "xcourb",
    Xcourbi => "xcourbi",
    Xcouri => "xcouri",
    Xhelv => "xhelv",
    Xhelvb => "xhelvb",
    Xhelvbi => "xhelvbi",
    Xhelvi => "xhelvi",
    Xsbook => "xsbook",
    Xsbookb => "xsbookb",
    Xsbookbi => "xsbookbi",
    Xsbooki => "xsbooki",
    Xtimes => "xtimes",
}
