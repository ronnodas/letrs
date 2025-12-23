//! Collection of `.flf` fonts to be used by the [`letrs`](https://crates.io/crates/letrs) crate.

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
            /// An array containing all the variants
            pub const ALL: [Self; const{0 $(+ {_ = $file_name; 1} )*}] = [$(Self::$name),*];

            /// The contents of a font file
            #[must_use]
            pub const fn as_bytes(&self) -> &'static [u8] {
                match self {
                    $(Self::$name => include_bytes!(concat!("../fonts/", $file_name, ".flf")),)*
                }
            }

            /// The file stem
            #[must_use]
            pub const fn name(&self) -> &'static str {
                match self {
                    $(Self::$name => $file_name,)*
                }
            }

            /// Match a font name to an included font
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
    Fixed5x7 => "5x7",
    Fixed5x8 => "5x8",
    Fixed6x9 => "6x9",
    Banner => "banner",
    Big => "big",
    Block => "block",
    Bubble => "bubble",
    Charter => "chartr",
    CharterItalic => "chartri",
    CleanBold6x10 => "clb6x10",
    CleanBold8x8 => "clb8x8",
    CleanBold8x10 => "clb8x10",
    CleanItalic8x8 => "cli8x8",
    Clean4x6 => "clr4x6",
    Clean5x6 => "clr5x6",
    Clean5x8 => "clr5x8",
    Clean5x10 => "clr5x10",
    Clean6x6 => "clr6x6",
    Clean6x8 => "clr6x8",
    Clean6x10 => "clr6x10",
    Clean7x8 => "clr7x8",
    Clean7x10 => "clr7x10",
    Clean8x8 => "clr8x8",
    Clean8x10 => "clr8x10",
    Courier => "cour",
    CourierBold => "courb",
    CourierBoldItalic => "courbi",
    CourierItalic => "couri",
    Digital => "digital",
    Helvetica => "helv",
    HelveticaBold => "helvb",
    HelveticaBoldItalic => "helvbi",
    HelveticaItalic => "helvi",
    Ivrit => "ivrit",
    Lean => "lean",
    Mini => "mini",
    Mnemonic => "mnemonic",
    NewCenturySchoolbook => "sbook",
    NewCenturySchoolbookBold => "sbookb",
    NewCenturySchoolbookBoldItalic => "sbookbi",
    NewCenturySchoolbookItalic => "sbooki",
    Script => "script",
    Shadow => "shadow",
    Slant => "slant",
    Small => "small",
    SmScript => "smscript",
    SmShadow => "smshadow",
    SmSlant => "smslant",
    Standard => "standard",
    Terminal => "term",
    TimesBold => "times",
    XCharter => "xchartr",
    XCharterItalic => "xchartri",
    XCourier => "xcour",
    XCourierBold => "xcourb",
    XCourierBoldItalic => "xcourbi",
    XCourierItalic => "xcouri",
    XHelvetica => "xhelv",
    XHelveticaBold => "xhelvb",
    XHelveticaBoldItalic => "xhelvbi",
    XHelveticaItalic => "xhelvi",
    XNewCenturySchoolbook => "xsbook",
    XNewCenturySchoolbookBold => "xsbookb",
    XNewCenturySchoolbookBoldItalic => "xsbookbi",
    XNewCenturySchoolbookItalic => "xsbooki",
    XTimesBold => "xtimes",
}
