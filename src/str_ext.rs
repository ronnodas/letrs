use std::str::Chars;

use crate::PrintDirection;

pub(crate) trait StrExt {
    fn last(&self) -> Option<char>;
    fn bidi_chars(&self, direction: PrintDirection) -> BidiChars<'_>;
}

impl StrExt for str {
    fn last(&self) -> Option<char> {
        self.chars().next_back()
    }

    fn bidi_chars(&self, direction: PrintDirection) -> BidiChars<'_> {
        BidiChars::new(self, direction)
    }
}

pub(crate) struct BidiChars<'a> {
    chars: Chars<'a>,
    direction: PrintDirection,
}

impl<'a> BidiChars<'a> {
    fn new(string: &'a str, direction: PrintDirection) -> Self {
        let chars = string.chars();
        Self { chars, direction }
    }
}

impl Iterator for BidiChars<'_> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        match self.direction {
            PrintDirection::LeftToRight => self.chars.next(),
            PrintDirection::RightToLeft => self.chars.next_back(),
        }
    }
}

impl DoubleEndedIterator for BidiChars<'_> {
    fn next_back(&mut self) -> Option<Self::Item> {
        match self.direction {
            PrintDirection::LeftToRight => self.chars.next_back(),
            PrintDirection::RightToLeft => self.chars.next(),
        }
    }
}
