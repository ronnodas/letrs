use std::slice::Iter;

use crate::font::PrintDirection;

pub(crate) trait BStrExt {
    fn bidi_chars(&self, direction: PrintDirection) -> BidiBytes<'_>;
}

impl BStrExt for [u8] {
    fn bidi_chars(&self, direction: PrintDirection) -> BidiBytes<'_> {
        BidiBytes::new(self, direction)
    }
}

pub(crate) struct BidiBytes<'a> {
    bytes: Iter<'a, u8>,
    direction: PrintDirection,
}

impl<'a> BidiBytes<'a> {
    fn new(string: &'a [u8], direction: PrintDirection) -> Self {
        let bytes = string.iter();
        Self { bytes, direction }
    }
}

impl Iterator for BidiBytes<'_> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        match self.direction {
            PrintDirection::LeftToRight => self.bytes.next().copied(),
            PrintDirection::RightToLeft => self.bytes.next_back().copied(),
        }
    }
}

impl DoubleEndedIterator for BidiBytes<'_> {
    fn next_back(&mut self) -> Option<Self::Item> {
        match self.direction {
            PrintDirection::LeftToRight => self.bytes.next_back().copied(),
            PrintDirection::RightToLeft => self.bytes.next().copied(),
        }
    }
}
