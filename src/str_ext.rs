pub(crate) trait StrExt {
    fn last(&self) -> Option<char>;
}

impl StrExt for str {
    fn last(&self) -> Option<char> {
        self.chars().next_back()
    }
}
