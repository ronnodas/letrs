pub(crate) trait StrExt {
    fn first_non_blank(&self) -> Option<(usize, char)>;
    fn last(&self) -> Option<char>;
}

impl StrExt for str {
    fn last(&self) -> Option<char> {
        self.chars().next_back()
    }

    fn first_non_blank(&self) -> Option<(usize, char)> {
        self.chars().enumerate().find(|&(_, c)| c != ' ')
    }
}
