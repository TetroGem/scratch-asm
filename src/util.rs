use std::collections::HashMap;

use extend::ext;
use symspell::{AsciiStringStrategy, SymSpellBuilder, Verbosity};

pub fn spellcheck<'a>(
    given: &str,
    known: impl IntoIterator<Item = &'a str>,
) -> Option<symspell::Suggestion> {
    let mut symspell = SymSpellBuilder::<AsciiStringStrategy>::default()
        .max_dictionary_edit_distance(4)
        .build()
        .ok()?;

    let mut known_word_to_count = HashMap::<&str, usize>::new();

    for word in known {
        let count = known_word_to_count.entry(word).or_default();
        *count += 1;
    }

    for (word, count) in known_word_to_count {
        symspell.load_dictionary_line(&format!("{} {}", word, count), 0, 1, " ");
    }

    symspell.lookup(given, Verbosity::Top, 4).first().cloned()
}

pub trait PushGet {
    type Item;

    fn push_get(&mut self, value: Self::Item) -> &mut Self::Item;
}

impl<T> PushGet for Vec<T> {
    type Item = T;

    fn push_get(&mut self, value: Self::Item) -> &mut Self::Item {
        self.push(value);

        // SAFETY: we just pushed so there should be a last element
        self.last_mut().unwrap()
    }
}

#[ext(pub, name = SplitAtChecked)]
impl str {
    fn split_at_checked(&self, index: usize) -> Option<(&str, &str)> {
        match index > self.len() {
            true => None,
            false => Some(self.split_at(index)),
        }
    }
}
