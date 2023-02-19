use std::marker::PhantomData;
use std::ops::Index;

use quote::format_ident;

#[derive(Debug, Clone, Copy)]
pub struct Generator<T, A> {
    index: usize,
    alphabet: A,
    _symbol: PhantomData<T>,
}

pub trait Alphabet<T>: Index<usize, Output = T> {
    fn len(&self) -> usize;
}

impl<T, const N: usize> Alphabet<T> for [T; N] {
    fn len(&self) -> usize {
        N
    }
}

impl<A: Alphabet<T>, T> Generator<T, A> {
    pub fn new(alphabet: A) -> Self {
        Self {
            index: 0,
            alphabet,
            _symbol: PhantomData,
        }
    }
}

impl Default for Generator<char, [char; 3]> {
    fn default() -> Self {
        Self::new(['A', 'B', 'C'])
    }
}

fn decode(value: usize, numbers: usize, n: usize) -> Vec<usize> {
    let mut result = vec![];

    let mut number = value;
    for _ in 0..numbers {
        result.push(number % n);
        number /= n;
    }

    result.reverse();

    result
}

impl<A: Alphabet<char>> Iterator for Generator<char, A> {
    type Item = syn::Ident;

    fn next(&mut self) -> Option<Self::Item> {
        let mut length = 0;
        let mut first_index = 0;

        while first_index <= self.index {
            length += 1;
            first_index += self.alphabet.len().pow(length as u32);
        }

        first_index -= self.alphabet.len().pow(length as u32);

        let mut result = String::new();

        for index in decode(self.index - first_index, length, self.alphabet.len()) {
            result.push(self.alphabet[index]);
        }

        self.index += 1;

        Some(format_ident!("{}", result))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;

    #[test]
    fn test_decode() {
        assert_eq!(decode(0, 3, 2), vec![0, 0, 0]);
        assert_eq!(decode(1, 3, 2), vec![0, 0, 1]);
        assert_eq!(decode(2, 3, 2), vec![0, 1, 0]);
        assert_eq!(decode(3, 3, 2), vec![0, 1, 1]);
        assert_eq!(decode(4, 3, 2), vec![1, 0, 0]);

        assert_eq!(decode(0, 2, 3), vec![0, 0]);
        assert_eq!(decode(1, 2, 3), vec![0, 1]);
        assert_eq!(decode(2, 2, 3), vec![0, 2]);

        assert_eq!(decode(3, 2, 3), vec![1, 0]);
        assert_eq!(decode(4, 2, 3), vec![1, 1]);
        assert_eq!(decode(5, 2, 3), vec![1, 2]);

        assert_eq!(decode(6, 2, 3), vec![2, 0]);
        assert_eq!(decode(7, 2, 3), vec![2, 1]);
        assert_eq!(decode(8, 2, 3), vec![2, 2]);
    }

    #[test]
    fn test_generator() {
        let generator = Generator::new(['A', 'B', 'C']);

        assert_eq!(
            generator.take(21).collect::<Vec<_>>(),
            vec![
                format_ident!("A"),
                format_ident!("B"),
                format_ident!("C"),
                //
                format_ident!("AA"),
                format_ident!("AB"),
                format_ident!("AC"),
                format_ident!("BA"),
                format_ident!("BB"),
                format_ident!("BC"),
                format_ident!("CA"),
                format_ident!("CB"),
                format_ident!("CC"),
                //
                format_ident!("AAA"),
                format_ident!("AAB"),
                format_ident!("AAC"),
                format_ident!("ABA"),
                format_ident!("ABB"),
                format_ident!("ABC"),
                format_ident!("ACA"),
                format_ident!("ACB"),
                format_ident!("ACC"),
            ]
        )
    }
}
