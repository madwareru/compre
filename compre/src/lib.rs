use std::fmt::Debug;
use proc_monadde_macro::*;

pub trait Parametrized<T>{}
pub trait Functor<T: Sized+Copy, O: Sized+Copy+Default> {
    type UnderlyingO: Parametrized<O>;
    fn map<F: Fn(T) -> O>(&self, f: F) -> Self::UnderlyingO;
}
pub trait Monad<T: Sized+Copy, O: Sized+Copy+Default> : Functor<T, O> {
    fn flat_map<F: Fn(T) -> Self::UnderlyingO>(&self, f: F) -> Self::UnderlyingO;
}
pub trait FilteredFunctor<T: Sized+Copy, O: Sized+Copy+Default> : Functor<T, O> {
    fn filter_map<F: Fn(T) -> Option<O>>(&self, f: F) -> Self::UnderlyingO;
}

#[derive(PartialEq, Copy, Clone, Debug)]
pub enum Maybe<T: Sized+Copy> {
    Nothing,
    Just(T)
}

impl<T: Sized+Copy> Parametrized<T> for Maybe<T>{}
impl<T: Sized+Copy, O: Sized+Copy+Default> Functor<T, O> for Maybe<T> {
    type UnderlyingO = Maybe<O>;
    fn map<F: Fn(T) -> O>(&self, f: F) -> Self::UnderlyingO {
        match self {
            Maybe::Nothing => Maybe::Nothing,
            Maybe::Just(x) => Maybe::Just(f(*x)),
        }
    }
}
impl<T: Sized+Copy, O: Sized+Copy+Default> Monad<T, O> for Maybe<T> {
    fn flat_map<F: Fn(T) -> Self::UnderlyingO>(&self, f: F) -> Self::UnderlyingO {
        match self {
            Maybe::Nothing => Maybe::Nothing,
            Maybe::Just(x) => f(*x),
        }
    }
}
impl<T: Sized+Copy, O: Sized+Copy+Default> FilteredFunctor<T, O> for Maybe<T> {
    fn filter_map<F: Fn(T) -> Option<O>>(&self, f: F) -> Self::UnderlyingO {
        match self {
            Maybe::Nothing => Maybe::Nothing,
            Maybe::Just(x) => match f(*x) {
                None => Maybe::Nothing,
                Some(xx) => Maybe::Just(xx),
            }
        }
    }
}

impl<T: Sized+Copy> Parametrized<T> for Vec<T> {}
impl<T: Sized+Copy, O: Sized+Copy+Default> Functor<T, O> for Vec<T> {
    type UnderlyingO = Vec<O>;
    fn map<F: Fn(T) -> O>(&self, f: F) -> Self::UnderlyingO {
        let mut out_vec = Vec::with_capacity(self.capacity());
        for &item in self.iter() {
            out_vec.push(f(item));
        }
        out_vec
    }
}
impl<T: Sized+Copy, O: Sized+Copy+Default> Monad<T, O> for Vec<T> {
    fn flat_map<F: Fn(T) -> Self::UnderlyingO>(&self, f: F) -> Self::UnderlyingO {
        let mut out_vec = Vec::with_capacity(self.capacity());
        for &item in self.iter() {
            let mut foo_result = f(item);
            out_vec.append(&mut foo_result);
        }
        out_vec
    }
}
impl<T: Sized+Copy, O: Sized+Copy+Default> FilteredFunctor<T, O> for Vec<T> {
    fn filter_map<F: Fn(T) -> Option<O>>(&self, f: F) -> Self::UnderlyingO {
        let mut out_vec = Vec::with_capacity(self.capacity());
        for &item in self.iter() {
            match f(item) {
                None => {},
                Some(x) => out_vec.push(x),
            }
        }
        out_vec
    }
}

define_monadde_macro!();

macro_rules! compre {
    ($ex:expr; $($id:ident <- $monad:expr),+) => {
        monadde! {
            $($monad => $id |>)+
            $ex
        }
    };
    ($ex:expr; $($id:ident <- $monad:expr),+; $cond: expr) => {
        monadde! {
            $($monad => $id |>)+
            when $cond => $ex
        }
    }
}

macro_rules! hx_do {
    ($($id:ident <- $monad:expr),+; $ex:expr) => {
        monadde! {
            $($monad => $id |>)+
            $ex
        }
    };
    ($($id:ident <- $monad:expr),+; barrier: $cond: expr; $ex:expr) => {
        monadde! {
            $($monad => $id |>)+
            when $cond => $ex
        }
    }
}

#[cfg(test)]
mod test{
    use crate::{Monad, Functor};
    use crate::Maybe::{Nothing, Just};

    #[test]
    fn test_simple() {
        let tripples = compre! {
            (x, y, z);
            x <- 1..=17,
            y <- 1..=17,
            z <- 1..=17;
            x*x + y*y == z*z
        }.collect::<Vec<_>>();

        assert_eq!(
            vec![(3,  4,  5), ( 4,  3,  5), (5, 12, 13), ( 6,  8, 10),
                 (8,  6, 10), ( 8, 15, 17), (9, 12, 15), (12,  5, 13),
                 (12, 9, 15), (15,  8, 17)
            ],
            tripples
        );

        let tripples = hx_do! {
            x <- 1..=17,
            y <- 1..=17,
            z <- 1..=17;
            barrier: x*x + y*y == z*z;
            (x, y, z)
        }.collect::<Vec<_>>();

        assert_eq!(
            vec![(3,  4,  5), ( 4,  3,  5), (5, 12, 13), ( 6,  8, 10),
                 (8,  6, 10), ( 8, 15, 17), (9, 12, 15), (12,  5, 13),
                 (12, 9, 15), (15,  8, 17)
            ],
            tripples
        );

        let tripples = monadde! {
            1..=17 => x |>
            1..=17 => y |>
            1..=17 => z |>
            when x*x + y*y == z*z =>
            (x, y, z)
        }.collect::<Vec<_>>();

        assert_eq!(
            vec![(3,  4,  5), ( 4,  3,  5), (5, 12, 13), ( 6,  8, 10),
                 (8,  6, 10), ( 8, 15, 17), (9, 12, 15), (12,  5, 13),
                 (12, 9, 15), (15,  8, 17)
            ],
            tripples
        );
    }

    #[test]
    fn test_maybe() {
        let some_product =
            (1..=3).collect::<Vec<i32>>().flat_map(|a|
                (2..=3).collect::<Vec<i32>>().map(|b|
                    (a, b)));

        assert_eq!(vec![(1, 2), (1, 3), (2, 2), (2, 3), (3, 2), (3, 3)], some_product);

        let some_product = compre!{
            (a, b);
            a <- (1..=3).collect::<Vec<i32>>(),
            b <- (2..=3).collect::<Vec<i32>>()
        };

        assert_eq!(vec![(1, 2), (1, 3), (2, 2), (2, 3), (3, 2), (3, 3)], some_product);

        let some_product = compre! {
            (a, b);
            a <- (1..=3),
            b <- (2..=3)
        }.collect::<Vec<_>>();

        assert_eq!(vec![(1, 2), (1, 3), (2, 2), (2, 3), (3, 2), (3, 3)], some_product);

        let ololo = monadde! {
            Just(2)        => a |>
            Nothing::<i32> => b |>
            Just(1)        => c |>
            a * b + c
        };
        assert_eq!(Nothing, ololo);

        let ololo = compre! {
            a * b + c;
            a <- Just(2),
            b <- Nothing::<i32>,
            c <- Just(1)
        };
        assert_eq!(Nothing, ololo);

        let some_product2 =
            (1..=3).collect::<Vec<i32>>().flat_map(|a|
                (2..=3).collect::<Vec<i32>>().map(|b|
                    a * b));

        assert_eq!(vec![2, 3, 4, 6, 6, 9], some_product2);

        let x = Just(5).flat_map(|a|
            Just(10).map(|b|
                a + b));
        assert_eq!(Just(15), x);

        let x = Just(5).flat_map(|a|
            Nothing.map(|b: i32|
                a + b));
        assert_eq!(Nothing, x);

        let x = Nothing.flat_map(|a: i32|
            Just(10).map(|b|
                a + b));
        assert_eq!(Nothing, x);

        let x = Nothing.flat_map(|a: i32|
            Nothing.map(|b: i32|
                a + b));
        assert_eq!(Nothing, x);
    }
}