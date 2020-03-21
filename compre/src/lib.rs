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

define_monadde_macro!();

#[macro_export]
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

#[macro_export]
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
    use crate::{Monad, Functor, FilteredFunctor, Parametrized};

    impl<T: Sized+Copy> Parametrized<T> for Option<T>{}
    impl<T: Sized+Copy, O: Sized+Copy+Default> Functor<T, O> for Option<T> {
        type UnderlyingO = Option<O>;
        fn map<F: Fn(T) -> O>(&self, f: F) -> Self::UnderlyingO {
            match self {
                None => None,
                Some(x) => Some(f(*x)),
            }
        }
    }
    impl<T: Sized+Copy, O: Sized+Copy+Default> Monad<T, O> for Option<T> {
        fn flat_map<F: Fn(T) -> Self::UnderlyingO>(&self, f: F) -> Self::UnderlyingO {
            match self {
                None => None,
                Some(x) => f(*x),
            }
        }
    }
    impl<T: Sized+Copy, O: Sized+Copy+Default> FilteredFunctor<T, O> for Option<T> {
        fn filter_map<F: Fn(T) -> Option<O>>(&self, f: F) -> Self::UnderlyingO {
            match self {
                None => None,
                Some(x) => f(*x)
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

    #[test]
    fn test_simple() {
        let expected = vec![
            ( 3,  4,  5), ( 4,  3,  5), ( 5, 12, 13), ( 6,  8, 10), ( 8,  6, 10),
            ( 8, 15, 17), ( 9, 12, 15), (12,  5, 13), (12,  9, 15), (15,  8, 17)];

        let tripples = compre! {
            (x, y, z);
            x <- 1..=17,
            y <- 1..=17,
            z <- 1..=17;
            x*x + y*y == z*z
        }.collect::<Vec<_>>();
        assert_eq!(&expected, &tripples);

        let tripples = hx_do! {
            x <- 1..=17,
            y <- 1..=17,
            z <- 1..=17;
            barrier: x*x + y*y == z*z;
            (x, y, z)
        }.collect::<Vec<_>>();
        assert_eq!(&expected, &tripples);

        let tripples = monadde! {
            1..=17 => x |>
            1..=17 => y |>
            1..=17 => z |>
            when x*x + y*y == z*z =>
            (x, y, z)
        }.collect::<Vec<_>>();
        assert_eq!(&expected, &tripples);
    }

    #[test]
    fn test_options() {
        let res = monadde! {
            Some(2)  => a |>
            Some(10) => b |>
            Some(1)  => c |>
            a * b + c
        };
        assert_eq!(Some(21), res);

        let res = hx_do! {
            a <- Some(2),
            b <- None::<i32>,
            c <- Some(1);
            a * b + c
        };
        assert_eq!(None, res);
    }
}
