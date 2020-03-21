# compre
monadic comprehesions for rustlang

# usage
```rust
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
```
# There's are more!
You aren't limited to iterators. Anything that implements traits `Parametrized`, `Functor`, `FilteredFunctor`(it's optional -- you should impl this in case you want a filtering feature), `Monad`, can be used with this macros.

An example impl:
```rust
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
```
How it looks in action then:
```rust
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
```
