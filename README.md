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
