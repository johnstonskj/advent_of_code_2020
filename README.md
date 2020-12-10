# AOC 2020

This repository contains my [Advent of Code](https://adventofcode.com/2020) solutions for 2020. As with last year I’ll be using [Racket](https://racket-lang.org) for all the solutions; it’s still my favorite small (it’s not *really* that small, but it *feels* that way) language and ideal for these kinds of problems.

## Goals

My goal is to have fun, this means:

* if I fall behind I'm not going to stress to catch up,
* not trying to impress anyone, but do have friends taking part:
  * [swaits](https://git.sr.ht/~swaits/aoc2020/tree/master) in [Rust](https://www.rust-lang.org/)
  * [Jenn](https://github.com/jenndox/AoC) in [Rust](https://www.rust-lang.org/)
  * and a number of colleagues without public repos,
* not optimizing for speed or other efficiency,
* but, *maybe* for readability,
* will try to make it idiomatic Racket and modular although I will likely fail,
* it would be nice if it's elegant, but *not* baroque,

## Files

The following are the day specific *model/logic* files created. I have omitted the `day_n.rkt` and `day_n_input.txt` files as these are the driver and input for each day.

1. `data.rkt` data loading basics, and `expenses.rkt` calculations.
2. `passwords.rkt` check password policies.
3. `tree-map.rkt` skiing with trees.
4. `passports.rkt` check passport validity.
5. `airplane.rkt` find a seat on the airplane.
6. `customs.rkt` customs forms.
7. `bags.rkt` bags within bags, within bags.
8. `boot-code.rkt` debug simple code execution.
9. `xmas-cypher.rkt` decode the cypher weakness!
