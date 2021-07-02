# Benchmark of string searching algorithms in OCaml

# Note

original repp is https://github.com/314eter/ocaml-stringsearch-benchmark.

I could not push the fork because of some git issue. Go see the original in ocaml syntax

Also, the python script doesnt work from here for some reason so, again, go see the https://github.com/314eter/ocaml-stringsearch-benchmark where it does work!

## Usage

```sh
esy install
./download.sh
esy test > benchmark.csv
./plot.py
```

## Algorithms

* Naive brute-force algorithm
* Hash algorithm: Rabin-Karp with the sum of the characters as hash sum
* Horspool algorithm
* Knuth-Morris-Pratt algorithm
* Boyer-Moore algorithm
* Twoway algorithm

## Benchmarks

* A copy of _The Picture of Dorian Gray_
* A list of Wikipedia articles related to functional programming
* A string of random characters
* Part of the human genome
* Searching for `AAAA...AAAB` in `AAAA...AAAA`
* Searching for `BAAA...AAAA` in `AAAA...AAAA`
* Searching for `ABBB...BBBC` in `BBBB...BBBB`
* Searching for `BBBB...BBAC` in `BBBB...BBBB`

## Results

![benchmark](benchmark.png)
