// Console.log("Running Test Program:");
// let () = print_endline(Lib.Util.hello());
open Batteries;
open Lib;
module type Algorithm = {
  let name: string;
  let find_all: (string, string) => int;
};

type benchmark = {
  name: string,
  text: string,
  patterns: list(string),
};

let () = Random.init(0);

let randomstring = n => String.init(n, _ => Random.int(256) |> char_of_int);

let patternlengths = [
  1,
  2,
  3,
  4,
  5,
  6,
  7,
  8,
  9,
  10,
  12,
  15,
  20,
  30,
  45,
  65,
  90,
];

let dorian = {
  name: "dorian",
  text: String.repeat(input_file("doriangray.txt"), 100),
  patterns:
    List.map(
      String.left(
        "I have worshipped you with far more romance of feeling than a man usually gives to a friend",
      ),
      patternlengths,
    ),
};

let wikipedia = {
  name: "wikipedia",
  text: input_file("wikipedia.txt"),
  patterns:
    List.map(
      String.left(
        "Type inference is a technique which allows the compiler to determine from the code the type of each variable and symbol used in the program",
      ),
      patternlengths,
    ),
};

let random = {
  name: "random",
  text: randomstring(100000000),
  patterns: List.map(randomstring, patternlengths),
};

let dna = {
  name: "dna",
  text: input_file("dna.txt"),
  patterns:
    List.map(
      String.left(
        "CACCTAAAATGTAATCTTACACAGGCTGTAGATTATATCTCTGTTTCCAGGAAGCCCCGTGTGTTGCTTTGTGTGTGCATCAGTCTCTCG",
      ),
      patternlengths,
    ),
};

let aab = {
  name: "aab",
  text: String.repeat("A", 10000000),
  patterns: List.map(n => String.repeat("A", n - 1) ++ "B", patternlengths),
};

let baa = {
  name: "baa",
  text: String.repeat("A", 10000000),
  patterns: List.map(n => "B" ++ String.repeat("A", n - 1), patternlengths),
};

let abbc = {
  name: "abbc",
  text: String.repeat("B", 10000000),
  patterns:
    List.map(
      n => "A" ++ String.repeat("B", n - 2) ++ "C",
      List.tl(patternlengths),
    ),
};

let bbac = {
  name: "bbac",
  text: String.repeat("B", 10000000),
  patterns:
    List.map(
      n => String.repeat("B", n - 2) ++ "AC",
      List.tl(patternlengths),
    ),
};

let benchmarks = [dorian, wikipedia, random, dna, aab, baa, abbc, bbac];

let algorithms: list(module Algorithm) = (
  [
    (module Naive),
    (module Hash),
    (module Horspool),
    (module Kmp),
    (module Boyermoore),
    (module Twoway),
  ]:
    list(module Algorithm)
);

let time = (find_all, pattern, text) => {
  let start_time = Sys.time();
  let count = find_all(pattern, text);
  let search_time = Sys.time() -. start_time;
  (search_time, count);
};

let () = {
  Printf.printf("algorithm\ttext\tpattern\tsearch_time\n");
  benchmarks
  |> List.iter @@
  (
    benchmark =>
      benchmark.patterns
      |> List.iter @@
      (
        pattern => {
          let count = ref(0);
          algorithms
          |> List.iter @@
          (
            (module Algorithm: Algorithm) => {
              let (search_time, found) =
                time(Algorithm.find_all, pattern, benchmark.text);
              if (count^ == 0) {
                count := found;
              };
              if (found !== count^) {
                Printf.eprintf(
                  "Wrong count: %s\t%s\t%d\n",
                  Algorithm.name,
                  benchmark.name,
                  String.length(pattern),
                );
              } else {
                Printf.printf(
                  "%s\t%s\t%d\t%f\n",
                  Algorithm.name,
                  benchmark.name,
                  String.length(pattern),
                  search_time,
                );
              };
              flush(stdout);
            }
          );
        }
      )
  );
};
