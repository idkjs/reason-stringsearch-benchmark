Js.log("Running Test Program:");
// let () = print_endline(Lib.Util.hello());
// // open Batteries;
// open Lib;
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

let stringRepeat = (s,n) =>
  s |> Array.make(n) |> Array.to_list |> String.concat("");

let input_file = (str, encoding) => Node.Fs.readFileSync(str, encoding);

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
let left = (s, len) =>
  if (len >= String.length(s)) {
    s;
  } else {
    String.sub(s, 0, len);
  };
let dorian = {
  name: "dorian",
  text: stringRepeat( input_file("doriangray.txt", `ascii),100),
  patterns:
    List.map(
      left(
        "I have worshipped you with far more romance of feeling than a man usually gives to a friend",
      ),
      patternlengths,
    ),
};

let wikipedia = {
  name: "wikipedia",
  text: input_file("wikipedia.txt", `ascii),
  patterns:
    List.map(
      left(
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
  text: input_file("dna.txt", `ascii),
  patterns:
    List.map(
      left(
        "CACCTAAAATGTAATCTTACACAGGCTGTAGATTATATCTCTGTTTCCAGGAAGCCCCGTGTGTTGCTTTGTGTGTGCATCAGTCTCTCG",
      ),
      patternlengths,
    ),
};

let aab = {
  name: "aab",
  text: stringRepeat("A", 10000000),
  patterns: List.map(n => stringRepeat("A", n - 1) ++ "B", patternlengths),
};

let baa = {
  name: "baa",
  text: stringRepeat("A", 10000000),
  patterns: List.map(n => "B" ++ stringRepeat("A", n - 1), patternlengths),
};

let abbc = {
  name: "abbc",
  text: stringRepeat("B", 10000000),
  patterns:
    List.map(
      n => "A" ++ stringRepeat("B", n - 2) ++ "C",
      List.tl(patternlengths),
    ),
};

let bbac = {
  name: "bbac",
  text: stringRepeat("B", 10000000),
  patterns:
    List.map(
      n => stringRepeat("B", n - 2) ++ "AC",
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
