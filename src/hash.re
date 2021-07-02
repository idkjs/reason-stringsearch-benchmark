let name = "hash";

exception Found(int);

let start_search = (pattern, maxi, text) => {
  let hash = ref(0);
  let identical = ref(true);
  for (pos in 0 to maxi) {
    let patternchar = String.unsafe_get(pattern, pos) |> int_of_char;
    let textchar = String.unsafe_get(text, pos) |> int_of_char;
    hash := hash^ + textchar - patternchar;
    identical := identical^ && patternchar == textchar;
  };
  (hash^, identical^);
};

let search = (pattern, maxi, text, maxpos, hash, pos) => {
  let hash = ref(hash);
  let i = ref(0);
  for (pos in pos + 1 to maxpos) {
    hash :=
      hash^
      - (String.unsafe_get(text, pos - 1) |> int_of_char)
      + (String.unsafe_get(text, pos + maxi) |> int_of_char);
    if (hash^ == 0) {
      i := 0;
      while ({
               if (i^ == maxi) {
                 raise(Found(pos));
               };
               String.unsafe_get(text, pos + i^)
               == String.unsafe_get(pattern, i^);
             }) {
        incr(i);
      };
    };
  };
};

let find_all = (pattern, text) => {
  let maxi = String.length(pattern) - 1;
  let maxpos = String.length(text) - maxi - 1;
  let (hash, identical) = start_search(pattern, maxi, text);
  let count = if (identical) {1} else {0};
  let rec loop = (count, hash, pos) =>
    switch (search(pattern, maxi, text, maxpos, hash, pos)) {
    | exception (Found(pos)) => loop(count + 1, 0, pos)
    | () => count
    };
  loop(count, hash, 0);
};
