let name = "naive";

exception Found(int);

let search = (pattern, patternlength, text, maxpos, pos) => {
  let i = ref(0);
  for (pos in pos to maxpos) {
    i := 0;
    while (String.unsafe_get(text, pos + i^)
           == String.unsafe_get(pattern, i^)) {
      incr(i);
      if (i^ == patternlength) {
        raise(Found(pos));
      };
    };
  };
};

let find_all = (pattern, text) => {
  let patternlength = String.length(pattern);
  let maxpos = String.length(text) - patternlength;
  let rec loop = (count, pos) =>
    switch (search(pattern, patternlength, text, maxpos, pos)) {
    | exception (Found(pos)) => loop(count + 1, pos + 1)
    | () => count
    };
  loop(0, 0);
};
