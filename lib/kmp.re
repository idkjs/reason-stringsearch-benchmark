let name = "kmp";

exception Found(int);

let search = (pattern, patternlength, text, textlength, table, k, i) => {
  let k = ref(k);
  let i = ref(i);
  while (k^ < textlength) {
    while (i^ >= 0
           && String.unsafe_get(text, k^) !== String.unsafe_get(pattern, i^)) {
      i :=
        (
          if (i^ == 0) {
            (-1);
          } else {
            Array.unsafe_get(table, i^);
          }
        );
    };
    incr(k);
    incr(i);
    if (i^ == patternlength) {
      raise(Found(k^ - patternlength));
    };
  };
  i^;
};

let init = (pattern, patternlength) => {
  let table = Array.make(patternlength, 0);
  let k = ref(1)
  and i = ref(0);
  while (k^ < patternlength) {
    if (String.unsafe_get(pattern, k^) !== String.unsafe_get(pattern, i^)) {
      Array.unsafe_set(table, k^, i^);
      i :=
        (
          if (i^ == 0) {
            (-1);
          } else {
            Array.unsafe_get(table, i^);
          }
        );
      while (i^ >= 0
             && String.unsafe_get(pattern, k^)
             !== String.unsafe_get(pattern, i^)) {
        i :=
          (
            if (i^ == 0) {
              (-1);
            } else {
              Array.unsafe_get(table, i^);
            }
          );
      };
    } else {
      Array.unsafe_set(
        table,
        k^,
        if (i^ == 0) {
          (-1);
        } else {
          Array.unsafe_get(table, i^);
        },
      );
    };
    incr(k);
    incr(i);
  };
  Array.unsafe_set(table, 0, i^);
  table;
};

let find_all = pattern => {
  let patternlength = String.length(pattern);
  let table = init(pattern, patternlength);
  text => {
    let textlength = String.length(text);
    let rec loop = (count, k, i) =>
      switch (search(pattern, patternlength, text, textlength, table, k, i)) {
      | exception (Found(pos)) =>
        loop(count + 1, pos + patternlength, Array.unsafe_get(table, 0))
      | _ => count
      };
    loop(0, 0, 0);
  };
};
