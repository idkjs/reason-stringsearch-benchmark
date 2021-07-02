let name = "horspool";

let init = (pattern, maxi) => {
  let table = Array.make(256, maxi + 1);
  for (i in 0 to maxi - 1) {
    Array.unsafe_set(
      table,
      String.unsafe_get(pattern, i) |> int_of_char,
      maxi - i,
    );
  };
  table;
};

let search = (pattern, maxi, text, maxpos, table, skip, pos) => {
  let pos = ref(pos);
  let i = ref(0);
  while (pos^ <= maxpos && i^ !== (-1)) {
    i := maxi;
    while (i^ !== (-1)
           && String.unsafe_get(text, pos^ + i^)
           == String.unsafe_get(pattern, i^)) {
      decr(i);
    };
    if (i^ !== (-1)) {
      pos :=
        pos^
        + (
          if (i^ == maxi) {
            Array.unsafe_get(
              table,
              String.unsafe_get(text, pos^ + maxi) |> int_of_char,
            );
          } else {
            skip;
          }
        );
    };
  };
  if (i^ == (-1)) {
    Some(pos^);
  } else {
    None;
  };
};

let find_all = pattern => {
  let maxi = String.length(pattern) - 1;
  let table = init(pattern, maxi);
  let skip =
    Array.unsafe_get(table, String.unsafe_get(pattern, maxi) |> int_of_char);
  text => {
    let maxpos = String.length(text) - maxi - 1;
    let rec loop = (count, pos) =>
      switch (search(pattern, maxi, text, maxpos, table, skip, pos)) {
      | Some(i) => loop(count + 1, i + 1)
      | None => count
      };
    loop(0, 0);
  };
};
