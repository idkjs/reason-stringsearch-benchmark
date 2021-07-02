let name = "boyermoore";

let init_bc = (pattern, maxi) => {
  let table = Array.make(256, -1);
  for (i in 0 to maxi) {
    Array.unsafe_set(table, String.unsafe_get(pattern, i) |> int_of_char, i);
  };
  table;
};

let init_suffixes = (pattern, maxi) => {
  let suffixes = Array.make(maxi + 1, maxi + 1);
  let left = ref(maxi);
  let right = ref(maxi);
  for (i in maxi - 1 downto 0) {
    let l = Array.unsafe_get(suffixes, i + maxi - right^);
    if (i > left^ && l < i - left^) {
      Array.unsafe_set(suffixes, i, l);
    } else {
      if (i < left^) {
        left := i;
      };
      right := i;
      while (left^ >= 0
             && String.unsafe_get(pattern, left^)
             == String.unsafe_get(pattern, left^ + maxi - right^)) {
        decr(left);
      };
      Array.unsafe_set(suffixes, i, right^ - left^);
    };
  };
  suffixes;
};

let init_gs = (pattern, maxi) => {
  let table = Array.make(maxi + 1, maxi + 1);
  let suffixes = init_suffixes(pattern, maxi);
  let left = ref(0);
  for (i in maxi downto 0) {
    if (Array.unsafe_get(suffixes, i) == i + 1) {
      while (left^ < maxi - i) {
        if (Array.unsafe_get(table, left^) == maxi + 1) {
          Array.unsafe_set(table, left^, maxi - i);
        };
        incr(left);
      };
    };
  };
  for (i in 0 to maxi - 1) {
    Array.unsafe_set(table, maxi - Array.unsafe_get(suffixes, i), maxi - i);
  };
  table;
};

let search = (pattern, maxi, text, maxpos, table_bc, table_gs, pos) => {
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
      let shift_bc =
        i^
        - Array.unsafe_get(
            table_bc,
            String.unsafe_get(text, pos^ + i^) |> int_of_char,
          );
      let shift_gs = Array.unsafe_get(table_gs, i^);
      pos := pos^ + max(shift_bc, shift_gs);
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
  let table_bc = init_bc(pattern, maxi);
  let table_gs = init_gs(pattern, maxi);
  text => {
    let maxpos = String.length(text) - maxi - 1;
    let rec loop = (count, pos) =>
      switch (search(pattern, maxi, text, maxpos, table_bc, table_gs, pos)) {
      | Some(pos) => loop(count + 1, pos + Array.unsafe_get(table_gs, 0))
      | None => count
      };
    loop(0, 0);
  };
};
