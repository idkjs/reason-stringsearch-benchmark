let name = "twoway";

let rec substrings_match = (str1, str2, pos1, pos2, length) =>
  switch (length) {
  | 0 => true
  | _ when str1.[pos1] !== str2.[pos2] => false
  | _ => substrings_match(str1, str2, pos1 + 1, pos2 + 1, length - 1)
  };

let max_suffix = (~lt, pattern, patternlength) => {
  let rec loop = (maxpos, respos, reslen, period) =>
    if (respos + reslen == patternlength) {
      (maxpos, period);
    } else {
      let a' = pattern.[maxpos + reslen];
      let a = pattern.[respos + reslen];
      if (lt(a, a')) {
        let respos = respos + reslen + 1;
        loop(maxpos, respos, 0, respos - maxpos);
      } else if (a !== a') {
        loop(respos, respos + 1, 0, 1);
      } else if (reslen + 1 == period) {
        loop(maxpos, respos + reslen + 1, 0, period);
      } else {
        loop(maxpos, respos, reslen + 1, period);
      };
    };
  loop(0, 1, 0, 1);
};

let critical_position = (text, length) =>
  switch (max_suffix((<), text, length), max_suffix((>), text, length)) {
  | ((m1, _) as mp1, (m2, _)) when m1 > m2 => mp1
  | (_, mp2) => mp2
  };

let search_exact =
    (pattern, patternlength, text, maxpos, critical, period, skip, pos) => {
  let pos = ref(pos);
  let skip = ref(skip);
  let i = ref(0);
  while (pos^ <= maxpos && i^ >= 0) {
    i := max(critical, skip^);
    while (i^ < patternlength
           && String.unsafe_get(text, pos^ + i^)
           == String.unsafe_get(pattern, i^)) {
      incr(i);
    };
    if (i^ < patternlength) {
      pos := pos^ + max(i^ - critical + 1, skip^ - period + 1);
      skip := 0;
    } else {
      i := critical - 1;
      while (i^ >= 0
             && String.unsafe_get(text, pos^ + i^)
             == String.unsafe_get(pattern, i^)) {
        decr(i);
      };
      if (i^ >= 0) {
        pos := pos^ + period;
        skip := patternlength - period;
      };
    };
  };
  if (i^ < 0) {
    Some(pos^);
  } else {
    None;
  };
};

let search_inexact =
    (pattern, patternlength, text, maxpos, critical, period, _, pos) => {
  let pos = ref(pos);
  let i = ref(0);
  while (pos^ <= maxpos && i^ >= 0) {
    i := critical;
    while (i^ < patternlength
           && String.unsafe_get(text, pos^ + i^)
           == String.unsafe_get(pattern, i^)) {
      incr(i);
    };
    if (i^ < patternlength) {
      pos := pos^ + i^ - critical + 1;
    } else {
      i := critical - 1;
      while (i^ >= 0
             && String.unsafe_get(text, pos^ + i^)
             == String.unsafe_get(pattern, i^)) {
        decr(i);
      };
      if (i^ >= 0) {
        pos := pos^ + period;
      };
    };
  };
  if (i^ < 0) {
    Some(pos^);
  } else {
    None;
  };
};

let find_all = pattern => {
  let patternlength = String.length(pattern);
  let (critical, period) = critical_position(pattern, patternlength);
  let (search, period) =
    if (2 * critical < patternlength) {
      if (substrings_match(pattern, pattern, 0, period, critical)) {
        (search_exact, period);
      } else {
        (search_inexact, patternlength - critical + 1);
      };
    } else {
      (search_inexact, critical + 1);
    };
  text => {
    let maxpos = String.length(text) - patternlength;
    let rec loop = (count, skip, pos) =>
      switch (
        search(
          pattern,
          patternlength,
          text,
          maxpos,
          critical,
          period,
          skip,
          pos,
        )
      ) {
      | Some(pos) => loop(count + 1, patternlength - period, pos + period)
      | None => count
      };
    loop(0, 0, 0);
  };
};
