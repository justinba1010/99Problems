/* Copyright 2019
** Justin Baum
** Problem 26
** 29 May 2019
*/
/*
This was copied from the solutions on the Ocaml 99 problems site, and then formatted for ReasonML with refmt.
*/

let rec extract = (k, list) =>
  if (k <= 0) {
    [[]];
  } else {
    switch list {
    | [] => []
    | [h, ...tl] =>
      let with_h = List.map((l) => [h, ...l], extract(k - 1, tl));
      let without_h = extract(k, tl);
      with_h @ without_h;
    };
  };
