/* Copyright 2019
** Justin Baum
** Problem 27
** 29 May 2019
*/
/*
This was copied from the solutions on the Ocaml 99 problems site, and then formatted for ReasonML with refmt.
*/

let group = (list, sizes) => {
  let initial = List.map((size) => (size, []), sizes);
  /* The core of the function. Prepend accepts a list of groups,
          each with the number of items that should be added, and
          prepends the item to every group that can support it, thus
          turning [1,a ; 2,b ; 0,c] into [ [0,x::a ; 2,b ; 0,c ];
          [1,a ; 1,x::b ; 0,c]; [ 1,a ; 2,b ; 0,c ]]

          Again, in the prolog language (for which these questions are
          originally intended), this function is a whole lot simpler.  */
  let prepend = (p, list) => {
    let emit = (l, acc) => [l, ...acc];
    let rec aux = (emit, acc) =>
      fun
      | [] => emit([], acc)
      | [(n, l) as h, ...t] => {
          let acc =
            if (n > 0) {
              emit([(n - 1, [p, ...l]), ...t], acc);
            } else {
              acc;
            };
          aux((l, acc) => emit([h, ...l], acc), acc, t);
        };
    aux(emit, [], list);
  };
  let rec aux =
    fun
    | [] => [initial]
    | [h, ...t] => List.concat(List.map(prepend(h), aux(t)));
  let all = aux(list);
  /* Don't forget to eliminate all group sets that have non-full
     groups */
  let complete = List.filter(List.for_all(((x, _)) => x == 0), all);
  List.map(List.map(snd), complete);
};
