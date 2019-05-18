/* Copyright 2019
** Justin Baum
** Problem 16
** 18 May 2019
*/

/* O(n) */
let drop = (list: list('a), k: int) => {
  let rec aux = (list, acc, k, i) =>
    switch (list, k, i) {
      | ([head, ...tail], k, i) when k == i => aux(tail, acc, k, 0)
      | ([head, ...tail], _k, _i) => aux(tail, [head] @ acc, k, i + 1)
      | ([], _k, _i) => List.rev(acc)
    };
  aux(list, [], k-1, 0);
};
