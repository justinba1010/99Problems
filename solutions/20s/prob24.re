/* Copyright 2019
** Justin Baum
** Problem 24
** 29 May 2019
*/

exception Fatal;

/* O(n) */
let range = (i: int, j: int) => {
  let rec aux = (acc, i, j) =>
    switch (i ,j) {
      | (i, j) when i == j => [i] @ acc
      | (i, j) => aux([i] @ acc, i + 1, j)
    };
    switch (i, j) {
      | (i, j) when i > j => aux([],j, i)
      | (i, j) => List.rev(aux([], i, j))
    };
};

/* O(kn) */
let randSelect = (k: int, list) => {
  let rec extract = (list, index, acc) =>
    switch (list, index) {
      | ([], _index) => raise(Fatal)
      | ([head, ...tail], index) when index == 0 => ((List.rev(acc) @ tail, head))
      | ([head, ...tail], index) => extract(tail, index - 1, [head] @ acc)
    };
  let rec aux = (list, k, acc) =>
    switch (k, List.length(list)) {
      | (0, _lenght) => acc
      | (k, length) when length > 0 =>
        switch (extract(list, Random.int(length), [])) {
          | (newlist, elem) => aux(newlist, k - 1, [elem] @ acc)
        }
      | (_k, _length) => acc
    };
  aux(list, k, []);
};

/* O(kn) */
let lotto = (m, n) => range(1, m) |> randSelect(n);
