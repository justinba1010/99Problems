/* Copyright 2019
** Justin Baum
** Problem 23
** 18 May 2019
*/

exception Fatal;

/* O(kn) */
let randSelect = (list, k: int) => {
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
