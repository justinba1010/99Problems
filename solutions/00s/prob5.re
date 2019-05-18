/* Copyright 2019
** Justin Baum
** Problem 5
** 18 May 2019
*/

/* O(n) */
let rev = (list: list('a)) => {
  let rec aux = (list, acc) =>
    switch list {
      | [] => acc
      | [head, ...tail] => aux(tail, [head] @ acc)
    };
  aux(list, []);
};
