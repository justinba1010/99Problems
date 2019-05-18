/* Copyright 2019
** Justin Baum
** Problem 14
** 18 May 2019
*/

/* O(n) */
let duplicate = (list: list('a)) => {
  let rec aux = (list, acc) =>
    switch list {
      | [head, ...tail] => aux(tail, [head, head] @ acc)
      | [] => List.rev(acc)
    };
  aux(list, []);
};
