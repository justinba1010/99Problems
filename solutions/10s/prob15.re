/* Copyright 2019
** Justin Baum
** Problem 15
** 18 May 2019
*/

/* O(n) */
let replicate = (list: list('a)) => {
  let rec repeat = (x, k, acc) =>
    switch k {
      | k when k < 0 => []
      | 0 => acc
      | k => repeat(x, k - 1, [x] @ acc)
    };
  let repeat = (x,k) => repeat(x,k,[]);
  let rec aux = (list, acc, k) =>
    switch list {
      | [head, ...tail] => aux(tail, repeat(head, k) @ acc, k)
      | [] => List.rev(acc)
    };
  aux(list, []);
};
