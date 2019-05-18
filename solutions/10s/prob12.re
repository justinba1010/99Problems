/* Copyright 2019
** Justin Baum
** Problem 12
** 18 May 2019
*/

type manyList('a) =
  | One('a)
  | Many(int, 'a);

/* O(n) */
let decode = (list: list(manyList('a))) => {
  let rec repeat = (x, k, acc) =>
    switch k {
      | k when k < 0 => []
      | 0 => acc
      | k => repeat(x, k - 1, [x] @ acc)
    };
  /* Little trick recast */
  let repeat = (x,k) => repeat(x,k,[]);
  let rec aux = (list, acc) =>
    switch list {
      | [One(x), ...tail] => aux(tail, [x] @ acc)
      | [Many(k, x), ...tail] => aux(tail, repeat(x, k) @ acc)
      | [] => List.rev(acc)
    };
  aux(list, []);
};
