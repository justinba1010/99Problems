/* Copyright 2019
** Justin Baum
** Problem 13
** 18 May 2019
*/

type manyList('a) =
  | One('a)
  | Many(int, 'a);

/* O(n) */
let encode = (list: list('a)) => {
  let rec aux = (list, acc) => {
    switch (list, acc) {
      | ([head, ...tail], [One(x), ...tail2]) when x == head => aux(tail, [Many(2, x)] @ tail2)
      | ([head, ...tail], [Many(k, x), ...tail2]) when x == head => aux(tail, [Many(k+1, x)] @ tail2)
      | ([head, ...tail], _acc) => aux(tail, [One(head)] @ acc)
      | ([], _acc) => List.rev(acc)
    }
  };
  aux(list, []);
};
