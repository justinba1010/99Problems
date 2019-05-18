/* Copyright 2019
** Justin Baum
** Problem 18
** 18 May 2019
*/

/* O(n) */
let slice = (list: list('a), i: int, j: int) => {
  let rec aux = (list, acc, count) =>
    switch (list, count) {
      | ([head, ...tail], count) when count >= i && count <= j => aux(tail, [head] @ acc, count + 1)
      | ([_head, ...tail], count) => aux(tail, acc, count + 1)
      | ([], _count) => List.rev(acc)
    };
    aux(list, [], 0);
};
