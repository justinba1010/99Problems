/* Copyright 2019
** Justin Baum
** Problem 3
** 18 May 2019
 */

/* O(n) */
let at = (list: list('a), k: int) => {
  let rec aux = (list, k) =>
    switch (list, k) {
      | (_, k) when k < 0 => None /* Give a None for negative indices */
      | ([head, ..._tail], 0) => Some(head)
      | ([], _k) => None
      | ([_head, ...tail], k) => aux(tail, k-1)
    };
  aux(list, k);
};
