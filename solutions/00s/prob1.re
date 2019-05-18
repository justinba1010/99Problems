/* Copyright 2019
** Justin Baum
** Problem 1
** 18 May 2019
 */

/* O(n) */
let last = (list: list('a)) => {
  let rec aux = (list) => 
    switch list {
      | [] => None
      | [head] => Some(head)
      | [_head, ...tail] => aux(tail)
    };
    aux(list);
};
