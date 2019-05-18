/* Copyright 2019
** Justin Baum
** Problem 4
** 18 May 2019
 */

/* This was the first use of the Ocaml Aux style that paid off. */

/* O(n) */
let length = (list: list('a)) => {
  let rec aux = (list, k: int) => {
    switch list {
      | [] => k
      | [_head, ...tail] => aux(tail, k+1)
    }
  }
  aux(list, 0);
};
