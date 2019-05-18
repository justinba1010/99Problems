/* Copyright 2019
** Justin Baum
** Problem 7
** 18 May 2019
*/

type multiList('a) =
  | One('a)
  | Many(list(multiList('a)));

/* O(n) */
let flatten = (list: multiList('a)) => {
  let rec aux = (list: multiList('a)) => {
    switch list {
      | One(x) => [x]
      | Many([head]) => aux(head)
      | Many([head, ...[tail]]) => aux(head) @ aux(tail)
      | Many([]) => []
    };
  };
  aux(list);
};

let ex1 = Many([One(1), Many([One(2)])]);
