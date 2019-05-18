/* Copyright 2019
** Justin Baum
** Problem 6
** 18 May 2019
*/

/* O(n) */

let isPalindrome = (list: list('a)) => {
  let rec aux = (list, listRev) => {
    switch (list, listRev) {
      | ([x, ...tail1], [y, ...tail2]) when x == y => aux(tail1, tail2)
      | ([], []) => true
      | _ => false
    };
  };
  aux(list, List.rev(list));
};
