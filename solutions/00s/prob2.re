/* Copyright 2019
** Justin Baum
** Problem 2
** 18 May 2019
 */
 
/* O(n) */
let lastTwo = (list: list('a)) => {
  let rec aux = (list) => 
    switch list {
      | [] => None
      | [head1, head2] => Some((head1, head2))
      | [_head, ...tail] => aux(tail)
    };
    aux(list);
};
