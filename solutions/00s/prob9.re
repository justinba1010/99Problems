/* Copyright 2019
** Justin Baum
** Problem 9
** This 
** 18 May 2019
*/
 
let pack = (list: list('a)) => {
  /*
  O(n)
  Can reuse namespace because outter is not a recursive function
  */
  let rec pack = (list, acc) => {
    switch (list, acc) {
      | ([head1, ...tail], [[head2, ...tail2], ...tail3]) when head1 == head2 => pack(tail, [[head1] @ [head2] @ tail2] @ tail3)
      | ([head1, ...tail], _acc) => pack(tail, [[head1]] @ acc)
      | ([], _acc) => List.rev(acc)
    }
  };
  pack(list, []);
};
