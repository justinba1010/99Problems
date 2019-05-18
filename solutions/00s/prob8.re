/* Copyright 2019
** Justin Baum
** Problem 8
** 18 May 2019
*/

/* O(n) */
let compress = (list: list('a)) => {
  /*
  Can reuse namespace because outter is not a recursive function
  */
  let rec removeDuplicates = (list, acc) => {
    switch (list, acc) {
      | ([head1, ...tail], [head2, ..._tail]) when head1 == head2 => removeDuplicates(tail, acc)
      | ([head1, ...tail], _acc) => removeDuplicates(tail, [head1] @ acc)
      | ([], _acc) => List.rev(acc)
    }
  };
  removeDuplicates(list, []);
};
