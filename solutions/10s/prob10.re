/* Copyright 2019
** Justin Baum
** Problem 10
** This 
** 18 May 2019
*/
 
let encode = (list: list('a)) => {
  /*
  O(n)
  Can reuse namespace because outter is not a recursive function
  */
  let rec encode = (list, acc) => {
    switch (list, acc) {
      | ([head, ...tail], [(count, lastChar), ...tail2]) when head == lastChar => encode(tail, [(count + 1, lastChar)] @ tail2)
      | ([head, ...tail], _acc) => encode(tail, [(1, head)] @ acc)
      | ([], _acc) => List.rev(acc)
    }
  };
  encode(list, []);
};
