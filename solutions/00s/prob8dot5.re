/* Copyright 2019
** Justin Baum
** Problem 8.5
** I misread the problem, this will remove all duplicates
** 18 May 2019
*/
 
let removeDuplicates = (list: list('a)) => {
  /* O(n) */
  let rec inList = (a, list) => {
    switch list {
      | [head, ...tail] when head == a => true
      | [head, ...tail] => inList(a, tail)
      | [] => false
    }
  };
  /*
  O(n^2)
  Can reuse namespace because outter is not a recursive function
  */
  let rec removeDuplicates = (list, acc) => {
    switch list {
      | [head, ...tail] when inList(head, acc) => removeDuplicates(tail, acc)
      | [head, ...tail] => removeDuplicates(tail, [head] @ acc)
      | [] => List.rev(acc)
    }
  };
  removeDuplicates(list, []);
};
