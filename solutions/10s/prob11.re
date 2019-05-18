/* Copyright 2019
** Justin Baum
** Problem 11
** This 
** 18 May 2019
*/

type manyList('a) =
  | One('a)
  | Many(int, 'a);

/* O(n) */
let encode = (list: list('a)) => {
  /*
  O(n)
  Can reuse namespace because outter is not a recursive function
  */
  let rec encode = (acc, list) => {
    switch (list, acc) {
      | ([head, ...tail], [(count, lastChar), ...tail2]) when head == lastChar => encode([(count + 1, lastChar)] @ tail2, tail)
      | ([head, ...tail], _acc) => encode([(1, head)] @ acc, tail)
      | ([], _acc) => acc /* Remove reverse, will reverse in other function */
    }
  };
  let rec encodeMany = (acc, list) => {
    switch (list) {
      | [(1, x), ...tail] => encodeMany([One(x)] @ acc, tail)
      | [(count, x), ...tail] => encodeMany([Many(count, x)] @ acc, tail)
      | [] => acc
    }
  };
  encode([], list) |> encodeMany([]);
};
