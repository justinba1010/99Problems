/* Copyright 2019
** Justin Baum
** Problem 20
** 18 May 2019
*/

let removeAt = (list: list('a), index: int) => {
  let rec aux = (list, acc, index) =>
    switch (list, index) {
      | (_list, index) when index < 0 => list
      | ([_head, ...tail], 0) => List.rev(acc) @ tail
      | ([], _index) => List.rev(acc)
      | ([head, ...tail], index) => aux(tail, [head] @ acc, index - 1)
    };
  aux(list, [], index);
};
