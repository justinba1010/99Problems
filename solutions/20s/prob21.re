/* Copyright 2019
** Justin Baum
** Problem 21
** 18 May 2019
*/

let insertAt = (index: int, alfa: 'a, list: list('a)) => {
  let rec aux = (list, acc, index) =>
    switch (list, index) {
      | (_list, index) when index < 0 => list
      | ([head, ...tail], 0) => List.rev(tail @ [head] @ [alfa] @ acc)
      | ([head, ...tail], index) => aux(tail, [head] @ acc, index - 1)
      | ([], _index) => List.rev([alfa] @ acc)
    };
  aux(list, [], index);
};
