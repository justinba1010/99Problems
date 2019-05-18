/* Copyright 2019
** Justin Baum
** Problem 19
** 18 May 2019
*/

/* O(n) */
let split = (list: list('a), k: int) => {
  let rec aux = (list, acc, k) =>
    switch (list, k) {
      | ([], _k) => (List.rev(acc), list)
      | (_list, k) when k <= 0 => (List.rev(acc), list)
      | ([head, ...tail], k) => aux(tail, [head] @ acc, k-1)
    };
    aux(list, [], k);
};

let rotate = (list: list('a), k: int) => {
  let length = List.length(list);
  let shift = (length + k) mod length;
  switch (split(list, shift)) {
    | (a,b) => b @ a
  };
};
