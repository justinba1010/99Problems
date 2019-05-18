/* Copyright 2019
** Justin Baum
** Problem 21
** 18 May 2019
*/

let range = (i: int, j: int) => {
  let rec aux = (acc, i, j) =>
    switch (i ,j) {
      | (i, j) when i == j => [i] @ acc
      | (i, j) => aux([i] @ acc, i + 1, j)
    };
    switch (i, j) {
      | (i, j) when i > j => aux([],j, i)
      | (i, j) => List.rev(aux([], i, j))
    };
};
