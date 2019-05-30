/* Copyright 2019
** Justin Baum
** Problem 30
** 29 May 2019
*/

let rec gcd = (a, b) => {
  switch (a,b) {
    | (a, 0) => a
    | (a, b) => gcd(b, a mod b)
  }
};
