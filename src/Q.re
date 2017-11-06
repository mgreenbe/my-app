type t =
  | Q((int, int));

let fromInt = (x) => Q((x, 1));

let fromPair = (x, y) =>
  switch (y, Z.gcd(x, y)) {
  | (_, None)
  | (0, _) => Js.Exn.raiseError("Division by zero")
  | (y, Some(g)) => Q((Z.sign(x) * Z.sign(y) * Z.abs(x) / g, Z.abs(y) / g))
  };

let zero = Q((0, 1));

let one = Q((1, 1));

let equal = (Q((a, b)), Q((c, d))) => a * d - b * c == 0;

let sign = (Q((a, b))) =>
  if (a * b == 0) {
    0
  } else if (a * b < 0) {
    (-1)
  } else {
    1
  };

let isZero = equal(zero);

let isOne = equal(one);

let abs = (Q((a, b))) => Q((abs(a), abs(b)));

let add = (Q((a, b)), Q((c, d))) => fromPair(a * d + b * c, b * d);

let sum = (a) => List.fold_left(add, zero, a);

let neg = (Q((a, b))) => Q((- a, b));

let isMinusOne = (x) => x |> neg |> isOne;

let sub = (Q((a, b)), Q((c, d))) => fromPair(a * d - b * c, b * d);

let mul = (Q((a, b)), Q((c, d))) => fromPair(a * c, b * d);

let prod = (a) => List.fold_left(mul, one, a);

let inv = (Q((a, b))) =>
  if (a == 0) {
    Js.Exn.raiseError("Division by zero")
  } else {
    fromPair(b, a)
  };

let div = (Q((a, b)), Q((c, d))) =>
  if (c == 0) {
    Js.Exn.raiseError("Division by zero")
  } else {
    fromPair(a * d, b * c)
  };

let toString = (Q((a, b))) =>
  b == 1 ? string_of_int(a) : string_of_int(a) ++ "/" ++ string_of_int(b);