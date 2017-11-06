let d = (-1);

type t = (Q.t, Q.t);

let zero: t = (Q.zero, Q.zero);

let one: t = (Q.one, Q.zero);

let i: t = (Q.zero, Q.one);

let fromQ = (x) : t => (x, Q.zero);

let fromInt = (n) : t => n |> Q.fromInt |> fromQ;

let toString = (~radical="sqrt(" ++ string_of_int(d) ++ ")", z: t) =>
  switch z {
  | (a, b) when Q.isZero(b) => Q.toString(a)
  | (a, b) when Q.isZero(a) && Q.isOne(b) => radical
  | (a, b) when Q.isZero(a) && Q.isMinusOne(b) => "-" ++ radical
  | (a, b) when Q.isZero(a) => Q.toString(b) ++ "*" ++ radical
  | (a, b) when Q.isOne(b) => Q.toString(a) ++ " + " ++ radical
  | (a, b) when Q.isMinusOne(b) => Q.toString(a) ++ " - " ++ radical
  | (a, b) when Q.sign(b) === (-1) =>
    Q.toString(a) ++ " - " ++ Q.toString(Q.neg(b)) ++ "*" ++ radical
  | (a, b) => Q.toString(a) ++ " + " ++ Q.toString(b) ++ "*" ++ radical
  };

let equal = ((a, b): t, (c, d): t) => Q.equal(a, c) && Q.equal(b, d);

let norm = ((a, b): t) => Q.add(Q.mul(a, a), Q.mul(Q.fromInt(d), Q.mul(b, b)));

let conj = ((a, b): t) : t => (a, Q.neg(b));

let add = ((a, b): t, (c, d): t) : t => (Q.add(a, c), Q.add(b, d));

let neg = ((a, b): t) : t => (Q.neg(a), Q.neg(b));

let sub = (w, z) => add(w, neg(z));

let mul = ((a, b): t, (c, d): t) : t => (
  Q.add(Q.mul(a, c), Q.mul(d, Q.mul(b, d))),
  Q.add(Q.mul(a, d), Q.mul(b, c))
);

let inv = (z) => mul(fromQ(Q.inv(norm(z))), conj(z));

let div = (w, z) => mul(w, inv(z));