let fromInt = (v) => v |> List.map(Q.fromInt);

let toList = (v) => v;

let length = (v) => List.length(v);

let zero = (n) =>
  n > 0 ? Utils.constantList(n, Q.zero) : Js.Exn.raiseError("Dimension must be positive.");

let standardBasisVector = (n, i) => List.mapi((j, _) => j === i ? Q.one : Q.zero, zero(n));

let padStart = (n, c, s) => String.make(max(0, n - String.length(s)), c) ++ s;

let toStrings = (~width=0, v) => v |> List.map(Q.toString) |> List.map(padStart(width, ' '));

let toString = (~width=0, ~sep=", ", ~ldelim="[", ~rdelim="]", v) =>
  ldelim ++ (v |> toStrings(~width) |> Utils.join(sep)) ++ rdelim;

/* let padToCommonLength = (u, v) => {
     let (i, j) = (length(u), length(v));
     let k = max(i, j);
     (append(u, make(k - i, Q.zero)), append(v, make(k - j, Q.zero)))
   };
    */
let leader = (u) => u |> Utils.findFirst((u_j) => ! Q.isZero(u_j));

let equal = (u, v) => List.length(u) === List.length(v) && List.for_all2(Q.equal, u, v); /* short-circuits */

let isZero = (v) => equal(zero(List.length(v)), v);

let add = (u, v) =>
  List.length(u) === List.length(v) ?
    List.map2(Q.add, u, v) : Js.Exn.raiseError("tor summands must have equal length.");

let neg = (u) => u |> List.map(Q.neg);

let sub = (u, v) => add(u, neg(v));

let sum = (u) => u |> List.fold_left(Q.add, Q.zero);

let dot = (u, v) =>
  List.length(u) === List.length(v) ?
    List.map2(Q.mul, u, v) |> sum : Js.Exn.raiseError("tor factors must have equal length.");

let smul = (x) => List.map(Q.mul(x));