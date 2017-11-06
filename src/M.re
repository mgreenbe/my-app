type t =
  | Mat(list(list(Q.t)));

let toList = (Mat(a)) => a;

let fromList = (a) => {
  let n = List.length(List.hd(a));
  a |> List.for_all((a_i) => List.length(a_i) === n) ?
    Mat(a) : Js.Exn.raiseError("Rows must have the same length.")
};

let fromInt = (a) => Mat(a |> List.map(V.fromInt));

let initMatrix = (~f, m, n) => Mat(Utils.initList(~f=(i) => Utils.initList(~f=f(i), n), m));

let toString = (Mat(a)) => {
  let b = Utils.zip(a);
  let widths =
    b |> List.map(List.map((x) => x |> Q.toString |> String.length)) |> List.map(Utils.listMax);
  List.map2((b_i, width_i) => V.toStrings(~width=width_i, b_i), b, widths)
  |> Utils.zip
  |> List.map((x) => "[ " ++ Utils.join(" ", x) ++ " ]")
  |> Utils.join("\n")
};

let size = (Mat(a)) => (List.length(a), List.length(List.hd(a)));

let zero = ((m, n)) => Mat(Utils.constantList(m, V.zero(n))); /* one argument of type (int, int) */

let identity = (n) => Mat(List.mapi((i, _) => V.standardBasisVector(n, i), V.zero(n)));

let transpose = (Mat(a)) => Mat(Utils.zip(a));

let equal = (Mat(a), Mat(b)) =>
  List.length(a) === List.length(b) && List.for_all2(V.equal, a, b); /* short-circuits */

let isZero = (a) => equal(a, zero(size(a)));

let add = (a, b) =>
  size(a) == size(b) ?
    Mat(List.map2(V.add, toList(a), toList(b))) :
    Js.Exn.raiseError("Matrices must have the same size.");

let neg = (Mat(a)) => Mat(List.map(V.neg, a));

let sub = (a, b) => add(a, neg(b));

let matvec = (Mat(a), v) => a |> List.map(V.dot(v));

let vecmat = (v, a) => matvec(transpose(a), v);

let mul = (a, b) =>
  snd(size(a)) == fst(size(b)) ?
    Mat(toList(a) |> List.map((a_i) => vecmat(a_i, b))) :
    Js.Exn.raiseError("Matrices have incompatible sizes.");

type rowOp =
  | Swap(int, int)
  | Scale(Q.t, int)
  | Transvect(Q.t, int, int);

let rowOpToString = (op) =>
  switch op {
  | Swap(i, j) => "Swap rows " ++ string_of_int(i) ++ " and " ++ string_of_int(j) ++ "."
  | Scale(x, i) => "Multiple row " ++ string_of_int(i) ++ " by " ++ Q.toString(x) ++ "."
  | Transvect(x, i, j) =>
    "Add "
    ++ Q.toString(x)
    ++ " times row "
    ++ string_of_int(i)
    ++ " to row "
    ++ string_of_int(j)
    ++ "."
  };

let applyRowOp = (op, Mat(a)) => {
  let m = List.length(a);
  let row = List.nth(a);
  switch op {
  | Swap(i, j) =>
    min(i, j) >= 0 && max(i, j) < m ?
      Mat(
        a
        |> List.mapi(
             (k, _) =>
               if (k === i) {
                 row(j)
               } else if (k === j) {
                 row(i)
               } else {
                 row(k)
               }
           )
      ) :
      Js.Exn.raiseError("Row index out of range.")
  | Scale(x, i) => Mat(a |> List.mapi((k, a_k) => k === i ? V.smul(x, a_k) : a_k))
  | Transvect(x, i, j) =>
    Mat(a |> List.mapi((k, a_k) => k === j ? V.add(V.smul(x, row(i)), row(j)) : a_k))
  }
};

let applyRowOps = (ops, a) => List.fold_right(applyRowOp, ops, a);

let leadingColumn = (a) =>
  /* the leftmost nonzero column */
  switch (a |> transpose |> toList |> Utils.findFirst((a_j) => ! V.isZero(a_j))) {
  | None => None
  | x => x
  };

let leader = (a) =>
  /* the topmost nonzero entry in the leftmost nonzero column */
  switch (leadingColumn(a)) {
  | Some((j, a_j)) =>
    switch (V.leader(a_j)) {
    | Some((i, a_ij)) => Some((i, j, a_ij))
    | _ => Js.Exn.raiseError("This shouldn't have happened.")
    }
  | _ => None
  };

let swapAndScale = (a) =>
  /* Put a leading 1 in the topmost row, as far to the left as possible. */
  switch (leader(a)) {
  | Some((0, _, a_ij)) => Q.isOne(a_ij) ? [] : [Scale(Q.inv(a_ij), 0)]
  | Some((i, _, a_ij)) => Q.isOne(a_ij) ? [Swap(0, i)] : [Scale(Q.inv(a_ij), 0), Swap(0, i)]
  | _ => []
  };

let folder = (a_j) =>
  List.fold_left2(
    (acc, a_ij, i) => i > 0 && ! Q.isZero(a_ij) ? [Transvect(Q.neg(a_ij), 0, i), ...acc] : acc,
    [],
    a_j,
    List.mapi((i, _) => i, a_j)
  );

let transvect = (Mat(a)) =>
  /* a must be in row echelon form */
  switch a {
  | [] => []
  | [a_0, ..._] =>
    switch (V.leader(a_0)) {
    | Some((j, _)) => List.nth(Utils.zip(a), j) |> folder
    | None => []
    }
  };

let incrementRowIndices = (ops) =>
  ops
  |> List.map(
       (op) =>
         switch op {
         | Swap(i, j) => Swap(i + 1, j + 1)
         | Scale(x, i) => Scale(x, i + 1)
         | Transvect(x, i, j) => Transvect(x, i + 1, j + 1)
         }
     );

let rec rowEchelonForm = (a) => {
  let ops1 = swapAndScale(a);
  let b = applyRowOps(ops1, a);
  let ops2 = transvect(b);
  let c = applyRowOps(ops2, b);
  switch (c |> toList) {
  | [c_0, ...rest_of_c] =>
    let (Mat(d), ops) = rowEchelonForm(Mat(rest_of_c));
    (Mat([c_0, ...d]), List.concat([incrementRowIndices(ops), ops2, ops1]))
  | _ => (Mat([]), [])
  }
};

let rec rrefReflected = (a) => {
  let ops = transvect(a);
  let b = applyRowOps(ops, a);
  switch (b |> toList) {
  | [b_0, ...rest_of_b] =>
    let (Mat(c), moreOps) = rrefReflected(Mat(rest_of_b));
    (Mat([b_0, ...c]), List.concat([incrementRowIndices(moreOps), ops]))
  | _ => (Mat([]), [])
  }
};

let reflectMatrix = (Mat(a)) => a |> List.rev |> fromList;

let reflectRowIndices = (m, ops) =>
  ops
  |> List.map(
       (op) =>
         switch op {
         | Swap(i, j) => Swap(m - 1 - i, m - 1 - j)
         | Scale(x, i) => Scale(x, m - 1 - i)
         | Transvect(x, i, j) => Transvect(x, m - 1 - i, m - 1 - j)
         }
     );

let rref = (a) => {
  let (ref, refOps) = a |> rowEchelonForm;
  let (rrefReflected, rrefOpsReflected) = ref |> reflectMatrix |> rrefReflected;
  let (m, _) = size(a);
  (reflectMatrix(rrefReflected), List.append(rrefOpsReflected |> reflectRowIndices(m), refOps))
};

let rec randomRREFWithPivots = (~m, ~n, ~pivots, ~values) =>
  switch pivots {
  | [firstPivot, ...otherPivots] =>
    m === 0 ?
      [] :
      [
        Utils.initList(
          n,
          ~f=
            (j) =>
              if (j === firstPivot) {
                Q.fromInt(1)
              } else if (j > firstPivot && ! List.mem(j, otherPivots)) {
                Utils.randomElement(values)
              } else {
                Q.fromInt(0)
              }
        ),
        ...randomRREFWithPivots(~m=m - 1, ~n, ~pivots=otherPivots, ~values)
      ]
  | _ => zero((m, n)) |> toList
  };

let ( ** ) = mul;

let randomTransvection = (n) => {
  let i = Random.int(n);
  let k = Random.int(n - 1);
  let j = k < i ? k : k + 1;
  Transvect(Q.fromInt(Random.bool() ? (-1) : 1), i, j)
};

let randomPermutation = (n) => Mat(identity(n) |> toList |> Utils.randomPermOfRandomSubset(n));

let randomProductOfTransvections = (~nFactors=10, n) =>
  Utils.initList(nFactors, ~f=(_) => randomTransvection(n))
  |> ((ops) => applyRowOps(ops, identity(n)));

let randomUpperUnipotent = (~values, n) =>
  initMatrix(
    ~f=
      (i, j) =>
        if (i === j) {
          Q.fromInt(1)
        } else if (i < j) {
          Utils.randomElement(values)
        } else {
          Q.zero
        },
    n,
    n
  );

let randomLowerUnipotent = (~values, n) => randomUpperUnipotent(~values, n) |> transpose;

let randomDiagonal = (~values, ~invertible=false, n) => {
  let filteredValues = invertible ? values |> List.filter((x) => ! Q.isZero(x)) : values;
  initMatrix(~f=(i, j) => i === j ? Utils.randomElement(filteredValues) : Q.zero, n, n)
};

let randomPLDU = (~values, ~invertible=true, n) => {
  let p = randomPermutation(n);
  let l = randomLowerUnipotent(~values, n);
  let d = randomDiagonal(~values, ~invertible, n);
  let u = randomUpperUnipotent(~values, n);
  p ** (l ** (d ** u))
};

let horizontalJoin = (Mat(a), Mat(b)) => Mat(List.map2((a_i, b_i) => List.append(a_i, b_i), a, b));

let verticalJoin = (Mat(a), Mat(b)) => Mat(List.append(a, b));

let subList = (indices, lst) => lst |> Utils.filteri(((i: int, _)) => List.mem(i, indices));

let subMatrix = (rows, cols, Mat(a)) => Mat(a |> subList(rows) |> List.map(subList(cols)));

let a =
  Mat(
    randomRREFWithPivots(
      ~m=5,
      ~n=8,
      ~pivots=[0, 1, 3, 4],
      ~values=Utils.range(~start=(-4), 5) |> List.map(Q.fromInt)
    )
  );

let inverse = (a) =>
  switch (size(a)) {
  | (m, n) when m === n =>
    let r = horizontalJoin(a, identity(m)) |> rref |> fst;
    let left = r |> subMatrix(Utils.range(m), Utils.range(n));
    let right = r |> subMatrix(Utils.range(m), Utils.range(~start=n, 2 * n));
    equal(left, identity(m)) ? Some(right) : None
  | _ => None
  };

let inverseUnsafe = (a) => {
  let (m, n) = size(a);
  let r = horizontalJoin(a, identity(m)) |> rref |> fst;
  r |> subMatrix(Utils.range(m), Utils.range(~start=n, 2 * n))
};

let diagonal = (Mat(a)) => List.mapi((i, a_i) => List.nth(a_i, i), a);

let det = (a) => {
  let (r, ops) = rref(a);
  ops
  |> List.map(
       (op) =>
         switch op {
         | Swap(_, _) => Q.neg(Q.one)
         | Scale(x, _) => Q.inv(x)
         | Transvect(_, _, _) => Q.one
         }
     )
  |> Q.prod
  |> Q.mul(r |> diagonal |> Q.prod)
};