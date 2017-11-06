let (fold_left, init, length, map, mapi) = (
  Array.fold_left,
  Array.init,
  Array.length,
  Array.map,
  Array.mapi
);

let rec constantList = (~u=[], n, x) => n === 0 ? u : constantList(~u=[x, ...u], n - 1, x);

let arrMax = (a) => a |> fold_left(max, 0);

let init2 = (m, n, f) => init(m, (i) => init(n, f(i)));

let map2 = (f, (a, b)) => a |> mapi((i, a_i) => f(a_i, b[i]));

let zip = (a) => {
  let m = length(a);
  m === 0 ?
    a :
    {
      let n = length(a[0]);
      fold_left((b, v) => b && length(v) == length(a[0]), true, a) ?
        init2(n, m, (i, j) => a[j][i]) : Js.Exn.raiseError("Rows must have the same length.")
    }
};

let join = (~ldelim="", ~rdelim="", sep, a) => ldelim ++ Js.Array.joinWith(sep, a) ++ rdelim;

let findFirst = (f, a: array('a)) =>
  Js.Array.reducei(
    (found, a_i, i) =>
      switch (found, f(a_i)) {
      | (Some(x), _) => Some(x)
      | (None, true) => Some((i, a_i))
      | _ => None
      },
    None,
    a
  );

let findAll = (f, a: array('a)) =>
  Js.Array.reducei((found, a_i, i) => f(a_i) ? [(i, a_i), ...found] : found, [], a);