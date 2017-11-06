let rec constantList = (~u=[], n, x) => n === 0 ? u : constantList(~u=[x, ...u], n - 1, x);

let rec join = (sep, u) =>
  switch u {
  | [] => ""
  | [u_0] => u_0
  | [u_0, ...u_1] => u_0 ++ sep ++ join(sep, u_1)
  };

let findFirst = (f, a) =>
  List.fold_left2(
    (acc, a_i, i) =>
      switch (acc, f(a_i)) {
      | (Some(x), _) => Some(x)
      | (None, true) => Some((i, a_i))
      | _ => None
      },
    None,
    a,
    List.mapi((i, _) => i, a)
  );

let listMax = (a) => a |> List.fold_left(max, 0);

let rec zip = (v) =>
  switch v {
  | [] => []
  | [v_0] => List.map((x) => [x], v_0)
  | [v_0, ...w] => List.map2((x, w_j) => [x, ...w_j], v_0, zip(w))
  };

let rec initList = (~start=0, ~f, n) =>
  start === n ? [] : [f(start), ...initList(~start=start < n ? start + 1 : start - 1, n, ~f)];

let range = (~start=0, n) => initList(~start, ~f=(x) => x, n);

let remove = (n, a) =>
  List.fold_right2(
    (a_i, i, acc) => i === List.length(a) - n - 1 ? acc : [a_i, ...acc],
    a,
    range(~start=List.length(a) - 1, (-1)),
    []
  );

let randomElement = (lst) => Random.int(lst |> List.length) |> List.nth(lst);

let rec randomPermOfRandomSubset = (r, lst) =>
  switch (List.length(lst), r) {
  | (_, r) when r < 0 => Js.Exn.raiseError("The size of a set cannot be negative.")
  | (_, 0) => []
  | (len, r) when r <= len =>
    let n = Random.int(len);
    [List.nth(lst, n), ...randomPermOfRandomSubset(r - 1, remove(n, lst))]
  | _ => Js.Exn.raiseError("Invalid subset size.")
  };

let choose = (n, r) => randomPermOfRandomSubset(r, range(n)) |> List.sort(Pervasives.compare);

let filteri = (f, a) =>
  List.combine(range(a |> List.length), a) |> List.filter(f) |> List.map(snd);