[%bs.raw {|require('./app.css')|}];

[@bs.module] external logo : string = "./logo.svg";

let se = ReasonReact.stringToElement;

let a = M.fromInt([[0, 1, 2], [3, 4, 5], [6, 7, 8]]);

/*Js.log(a |> M.toString);

  let (r, ops) = M.rref(a);

  List.rev(ops) |> List.iter((op) => op |> M.rowOpToString |> Js.log);

  Js.log(r |> M.toString);

  let b = M.fromInt([[1, 2, 3], [4, 5, 6], [7, 8, 9]]);

  Js.log(b |> M.toString);

  let (r, ops) = M.rref(b);

  List.rev(ops) |> List.iter((op) => op |> M.rowOpToString |> Js.log);

  Js.log(r |> M.toString);

  Js.log(Utils.remove(1, ["a", "b", "c"]) |> Array.of_list);
  */
let z: C.t = (Q.fromPair(0, 7), Q.fromPair((-7), 666));

Js.log(z |> C.toString(~radical="i"));

let component = ReasonReact.statelessComponent("App");

let make = (~message, _children) => {
  ...component,
  render: (_self) =>
    <div className="App">
      <div className="App-header">
        <img src=logo className="App-logo" alt="logo" />
        <h2> (se(message)) </h2>
      </div>
      <p className="App-intro">
        (se("To get started, edit"))
        <code> (se(" src/App.re ")) </code>
        (se("and save to reload."))
      </p>
      <pre> (se(M.toString(a))) </pre>
    </div>
};