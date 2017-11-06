let sign = (x) =>
  if (x == 0) {
    0
  } else if (x > 0) {
    1
  } else {
    (-1)
  };

let abs = abs;

let rec gcd = (a, b) =>
  switch (min(abs(a), abs(b)), max(abs(a), abs(b))) {
  | (_, 0) => None
  | (0, v) => Some(v)
  | (u, v) => gcd(v mod u, u)
  };