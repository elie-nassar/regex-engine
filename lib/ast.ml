type regex =
  | Epsilon
  | Char of char
  | Concat of regex * regex
  | Union of regex * regex
  | Star of regex