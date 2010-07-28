type token =
    (* Keywords *)
      Def
    (* Operators *)
    | Op of char
    (* Identifiers *)
    | Ident of string
    (* Integers *)
    | Int of int
    (* Floats *)
    | Float of float
