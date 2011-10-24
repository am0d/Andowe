type expr =
    | Number of int
    | Variable of string
    | Binary of char * expr * expr
    | Call of string * expr array
    | If of expr * expr * expr
    | Assignment of string * expr
    | Sequence of expr * expr

type prototype = Prototype of string * string array

type func = Function of prototype * expr
