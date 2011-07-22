type expr =
    | Number of float
    | Variable of string
    | Binary of char * expr * expr
    | Call of string * expr array
    | If of expr * expr * expr

type prototype = Prototype of string * string array

type func = Function of prototype * expr
