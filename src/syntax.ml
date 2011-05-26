type expr =
    | Number of float
    | Binary of char * expr * expr
