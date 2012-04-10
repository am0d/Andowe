exception Error of string
exception TypeError of string

let string_of_error = function
    | Error s ->
            s
    | TypeError s ->
            s
