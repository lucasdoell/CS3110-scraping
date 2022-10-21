(*takes an input from the command line and passes it along to another function.*)

exception EmptyInput of string
(**Raised when the input from the command line is empty.*)

val parse: string -> string -> string
(**Takes a string from the command line and the context it was asked for, 
    and outputs the string for another function to use.*)