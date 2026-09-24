external f : string -> string = "fups"

let s = "very long string will save!"
let d = f s
let g () () = d
let _x = g ()
