external f : string -> unit = "fups"

(* let () = f "Hello world" *)

let ( |> ) x f = f x
let () = f "LOXDDDD" |> fun _ -> ()

(* let s = "very long string will save!"
let d = f s
let g () () = d
let _x = g () *)
