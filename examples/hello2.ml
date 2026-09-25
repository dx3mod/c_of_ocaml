let () = Io.puts "Hello World"

(* let f n =
  let rec loop i a b = if i = n then a else loop (i + 1) b (a + b) in
  loop 0 0 1
;; *)

(* external putc : char -> unit = "caml_putc"

let () = putc 'Y' *)

(* external f : string -> unit = "fups"

(* let () = f "Hello world" *)

let ( |> ) x f = f x
let jojs = (1, ( |> ))

let _ =
  f "LOXDDDD"
  |>
  let uya = "hehehe" in
  (fun s oosi -> fun () _ d -> (s, (jojs, uya), d, f)) "KEKS"

let make_adder x = fun y -> (x, y) *)

(* let s = "very long string will save!"
let d = f s
let g () () = d
let _x = g () *)
