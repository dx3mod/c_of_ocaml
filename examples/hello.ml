let print_int n = Int.to_string n |> Io.puts

let () =
  Io.puts "Hi! What's your name?";
  let name = Io.gets () in
  Io.puts ("Hello, " ^ name ^ "! Here are some Fibonacci numbers:");
  for i = 1 to 30 do
    print_int (Fib.f i)
  done
