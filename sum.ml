open Lwt


type operation =
  | Add of (int * int)

let compute = function
  | Add (a,b) -> 
      a + b + 1

let parser =
  let open Angstrom in
  let integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string
  in
  return (fun a b -> Add (a,b))
    <* char '+'
    <* char ' '
    <*> integer
    <* char ' '
    <*> integer
    <* end_of_input

(** read to ints and send back their sum *)
let service (address, in_channel, out_channel)= 

  let rec read_and_sum () =
    Lwt_io.read_line in_channel >>= fun line ->

    print_endline line;

    (match Angstrom.parse_only parser (`String line) with
     | Ok op -> 
       compute op
         |> string_of_int
         |> Lwt_io.write_line out_channel
     | Error err -> 
       "ERROR"
          |> Lwt_io.write_line out_channel
         ) >>= fun () ->

    read_and_sum ()
  in

  read_and_sum ()

let () =
  Lwt_main.run (Tcp.Server.start service 7777)
