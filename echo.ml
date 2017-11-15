open Lwt

(** Echo server *)
let rec echo (address, in_channel, out_channel) = 
  let rec read_and_respond () =
    Lwt_io.read_line in_channel >>= fun line -> 
    Lwt_io.write_line out_channel line >>= fun line ->
    read_and_respond ()
  in
  print_endline "New connection";
  Lwt.catch read_and_respond (fun ex -> print_endline @@ Printexc.to_string ex; return ())

let () =
  Lwt_main.run (Tcp.Server.start echo 7777)
