open Lwt

(** Echo server *)
let rec echo (socket,address) () = 
  let ic = Lwt_io.of_fd ~close:(fun () -> Lwt_unix.close socket) ~mode:Lwt_io.input socket in
  let oc = Lwt_io.of_fd ~close:(fun () -> Lwt_unix.close socket) ~mode:Lwt_io.output socket in
  let rec read_and_respond () =
    Lwt_io.read_line ic >>= fun line -> 
    Lwt_io.write_line oc line >>= fun line ->
    read_and_respond ()
  in
  print_endline "New connection";
  Lwt.catch read_and_respond (fun ex -> print_endline @@ Printexc.to_string ex; return ())

let () =
  Lwt_main.run (Tcp.Server.start echo 7777)
