open Lwt
open Lwt_react


type state = string

type msg =
  | Ping
  | Pong

let rec send_ping send () =
  send Ping;
  Lwt_unix.sleep 1.0 >>= fun () ->
  send_ping send ()

let init send =
  Lwt.async (send_ping send);
  return "Hello"

let update send state msg =
  match msg with
    | Ping -> print_endline "Ping"; send Pong; state
    | Pong -> print_endline "Pong"; state

let rec main () =
  let msg_e, send = E.create () in
  init send >>= fun init ->
  let _ = Lwt_react.E.fold (update send) init msg_e in
  return ()


let () = Lwt_main.run @@ (main () <&> fst @@ Lwt.wait ())
