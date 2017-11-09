open Lwt
open Lwt_react
open BatPervasives


module Return : sig
  type ('state,'cmd) t
  val singleton : 'state -> ('state, 'cmd) t
  val command : 'cmd -> ('state, 'cmd) t -> ('state, 'cmd) t
  val map_cmd : ('a -> 'b) -> ('state, 'a) t -> ('state, 'b) t
  val map : ('a -> 'b) -> ('a, 'msg) t -> ('b, 'msg) t
  val run: ('cmd -> unit) -> ('state, 'cmd) t ->  'state
end = struct
  type ('state,'cmd) t = 'state * 'cmd list
  let singleton state = state, []
  let command cmd (state, cmds) =
    (state, cmd :: cmds)
  let map f (state, cmds) =
    (f state, cmds)
  let map_cmd f (state, cmds) =
    (state, List.map f cmds)
  let run runner (state, cmds) =
    List.iter runner cmds;
    state
end


type state = string

type msg =
  | Ping
  | Pong

let init () =
  "Hello"
    |> Return.singleton
    |> Return.command (return Ping)

let update state = function
  | Ping -> 
    print_endline "Ping";
    state
      |> Return.singleton
      |> Return.command (return Pong)
  | Pong ->
    print_endline "Pong";
    state
      |> Return.singleton

let main () =
  (* message events *)
  let msg_e, send_msg = E.create () in
  (* command events *)
  let cmd_e, send_cmd = E.create () in

  (* run the commands and send results on as messages *)
  let _ = cmd_e |> E.map (fun t -> Lwt.on_success t send_msg) in

  (* we need the init state but can not run init effects before the msg handling is set up *)
  let hold_init, run_init = Lwt.wait () in
  let init_state = init ()
    |> Return.map_cmd (fun cmd -> hold_init >>= fun () -> cmd)
    |> Return.run send_cmd
  in

  (* run update function on message event *)
  let _ = E.fold (fun state msg -> update state msg |> Return.run send_cmd) init_state msg_e in

  (* we are ready to run init effects *)
  Lwt.wakeup run_init ();
  return ()


let () = Lwt_main.run @@ (main () <&> fst @@ Lwt.wait ())
