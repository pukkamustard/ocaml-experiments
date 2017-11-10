open Lwt
open Lwt_react
open BatPervasives


(** Model output of update function as writer monad

    Return.t holds state and any side effects (commands).
*)
module Return : sig
  (** The type for return *)
  type ('state,'cmd) t

  (** Create from state *)
  val singleton : 'state -> ('state, 'cmd) t

  (** Add a command to a Return *)
  val command : 'cmd -> ('state, 'cmd) t -> ('state, 'cmd) t

  (** Map on the cmd *)
  val map_cmd : ('a -> 'b) -> ('state, 'a) t -> ('state, 'b) t

  (** Map the state *)
  val map : ('a -> 'b) -> ('a, 'msg) t -> ('b, 'msg) t

  (** Run the side effects and return the state *)
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


module App : sig
  val create: init:(unit -> ('state, 'msg Lwt.t) Return.t) -> update:(stop:'a Lwt.u -> 'state -> 'msg -> ('state, 'msg Lwt.t) Return.t) -> unit Lwt.u * 'state Lwt_react.signal * 'a Lwt.t
end = struct
  let create ~init ~update =
    (* message events *)
    let msg_e, send_msg = E.create () in

    (* command events *)
    let cmd_e, send_cmd = E.create () in

    (* run the commands and send results on as messages *)
    (* Note: we need to keep this from the garbage collector *)
    cmd_e 
      |> E.map (fun t -> Lwt.on_success t send_msg)
      |> E.keep;

    (* we need the initial state but can not run initial effects before the msg handling is set up *)
    let wait_for_start, start = Lwt.wait () in
    let init_state = init ()
      |> Return.map_cmd (fun cmd -> wait_for_start >>= fun () -> cmd)
      |> Return.run send_cmd
    in

    (* allow the app to stop itself with a return value *)
    let stop_promise, stop = Lwt.wait () in

    (* run update function on message event *)
    (* Note: we need to use fold_s to ensure atomic updates *)
    let state = S.fold_s (fun state msg -> update ~stop:stop state msg |> Return.run send_cmd |> return) init_state msg_e in

    (* stop state and pending side effects *)
    let stop_promise = 
      stop_promise >>= fun value ->
      Lwt_react.E.stop msg_e;
      Lwt_react.E.stop cmd_e;
      Lwt_react.S.stop state;
      return value 
    in

    (* Return up start resolver and state signal and promis that is resolved when app stops *)
    (start, state, stop_promise)
end

type state = string

type msg =
  | Ping
  | Pong

let init () =
  0
    |> Return.singleton
    |> Return.command (return Ping)

let update ~stop state = function
  | Ping -> 
    if state > 2 then Lwt.wakeup stop state else ();
    state + 1
      |> Return.singleton
      |> Return.command (return Pong)
  | Pong ->
    state
      |> Return.singleton
      |> Return.command (Lwt_unix.sleep 1.0 >>= fun () -> return Ping)


let () = 
  let (start, state, app) = App.create init update in
  S.map (fun state -> Printf.sprintf "State: %d\n" state |> print_string) state
    |> S.keep;
  (* try removing the S.keep *)
  Gc.full_major ();

  Lwt.wakeup start ();
  Lwt_main.run @@ (app >>= fun value -> 
                   Printf.sprintf "App returned with value: %d\n" value |> print_string;
                   return ())
