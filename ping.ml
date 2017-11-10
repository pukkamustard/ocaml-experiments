open Lwt
open Lwt_react
open Reactor

(** Example app using Reactor for functional reactive programming *)

type msg =
    Ping 
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
