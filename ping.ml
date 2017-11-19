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

let update ~stop ~send_msg model = function
  | Ping -> 
    if model > 2 then
      stop model
    else
      ();
    model + 1
      |> Return.singleton
      |> Return.command (return Pong)
  | Pong ->
    model
      |> Return.singleton
      |> Return.command (Lwt_unix.sleep 0.01 >>= fun () -> return Ping)

let () = 
  let app = App.create init update in
  S.map (fun model -> Printf.sprintf "State: %d\n" model |> print_string) app.model_signal
  (* try removing the S.keep and see what the garbage collector does *)
    |> S.keep;

  Gc.full_major ();

  Lwt.wakeup app.start ();
  match (Lwt_main.run app.result) with 
  | Ok value -> 
    Printf.sprintf "Ok: %d\n" value |> print_string;
  | Error e ->
    ("Error: " ^ Printexc.to_string e)
      |> print_endline
