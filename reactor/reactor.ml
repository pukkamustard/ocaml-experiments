open Lwt
open Lwt_react


(** Model output of update function as writer monad

    Return.t holds model and any side effects (commands).
*)
module Return : sig
  (** The type for return *)
  type ('model,'cmd) t

  (** Create from model*)
  val singleton : 'model -> ('model, 'cmd) t

  (** Add a command to a Return *)
  val command : 'cmd -> ('model, 'cmd) t -> ('model, 'cmd) t

  (** Map on the cmd *)
  val map_cmd : ('a -> 'b) -> ('model, 'a) t -> ('model, 'b) t

  (** Map the model*)
  val map : ('a -> 'b) -> ('a, 'msg) t -> ('b, 'msg) t

  (** Run the side effects and return the model*)
  val run: ('cmd -> unit) -> ('model, 'cmd) t ->  'model
end = struct
  type ('model,'cmd) t = 'model* 'cmd list

  let singleton model = model, []

  let command cmd (model, cmds) =
    (model, cmd :: cmds)

  let map f (model, cmds) =
    (f model, cmds)

  let map_cmd f (model, cmds) =
    (model, List.map f cmds)

  let run runner (model, cmds) =
    List.iter runner cmds;
    model 
end

(** An Elm inspired way of doing functional reactive programming *)
module App : sig

  (** An app is an object with a function to start the application, a function to send messages to the running app, a signal carrying the model and a promise that resolves to the result of the app's computation *)
  type ('a, 'model, 'msg) t = 
    { start: unit Lwt.u
    ; send: ?step:React.step -> 'msg -> unit
    ; model_signal : 'model Lwt_react.signal
    ; result : ('a, exn) Lwt_result.t
    }

  (** An init function retuns the initial model and side-effects. *)
  type ('model, 'msg) init = unit -> ('model, 'msg Lwt.t) Return.t

  type ('a, 'model, 'msg) update = stop:('a -> unit Lwt.t) -> 'model -> 'msg -> ('model, 'msg Lwt.t) Return.t

(** [create ~init:init ~update:update] creates an app described by the provided [init] and [update] functions. *)
  val create: 
    init: ('model, 'msg) init -> 
    update:('a, 'model, 'msg) update ->
    ('a, 'model, 'msg) t

end = struct

  type ('a, 'model, 'msg) t = 
    { start: unit Lwt.u
    ; send: ?step:React.step -> 'msg -> unit
    ; model_signal : 'model Lwt_react.signal
    ; result : ('a, exn) Lwt_result.t
    }

  type ('model, 'msg) init = unit -> ('model, 'msg Lwt.t) Return.t

  type ('a, 'model, 'msg) update = stop:('a -> unit Lwt.t) -> 'model -> 'msg -> ('model, 'msg Lwt.t) Return.t

  let create ~init ~update =
    
    (* set equality for model signal to phyisical equality *)
    let eq = fun a b -> a == b in

    (* message events *)
    let msg_e, send_msg = E.create () in

    (* command events *)
    let cmd_e, send_cmd = E.create () in

    (* app result and resolver *)
    let result, resolver = Lwt.task () in
    let on_exception = fun e -> Lwt.wakeup resolver (Error e) in
    let on_stop = fun a -> return @@ Lwt.wakeup resolver (Ok a) in

    (* run the commands and send results on as messages *)
    (* Note: we need to keep this from the garbage collector *)
    cmd_e 
      |> E.map (fun t -> Lwt.on_any t send_msg on_exception)
      |> E.keep;

    (* we need the initial model but can not run initial effects before the msg handling is set up *)
    let wait_for_start, start = Lwt.wait () in
    let init_model = init ()
      |> Return.map_cmd (fun cmd -> wait_for_start >>= fun () -> cmd)
      |> Return.run send_cmd
    in

    (* run update function on message event *)
    (* Note: we need to use fold_s to ensure atomic updates *)
    let model_signal = S.fold_s ~eq:eq (fun model msg -> 
        try 
        update ~stop:on_stop model msg 
          |> Return.run send_cmd 
          |> return
        with
          e -> on_exception e; model |> return
      ) init_model msg_e in

    (* stop events and signal *)
    let result = 
      result >>= fun value ->
      Lwt_react.E.stop msg_e;
      Lwt_react.E.stop cmd_e;
      Lwt_react.S.stop model_signal;
      return value 
    in

    (* Catch any other possible exceptions (e.g. Cancel) *)
    let result = Lwt.catch (fun () -> result) (fun e -> return @@ Error e) in

    { start = start
    ; send = send_msg
    ; model_signal = model_signal
    ; result = result
    }
end
