open Lwt
open Reactor


let address =
  Unix.ADDR_INET (Unix.inet_addr_loopback, 7777)

type event =
  { delay : float
  ; command : string
  ; expected_response: string
  }

type connection =
  { input: Lwt_io.input_channel
  ; output: Lwt_io.output_channel
  ; socket: Lwt_unix.file_descr
  }

let connect sockaddr =
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  ( Lwt_unix.connect socket sockaddr >>= fun () ->
    let input = Lwt_io.of_fd ~mode:Lwt_io.input socket in
    let output = Lwt_io.of_fd ~mode:Lwt_io.output socket in
    return {input = input; output = output; socket = socket}
  )
    |> Lwt_result.catch 

type model =
  { connection: connection option
  ; events : event list
  }

type msg =
  | Connect
  | ConnectResult of connection result
  | Send of string * string
  | Receive of string * string
  | Error of string
  | NoOp
  | End

let init () =
  {
    connection = None
  ; events = []
  }
    |> Return.singleton
    |> Return.command (return Connect)

let timeout t msg =
  Lwt_unix.sleep t >>= fun () ->
  return msg

let update ~stop model = function
  | Connect ->
    print_endline "Connect";
    model
      |> Return.singleton
      |> Return.command (timeout 0.2 (Error "connect timeout") <?> 
                        ( connect address >>= fun result ->
                          return @@ ConnectResult result))

  | ConnectResult (Ok connection) ->
    print_endline "Connected";
    { model with connection = Some connection }
      |> Return.singleton
      |> Return.command (return @@ Send ("+ 1 2", "3"))

  | ConnectResult (Error _) ->
    model
      |> Return.singleton
      |> Return.command (return @@ Error "connect failed")

  | Send (line,expected) ->
    model 
      |> Return.singleton
      |> Return.command (
        match model.connection with
        | Some connection ->
          Lwt_io.read_line connection.input >>= fun received ->
          return @@ Receive (expected, received)
        | None ->
          return @@ Error "not connected"
      )
      |> Return.command (
        match model.connection with
        | Some connection ->
          Lwt_io.write_line connection.output line >>= fun () ->
          return NoOp
        | None ->
          return @@ Error "not connected"
      )

  | Receive (expected, received) ->
    model 
    |> Return.singleton
    |> Return.command (
      if expected = received then
        return End
      else
        return @@ Error "received does not match expectation"
    )

  | Error msg ->
    print_endline @@ "Error: " ^ msg;
    stop false;
    model
      |> Return.singleton

  | End ->
    stop true;
    model
      |> Return.singleton

  | NoOp ->
    model |> Return.singleton



let () =
  let (start, state, app) = App.create init update in
  Lwt.wakeup start ();
  Lwt_main.run (app >>= fun value -> 
                   Printf.sprintf "App returned with value: %b\n" value |> print_string;
                   return ())
