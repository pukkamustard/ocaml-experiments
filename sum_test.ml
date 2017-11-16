open Lwt
open Reactor


let address =
  Unix.ADDR_INET (Unix.inet_addr_loopback, 7777)

type model =
  { connection: Tcp.Client.connection option
  ; events : (string * string) list
  }

type msg =
  | Connect
  | ConnectResult of Tcp.Client.connection result
  | Send of string * string
  | Receive of string * string
  | Error of string
  | NoOp
  | NextEvent
  | Close of bool
  | Stop of bool

let init events () =
  {
    connection = None
  ; events = events
  }
    |> Return.singleton
    |> Return.command (return Connect)

let timeout t msg =
  Lwt_unix.sleep t >>= fun () ->
  return msg

let update ~stop model = function
  | Connect ->
    model
      |> Return.singleton
      |> Return.command (timeout 0.2 (Error "connect timeout") <?> 
                        ( Tcp.Client.connect address >>= fun result ->
                          return @@ ConnectResult result))

  | ConnectResult (Ok connection) ->
    { model with connection = Some connection }
      |> Return.singleton
      |> Return.command (return NextEvent)

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
          timeout 0.2 (Error "timeout")
            <?>
          (
            Lwt_io.read_line connection.input >>= fun received ->
            return @@ Receive (expected, received)
          )
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
        return NextEvent
      else
        return @@ Error "received does not match expectation"
    )

  | Error msg ->
    print_endline @@ "Error: " ^ msg;
    model
      |> Return.singleton
      |> Return.command (return @@ Close false)

  | NextEvent ->
    (match model.events with
      | [] ->
        model
          |> Return.singleton
          |> Return.command (return @@ Close true)

      | (line, expectation) :: tail ->
        { model with events = tail }
          |> Return.singleton
          |> Return.command (return @@ Send (line,expectation))
    )

  | Close passed ->
    model
      |> Return.singleton
      |> Return.command (
        match model.connection with
          | Some connection ->
            Lwt_unix.close connection.socket >>= fun () ->
            return (Stop passed)
          | None ->
            return (Stop passed)
        )
 
  | Stop passed ->
    model
      |> Return.singleton
      |> Return.command (stop passed >>= fun () ->
                        return NoOp)

  | NoOp ->
    model |> Return.singleton


let simulate events =
  let (start, state, app) = App.create (init events) update in
  Lwt.wakeup start ();
  match Lwt_main.run app with
  | Ok value -> value
  | Error e -> false


let events = 
  let open QCheck.Gen in
  (pair nat nat >>= fun (a,b) ->
  return (Printf.sprintf "+ %d %d" a b, Printf.sprintf "%d" (a+b))
  )
    |> QCheck.make
    |> QCheck.list

let test =
  let open QCheck in
  Test.make ~count:100
   events simulate

let () =
  QCheck_runner.run_tests_main [test]


