open Lwt
open Reactor


let address =
  Unix.ADDR_INET (Unix.inet_addr_loopback, 7777)

type msg =
  | Connect
  | ConnectResult of Tcp.Client.connection result
  | ReaderResult of unit result
  | Send of string * string
  | SendResult of unit result
  | Receive of string
  | NextEvent
  | CloseConnection
  | Stop

type model =
  { connection: Tcp.Client.connection option
  ; reader: msg Lwt.t option
  (* TODO: use functional (non mutable) CCFQueue *)
  ; expecting: string Queue.t
  ; events : (string * string) list
  }

let init events () =
  {
    connection = None
  ; reader = None
  ; expecting = Queue.create ()
  ; events = events
  }
    |> Return.singleton
    |> Return.command (return Connect)

exception Sim_Error of string

let timeout t msg =
  Lwt_unix.sleep t >>= fun () ->
  raise @@ Sim_Error msg

let update ~stop ~(send_msg:?step:React.step -> 'msg -> unit) model = function
  | Connect ->
    model
      |> Return.singleton
      |> Return.command (timeout 0.2 "timeout while connecting" <?> 
                        ( Tcp.Client.connect address >>= fun result ->
                          return @@ ConnectResult result))

  | ConnectResult (Ok connection) ->
    let reader = 
      Tcp.Client.read_line (fun line -> send_msg @@ Receive line) connection
        |> Lwt_result.catch
        |> Lwt.map (fun result -> ReaderResult result)
    in
    { model with connection = Some connection; reader = Some reader }
      |> Return.singleton
      |> Return.command (return NextEvent)
      |> Return.command reader

  | ConnectResult (Error e) ->
    raise e

  | ReaderResult result ->
    (match result with
     | Ok () ->
       model |> Return.singleton
     | Error Lwt.Canceled ->
       model |> Return.singleton
     | Error e ->
       raise e
    )

  | Send (line,expected) ->
    (match model.connection with
      | Some connection ->
        Queue.push expected model.expecting;
        model
          |> Return.singleton
          |> Return.command (Lwt_io.write_line connection.output line
                            |> Lwt_result.catch
                            |> Lwt.map (fun result -> SendResult result)
                           )
      | None ->
        raise @@ Sim_Error "tyring to send while not connected"
    )

  | SendResult (Ok ()) ->
    model
      |> Return.singleton

  | SendResult (Error e) ->
    raise e

  | Receive line ->
    let expected = Queue.pop model.expecting in
    if expected <> line then
      raise @@ Sim_Error ("expecting: " ^ expected ^ " received: " ^ line) 
    else
      model 
      |> Return.singleton
      |> Return.command (return NextEvent)

  | NextEvent ->
    (match model.events with
      | [] ->
        model
          |> Return.singleton
          |> Return.command (return CloseConnection)

      | (line, expectation) :: tail ->
        { model with events = tail }
          |> Return.singleton
          |> Return.command (return @@ Send (line,expectation))
    )

  | CloseConnection ->
    CCOpt.map_or ~default:() Lwt.cancel model.reader;
    {model with connection = None; reader = None}
      |> Return.singleton
      |> Return.command (
        match model.connection with
          | Some connection ->
            Lwt_unix.close connection.socket >>= fun () ->
            return Stop
          | None ->
            return Stop
        )
 
  | Stop ->
    stop true;
    model
      |> Return.singleton


let events = 
  let print = 
    let open QCheck.Print in
    pair string string
  in
  let gen =
    let open QCheck.Gen in
    pair nat nat >>= fun (a,b) ->
    return (Printf.sprintf "%d + %d" a b, Printf.sprintf "%d" (a+b))
  in
  QCheck.make ~print:print gen
    |> QCheck.list

let test =
  let open QCheck in
  Test.make ~count:1000
   events (fun events -> App.run_exn @@ App.create (init events) update)

let () =
  QCheck_runner.run_tests_main [test]


