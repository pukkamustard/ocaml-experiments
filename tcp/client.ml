open Lwt

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

(* TODO: this might be a nice place to play with Functors. Define something that is a reader and somehow Functor magic so that it can read any kind of data (line, char, byte,...) *)
let read_line (on_read:string -> unit) connection =
  let rec read_loop () = 
    Lwt_io.read_line connection.input >>= fun line ->
    on_read line;
    read_loop ()
  in
  Lwt.catch (fun () -> read_loop () >>= fun _ -> return ()) 
    (fun e -> 
       match e with
       | End_of_file -> return ()
       | e -> raise e
    )
