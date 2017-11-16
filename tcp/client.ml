open Lwt
open BatPervasives

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

