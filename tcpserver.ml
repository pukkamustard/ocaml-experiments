open Lwt
open BatPervasives


let service (socket, address) =
  return (
    print_endline "New Connection!"
  )

let tcp_server_listen () =
  let open Lwt_unix in
  let s = socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let addr = Unix.inet_addr_loopback in
  let port = 9000 in
  bind s (Unix.ADDR_INET (addr,port))
  >>= fun () -> listen s 10; return s

let rec handler (socket,address) () = 
  let buffer = Bytes.create 64 in
  Lwt_unix.read socket buffer 0 64
  >>= fun n -> Lwt_unix.write socket buffer 0 n
  >>= fun _ -> handler (socket,address) ()

let rec accept_loop socket =
  Lwt_unix.accept socket
  >>= fun sa -> handler sa |> Lwt.async |> return 
  >>= fun () -> accept_loop socket

let tcp_server =
  tcp_server_listen ()
    >>= (accept_loop )

let () =
  Lwt_main.run (tcp_server)

