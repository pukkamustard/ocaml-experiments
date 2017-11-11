open Lwt
open BatPervasives


(** Example tcp servers *)

(** Bind, listen and run service for every new connection *)
let start service port =
  let open Lwt_unix in
  let tcp_server_listen () =
    let s = socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let addr = Unix.inet_addr_loopback in
    bind s (Unix.ADDR_INET (addr,port)) >>= fun () ->
    listen s 10; return s
  in
  let rec accept_loop service socket =
    accept socket >>= fun sa -> 
    service sa |> async |> return >>= fun () -> 
    accept_loop service socket
  in
  tcp_server_listen () >>=
  accept_loop service

(** Print New Connection and close *)
let dummy (socket, address) () =
  print_endline "New Connection!";
  Lwt_unix.close socket

