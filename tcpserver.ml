open Lwt
open BatPervasives


(** Example tcp servers *)

(** Bind, listen and run service for every new connection *)
let tcp_server service port =
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

(** Echo server using Lwt_io channels *)
let rec echo (socket,address) () = 
  let ic = Lwt_io.of_fd ~close:(fun () -> Lwt_unix.close socket) ~mode:Lwt_io.input socket in
  let oc = Lwt_io.of_fd ~close:(fun () -> Lwt_unix.close socket) ~mode:Lwt_io.output socket in
  let rec read_and_respond () =
    Lwt_io.read_line ic >>= fun line -> 
    Lwt_io.write_line oc line >>= fun line ->
    read_and_respond ()
  in
  Lwt.catch read_and_respond (fun ex -> print_endline @@ Printexc.to_string ex; return ())

let () =
  Lwt_main.run (tcp_server echo 7777)
