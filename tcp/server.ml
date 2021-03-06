open Lwt

(** Bind, listen and run service for every new connection *)
let start service port =
  let open Lwt_unix in

  let tcp_server_listen () =
    let s = socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let addr = Unix.inet_addr_loopback in
    setsockopt s SO_REUSEADDR true;
    bind s (Unix.ADDR_INET (addr,port)) >>= fun () ->
    listen s 10; return s
  in

  let rec accept_loop socket =
    accept socket >>= fun (client_socket, address) -> 

    let worker () = 
      let in_channel = Lwt_io.of_fd ~mode:Lwt_io.input client_socket in
      let out_channel = Lwt_io.of_fd ~mode:Lwt_io.output client_socket in
      Lwt.catch 
        (fun () ->
           service (address, in_channel, out_channel)
        ) 
        (fun ex -> 
             close client_socket
        )
    in

    Lwt.async worker;
    accept_loop socket
  in

  tcp_server_listen () >>= fun socket ->
  Lwt_main.at_exit (fun () -> close socket);
  accept_loop socket

