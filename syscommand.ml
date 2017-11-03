open Lwt

let main () =
  let command = "", [| "ps"; "-ef" |] in
  Lwt_process.exec ~stdout:`Keep command
  >>= fun _ -> Lwt_process.exec ~stdout:`Keep ("",[| "uname"; "-a" |])
  >>= fun _ -> return ()


let () =
  Lwt_main.run (main ())
