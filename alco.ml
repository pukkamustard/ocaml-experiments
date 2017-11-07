module To_test = struct
  let capit letter = Char.uppercase_ascii letter
  let plus int_list = List.fold_left (fun a b -> a + b) 0 int_list
end

(* The test functions

   Tests are just fust functions of type `unit -> unit`.

*)
let capit () =
  Alcotest.check Alcotest.char "same chars"  'A' (To_test.capit 'a')

let plus () =
  Alcotest.(check int) "same ints" 7 (To_test.plus [1;1;2;3])

let sleep () =
  Lwt_main.run (Lwt_unix.sleep 2.0)

let command () =
  Alcotest.check Alcotest.int "command exits with 0" 0 (Sys.command "ps -ef")


(* List of test cases

   A list of Alcotest.test, which is a triple of a documentation string speed level and test fuction.

   Speed level is a boolean (isomorphic) indicating if the test case will be run in quick mode (with command mode option `-q`).
*)
let test_set = [
  "Capitalize" , `Quick, capit;
  "Add entries", `Slow , plus ;
  "Sleep", `Quick, sleep;
  "Command", `Slow, command;
]

let test2 = [
  "Sleep", `Quick, sleep;
]

(* Run it *)
let () =
  Alcotest.run ~and_exit:false "My first test" [
    "test_set", test_set;
  ];
  Alcotest.run ~and_exit:false "Second test suite" [
    "whasdf", test2;
  ];
  (*Following line is to test usage of internal libraries*)
  print_endline Internal_lib.A.message
