let test_list_rev =
  let open QCheck in
  Test.make ~count:1000
   (list int) (fun l -> List.rev (List.rev l) = l)

let test = 
  let open QCheck in
  Test.make
    ~count:10_000 ~max_fail:3
    (list small_nat)
    (fun l -> l = List.sort compare l)

let () =
  QCheck.Test.check_exn test
  (*QCheck_runner.run_tests_main [test_list_rev; test]*)
