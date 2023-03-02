let
  fun test (filename, anyErrors) =
    let val absyn = Parse.parse filename
        val res = (print ("Testing file " ^ filename ^ "\n"); Semant.transProg absyn; !ErrorMsg.anyErrors)
    in
      (ErrorMsg.reset;
      if res = anyErrors
            then (print ("Test Passed!\n"); true)
            else (print ("Test Failed!\n, expected errors: " ^ (Bool.toString anyErrors) ^ "\n");
                print ("got: " ^ (Bool.toString res) ^ "\n");
                false))
    end
  val test_answers = [
    ("testcases/sample-tiger-programs/merge.tig", false),
    ("testcases/sample-tiger-programs/queens.tig", false),
    ("testcases/sample-tiger-programs/test1.tig", false),
    ("testcases/sample-tiger-programs/test2.tig", false),
    ("testcases/sample-tiger-programs/test3.tig", false),
    ("testcases/sample-tiger-programs/test4.tig", false),
    ("testcases/sample-tiger-programs/test5.tig", false),
    ("testcases/sample-tiger-programs/test6.tig", false),
    ("testcases/sample-tiger-programs/test7.tig", false),
    ("testcases/sample-tiger-programs/test8.tig", false),
    ("testcases/sample-tiger-programs/test9.tig", true),
    ("testcases/sample-tiger-programs/test10.tig", true),
    ("testcases/sample-tiger-programs/test11.tig", true),
    ("testcases/sample-tiger-programs/test12.tig", false),
    ("testcases/sample-tiger-programs/test13.tig", true),
    ("testcases/sample-tiger-programs/test14.tig", true),
    ("testcases/sample-tiger-programs/test15.tig", true),
    ("testcases/sample-tiger-programs/test16.tig", true),
    ("testcases/sample-tiger-programs/test17.tig", true),
    ("testcases/sample-tiger-programs/test18.tig", true),
    ("testcases/sample-tiger-programs/test19.tig", true),
    ("testcases/sample-tiger-programs/test20.tig", true),
    ("testcases/sample-tiger-programs/test21.tig", true),
    ("testcases/sample-tiger-programs/test22.tig", true),
    ("testcases/sample-tiger-programs/test23.tig", true),
    ("testcases/sample-tiger-programs/test24.tig", true),
    ("testcases/sample-tiger-programs/test25.tig", true),
    ("testcases/sample-tiger-programs/test26.tig", true),
    ("testcases/sample-tiger-programs/test27.tig", false),
    ("testcases/sample-tiger-programs/test28.tig", true),
    ("testcases/sample-tiger-programs/test29.tig", true),
    ("testcases/sample-tiger-programs/test30.tig", false),
    ("testcases/sample-tiger-programs/test31.tig", true),
    ("testcases/sample-tiger-programs/test32.tig", true),
    ("testcases/sample-tiger-programs/test33.tig", true),
    ("testcases/sample-tiger-programs/test34.tig", true),
    ("testcases/sample-tiger-programs/test35.tig", true),
    ("testcases/sample-tiger-programs/test36.tig", true),
    ("testcases/sample-tiger-programs/test37.tig", false),
    ("testcases/sample-tiger-programs/test38.tig", true),
    ("testcases/sample-tiger-programs/test39.tig", true),
    ("testcases/sample-tiger-programs/test40.tig", true),
    ("testcases/sample-tiger-programs/test41.tig", false),
    ("testcases/sample-tiger-programs/test42.tig", false),
    ("testcases/sample-tiger-programs/test43.tig", true),
    ("testcases/sample-tiger-programs/test44.tig", false),
    ("testcases/sample-tiger-programs/test45.tig", true),
    ("testcases/sample-tiger-programs/test46.tig", false),
    ("testcases/sample-tiger-programs/test47.tig", false),
    ("testcases/sample-tiger-programs/test48.tig", false),
    ("testcases/sample-tiger-programs/test49.tig", true)
  ]
  fun test_all tests = foldl (fn (testcase, b) => (test testcase) andalso b) true tests
  in
    () = OS.Process.exit (if test_all test_answers then OS.Process.success else OS.Process.failure)
end
