structure A = Absyn;

let
  val sym = Symbol.symbol
  val es = ref false
  fun test (filename, ans) =
    let val absyn = Parse.parse filename
    in
        print ("Testing file " ^ filename ^ "\n");
        if A.equalsExp(absyn,ans)
            then (print ("Test Passed!\n"); true)
            else (print ("Test Failed!\n, got:\n");
                PrintAbsyn.print (TextIO.stdOut, absyn);
                print ("expected:\n");
                PrintAbsyn.print (TextIO.stdOut, ans);
                false)
    end
  val test_answers = [
    (
      "testcases/lexing/test1.tig",
      A.LetExp{
        decs = [
            A.TypeDec [{name = sym "arrtype", ty=A.ArrayTy(sym "int", 0), pos=0}],
            A.VarDec {
              name   = (sym "arr1"),
              escape = es,
              typ    = SOME(sym "arrtype", 0),
              init   = A.ArrayExp {
                typ = (sym "arrtype"),
                size = A.IntExp 10,
                init = A.IntExp 0,
                pos = 0
              },
              pos    = 0
            }
        ],
        body=A.SeqExp [
        (A.VarExp (A.SimpleVar (sym "arr1", 2)), 0)
        ],
        pos=0
      }
    ),
    (
      "testcases/lexing/teststrings.tig",
      A.LetExp{
        decs=[
        A.VarDec{name= sym "s1", escape = ref false, typ=SOME(sym "string", 0), init=A.StringExp ("Hello \n World!", 0), pos=0},
        A.VarDec{name= sym "s2", escape = ref false, typ=NONE, init=A.StringExp ("I am a multiline string", 0), pos=0}
        ],
        body=A.SeqExp [(A.VarExp (A.SimpleVar (sym "s1", 0)), 0)],
        pos = 0
      }
    )
  ]
  fun test_all tests = foldl (fn (testcase, b) => (test testcase) andalso b) true tests
  in
    () = OS.Process.exit (if test_all test_answers then OS.Process.success else OS.Process.failure)
end
