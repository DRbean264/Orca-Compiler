structure Test =
struct 
fun test (filename, test_tokens) =
    let val file = TextIO.openIn filename
	fun get _ = TextIO.input file
	val lexer = Mlex.makeLexer get
	fun do_it tl =
	    let val t = lexer()
	    in print t; print "\n";
	       if substring(t,0,3)="EOF" then t::tl else do_it (t::tl)
	    end
    val token_list = rev (do_it [])
    in
        TextIO.closeIn file;
        token_list = test_tokens
    end
end
