let val test_tokens = 
[
    Tokens.LET (44, 47),
    Tokens.TYPE (49, 53),
    Tokens.ID (("arrtype"), 55, 62),
    Tokens.EQ (63, 64),
    Tokens.ARRAY (65, 70),
    Tokens.OF (71, 73),
    Tokens.ID(("int"), 74, 77),
    Tokens.VAR (79, 82),
    Tokens.ID (("arr1"), 83, 87),
    Tokens.COLON (87, 88),
    Tokens.ID (("arrtype"), 88, 95),
    Tokens.ASSIGN (96, 98),
    Tokens.ID (("arrtype"), 99, 106),
    Tokens.LBRACK (107, 108),
    Tokens.INT ((10), 108, 110),
    Tokens.RBRACK (110, 111),
    Tokens.OF (112, 114),
    Tokens.INT ((0), 115, 116),
    Tokens.IN (117, 119),
    Tokens.ID (("arr1"), 121, 125),
    Tokens.END (126, 129),
    Tokens.EOF (129, 132)
]
in 
OS.Process.exit (if Test.test ("test1.tig", test_tokens) then OS.Process.success
                 else OS.Process.failure)
end
