structure Test =
struct 
fun test (filename, test_tokens) =
let val file = TextIO.openIn filename
    fun get _ = TextIO.input file
    val lexer = Mlex.makeLexer get
    fun do_it tl =
        let val t = lexer()
        in (*print t; print "\n";*)
           if substring(t,0,3)="EOF" then t::tl else do_it (t::tl)
        end
    fun tl_to_string [] = "" 
      | tl_to_string (a::[]) = a
      | tl_to_string (a::l) = (a ^ ", " ^ (tl_to_string l))
    val token_list = rev (do_it [])
in
    TextIO.closeIn file;
    print ("Testing file " ^ filename ^ "\n");
    if token_list = test_tokens then print ("Test on " ^ filename ^ " passed!\n") else
      print ("Test on " ^ filename ^ " failed!\nExpected tokens: \n[" ^ (tl_to_string test_tokens) ^ "]\n, got \n["
      ^ (tl_to_string token_list) ^ "]\n");
    token_list = test_tokens
end

fun test_all tests = foldl (fn (testcase, b) => (test testcase) andalso b) true tests

val test_answers = 
[
    (
        "testcases/teststrings.tig",
        [
            Tokens.LET (25, 0),
            Tokens.VAR (33, 0),
            Tokens.ID (("s1"), 37, 0),
            Tokens.COLON (40, 0),
            Tokens.ID (("string"), 42, 0),
            Tokens.ASSIGN (49, 0),
            Tokens.STRING (("Hello \n World!"), 52, 0),
            Tokens.VAR (74, 0),
            Tokens.ID (("s2"), 78, 0),
            Tokens.COLON (81, 0),
            Tokens.ASSIGN (83, 0),
            Tokens.STRING (("I am a multiline string"), 86, 0),
            Tokens.IN (129, 0),
            Tokens.ID (("s1"), 133, 0),
            Tokens.END (136, 0),
            Tokens.EOF (139, 0)
        ]
    ),
    (
        "testcases/test1.tig",
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
    ),
    (
        "testcases/test2.tig",
        [
            Tokens.LET (56, 59),
            Tokens.TYPE (61, 65),
            Tokens.ID (("myint"), 66, 71),
            Tokens.EQ (72, 73),
            Tokens.ID (("int"), 74, 77),
            Tokens.TYPE (79, 83),
            Tokens.ID (("arrtype"), 85, 92),
            Tokens.EQ (93, 94),
            Tokens.ARRAY (95, 100),
            Tokens.OF (101, 103),
            Tokens.ID (("myint"), 104, 109),
            Tokens.VAR (112, 115),
            Tokens.ID (("arr1"), 116, 120),
            Tokens.COLON (120, 121),
            Tokens.ID (("arrtype"), 121, 128),
            Tokens.ASSIGN (129, 131),
            Tokens.ID (("arrtype"), 132, 139),
            Tokens.LBRACK (140, 141),
            Tokens.INT ((10), 141, 143),
            Tokens.RBRACK (143, 144),
            Tokens.OF (145, 147),
            Tokens.INT ((0), 148, 149),
            Tokens.IN (150, 152),
            Tokens.ID (("arr1"), 154, 158),
            Tokens.END (159, 161),
            Tokens.EOF (162, 165)
        ]
    ),
    (
        "testcases/merge.tig",
        [
            (* Note that the current token parsing only cares where it starts,
            * not where it ends. This may be a problem later on, but for now
            * it's ok*)
            Tokens.LET (2, 0),
            Tokens.TYPE (9, 0),
            Tokens.ID (("any"), 14, 0),
            Tokens.EQ (18, 0),
            Tokens.LBRACE (20, 0),
            Tokens.ID (("any"), 21, 0),
            Tokens.COLON (25, 0),
            Tokens.ID (("int"), 27, 0),
            Tokens.RBRACE (30, 0),
            Tokens.VAR (33, 0),
            Tokens.ID (("buffer"), 37, 0),
            Tokens.ASSIGN (44, 0),
            Tokens.ID (("getchar"), 47, 0),
            Tokens.LPAREN (54, 0),
            Tokens.RPAREN (55, 0),
            Tokens.FUNCTION (58, 0),
            Tokens.ID (("readint"), 67, 0),
            Tokens.LPAREN (74, 0),
            Tokens.ID (("any"), 75, 0),
            Tokens.COLON (78, 0),
            Tokens.ID (("any"), 80, 0),
            Tokens.RPAREN (83, 0),
            Tokens.COLON (85, 0),
            Tokens.ID (("int"), 87, 0),
            Tokens.EQ (91, 0),
            Tokens.LET (94, 0),
            Tokens.VAR (98, 0),
            Tokens.ID (("i"), 102, 0),
            Tokens.ASSIGN (104, 0),
            Tokens.INT ((0), 107, 0),
            Tokens.FUNCTION (114, 0),
            Tokens.ID (("isdigit"), 123, 0),
            Tokens.LPAREN (130, 0),
            Tokens.ID (("s"), 131, 0),
            Tokens.COLON (133, 0),
            Tokens.ID (("string"), 135, 0),
            Tokens.RPAREN (141, 0),
            Tokens.COLON (143, 0),
            Tokens.ID (("int"), 145, 0),
            Tokens.EQ (149, 0),
            Tokens.ID (("ord"), 156, 0),
            Tokens.LPAREN (159, 0),
            Tokens.ID (("buffer"), 160, 0),
            Tokens.RPAREN (166, 0),
            Tokens.GE (167, 0),
            Tokens.ID (("ord"), 169, 0),
            Tokens.LPAREN (172, 0),
            Tokens.STRING (("0"), 173, 0),
            Tokens.RPAREN (176, 0),
            Tokens.AND (178, 0),
            Tokens.ID (("ord"), 180, 0),
            Tokens.LPAREN (183, 0),
            Tokens.ID (("buffer"), 184, 0),
            Tokens.RPAREN (190, 0),
            Tokens.LE (191, 0),
            Tokens.ID (("ord"), 193, 0),
            Tokens.LPAREN (196, 0),
            Tokens.STRING (("9"), 197, 0),
            Tokens.RPAREN (200, 0),
            Tokens.FUNCTION (207, 0),
            Tokens.ID (("skipto"), 216, 0),
            Tokens.LPAREN (222, 0),
            Tokens.RPAREN (223, 0),
            Tokens.EQ (225, 0),
            Tokens.WHILE (234, 0),
            Tokens.ID (("buffer"), 240, 0),
            Tokens.EQ (246, 0),
            Tokens.STRING((" "), 247, 0),
            Tokens.OR (251, 0),
            Tokens.ID (("buffer"), 253, 0),
            Tokens.EQ (259, 0),
            Tokens.STRING (("\n"),260, 0),
            Tokens.DO (274, 0),
            Tokens.ID (("buffer"), 277, 0),
            Tokens.ASSIGN (284, 0),
            Tokens.ID (("getchar"), 287, 0),
            Tokens.LPAREN (294, 0),
            Tokens.RPAREN (295, 0),
            Tokens.IN (299, 0),
            Tokens.ID (("skipto"), 302, 0),
            Tokens.LPAREN (308, 0),
            Tokens.RPAREN (309, 0),
            Tokens.SEMICOLON (310, 0),
            Tokens.ID (("any"), 317, 0),
            Tokens.DOT (320, 0),
            Tokens.ID (("any"), 321, 0),
            Tokens.ASSIGN (325, 0),
            Tokens.ID (("isdigit"), 328, 0),
            Tokens.LPAREN (335, 0),
            Tokens.ID (("buffer"), 336, 0),
            Tokens.RPAREN (342, 0),
            Tokens.SEMICOLON (343, 0),
            Tokens.WHILE (350, 0),
            Tokens.ID (("isdigit"), 356, 0),
            Tokens.LPAREN (363, 0),
            Tokens.ID (("buffer"), 364, 0),
            Tokens.RPAREN (370, 0),
            Tokens.DO (379, 0),
            Tokens.LPAREN (382, 0),
            Tokens.ID (("i"), 383, 0),
            Tokens.ASSIGN (385, 0),
            Tokens.ID (("i"), 388, 0),
            Tokens.TIMES (389, 0),
            Tokens.INT ((10), 390, 0),
            Tokens.PLUS (392, 0),
            Tokens.ID (("ord"), 393, 0),
            Tokens.LPAREN (396, 0),
            Tokens.ID (("buffer"), 397, 0),
            Tokens.RPAREN (403, 0),
            Tokens.MINUS (404, 0),
            Tokens.ID (("ord"), 405, 0),
            Tokens.LPAREN (408, 0),
            Tokens.STRING (("0"), 409, 0),
            Tokens.RPAREN (412, 0),
            Tokens.SEMICOLON (413, 0),
            Tokens.ID (("buffer"), 415, 0),
            Tokens.ASSIGN (422, 0),
            Tokens.ID (("getchar"), 425, 0),
            Tokens.LPAREN (432, 0),
            Tokens.RPAREN (433, 0),
            Tokens.RPAREN (434, 0),
            Tokens.SEMICOLON (435, 0),
            Tokens.ID (("i"), 442, 0),
            Tokens.END (445, 0),
            Tokens.TYPE (451, 0),
            Tokens.ID (("list"), 456, 0),
            Tokens.EQ (461, 0),
            Tokens.LBRACE (463, 0),
            Tokens.ID (("first"), 464, 0),
            Tokens.COLON (469, 0),
            Tokens.ID (("int"), 471, 0),
            Tokens.COMMA (474, 0),
            Tokens.ID (("rest"), 476, 0),
            Tokens.COLON (480, 0),
            Tokens.ID (("list"), 482, 0),
            Tokens.RBRACE (486, 0),
            Tokens.FUNCTION (490, 0),
            Tokens.ID (("readlist"), 499, 0),
            Tokens.LPAREN (507, 0),
            Tokens.RPAREN (508, 0),
            Tokens.COLON (510, 0),
            Tokens.ID (("list"), 512, 0),
            Tokens.EQ (517, 0),
            Tokens.LET (523, 0),
            Tokens.VAR (527, 0),
            Tokens.ID (("any"), 531, 0),
            Tokens.ASSIGN (535, 0),
            Tokens.ID (("any"), 538, 0),
            Tokens.LBRACE (541, 0),
            Tokens.ID (("any"), 542, 0),
            Tokens.EQ (545, 0),
            Tokens.INT ((0), 546, 0),
            Tokens.RBRACE (547, 0),
            Tokens.VAR (557, 0),
            Tokens.ID (("i"), 561, 0),
            Tokens.ASSIGN (563, 0),
            Tokens.ID (("readint"), 566, 0),
            Tokens.LPAREN (573, 0),
            Tokens.ID (("any"), 574, 0),
            Tokens.RPAREN (577, 0),
            Tokens.IN (584, 0),
            Tokens.IF (587, 0),
            Tokens.ID (("any"), 590, 0),
            Tokens.DOT (593, 0),
            Tokens.ID (("any"), 594, 0),
            Tokens.THEN (607, 0),
            Tokens.ID (("list"), 612, 0),
            Tokens.LBRACE (616, 0),
            Tokens.ID (("first"), 617, 0),
            Tokens.EQ (622, 0),
            Tokens.ID (("i"), 623, 0),
            Tokens.COMMA (624, 0),
            Tokens.ID (("rest"), 625, 0),
            Tokens.EQ (629, 0),
            Tokens.ID (("readlist"), 630, 0),
            Tokens.LPAREN (638, 0),
            Tokens.RPAREN (639, 0),
            Tokens.RBRACE (640, 0),
            Tokens.ELSE (651, 0),
            Tokens.NIL (656, 0),
            Tokens.END (664, 0),
            Tokens.FUNCTION (670, 0),
            Tokens.ID (("merge"), 679, 0),
            Tokens.LPAREN (684, 0),
            Tokens.ID (("a"), 685, 0),
            Tokens.COLON (686, 0),
            Tokens.ID (("list"), 688, 0),
            Tokens.COMMA (692, 0),
            Tokens.ID (("b"), 694, 0),
            Tokens.COLON (695, 0),
            Tokens.ID (("list"), 697, 0),
            Tokens.RPAREN (701, 0),
            Tokens.COLON (703, 0),
            Tokens.ID (("list"), 705, 0),
            Tokens.EQ (710, 0),
            Tokens.IF (715, 0),
            Tokens.ID (("a"), 718, 0),
            Tokens.EQ (719, 0),
            Tokens.NIL (720, 0),
            Tokens.THEN (724, 0),
            Tokens.ID (("b"), 729, 0),
            Tokens.ELSE (734, 0),
            Tokens.IF (739, 0),
            Tokens.ID (("b"), 742, 0),
            Tokens.EQ (743, 0),
            Tokens.NIL (744, 0),
            Tokens.THEN (748, 0),
            Tokens.ID (("a"), 753, 0),
            Tokens.ELSE (758, 0),
            Tokens.IF (763, 0),
            Tokens.ID (("a"), 766, 0),
            Tokens.DOT (767, 0),
            Tokens.ID (("first"), 768, 0),
            Tokens.LT (774, 0),
            Tokens.ID (("b"), 776, 0),
            Tokens.DOT (777, 0),
            Tokens.ID (("first"), 778, 0),
            Tokens.THEN (791, 0),
            Tokens.ID (("list"), 796, 0),
            Tokens.LBRACE (800, 0),
            Tokens.ID (("first"), 801, 0),
            Tokens.EQ (806, 0),
            Tokens.ID (("a"), 807, 0),
            Tokens.DOT (808, 0),
            Tokens.ID (("first"), 809, 0),
            Tokens.COMMA (814, 0),
            Tokens.ID (("rest"), 815, 0),
            Tokens.EQ (819, 0),
            Tokens.ID (("merge"), 820, 0),
            Tokens.LPAREN (825, 0),
            Tokens.ID (("a"), 826, 0),
            Tokens.DOT (827, 0),
            Tokens.ID (("rest"), 828, 0),
            Tokens.COMMA (832, 0),
            Tokens.ID (("b"), 833, 0),
            Tokens.RPAREN (834, 0),
            Tokens.RBRACE (835, 0),
            Tokens.ELSE (843, 0),
            Tokens.ID (("list"), 848, 0),
            Tokens.LBRACE (852, 0),
            Tokens.ID (("first"), 853, 0),
            Tokens.EQ (858, 0),
            Tokens.ID (("b"), 859, 0),
            Tokens.DOT (860, 0),
            Tokens.ID (("first"), 861, 0),
            Tokens.COMMA (866, 0),
            Tokens.ID (("rest"), 867, 0),
            Tokens.EQ (871, 0),
            Tokens.ID (("merge"), 872, 0),
            Tokens.LPAREN (877, 0),
            Tokens.ID (("a"), 878, 0),
            Tokens.COMMA (879, 0),
            Tokens.ID (("b"), 880, 0),
            Tokens.DOT (881, 0),
            Tokens.ID (("rest"), 882, 0),
            Tokens.RPAREN (886, 0),
            Tokens.RBRACE (887, 0),
            Tokens.FUNCTION (891, 0),
            Tokens.ID (("printint"), 900, 0),
            Tokens.LPAREN (908, 0),
            Tokens.ID (("i"), 909, 0),
            Tokens.COLON (910, 0),
            Tokens.ID (("int"), 912, 0),
            Tokens.RPAREN (915, 0),
            Tokens.EQ (917, 0),
            Tokens.LET (921, 0),
            Tokens.FUNCTION (925, 0),
            Tokens.ID (("f"), 934, 0),
            Tokens.LPAREN (935, 0),
            Tokens.ID (("i"), 936, 0),
            Tokens.COLON (937, 0),
            Tokens.ID (("int"), 938, 0),
            Tokens.RPAREN (941, 0),
            Tokens.EQ (943, 0),
            Tokens.IF (945, 0),
            Tokens.ID (("i"), 948, 0),
            Tokens.GT (949, 0),
            Tokens.INT ((0), 950, 0),
            Tokens.THEN (959, 0),
            Tokens.LPAREN (964, 0),
            Tokens.ID (("f"), 965, 0),
            Tokens.LPAREN (966, 0),
            Tokens.ID (("i"), 967, 0),
            Tokens.DIVIDE (968, 0),
            Tokens.INT ((10), 969, 0),
            Tokens.RPAREN (971, 0),
            Tokens.SEMICOLON (972, 0),
            Tokens.ID (("print"), 974, 0),
            Tokens.LPAREN (979, 0),
            Tokens.ID (("chr"), 980, 0),
            Tokens.LPAREN (983, 0),
            Tokens.ID (("i"), 984, 0),
            Tokens.MINUS (985, 0),
            Tokens.ID (("i"), 986, 0),
            Tokens.DIVIDE (987, 0),
            Tokens.INT ((10), 988, 0),
            Tokens.TIMES (990, 0),
            Tokens.INT ((10), 991, 0),
            Tokens.PLUS (993, 0),
            Tokens.ID (("ord"), 994, 0),
            Tokens.LPAREN (997, 0),
            Tokens.STRING (("0"), 998, 0),
            Tokens.RPAREN (1001, 0),
            Tokens.RPAREN (1002, 0),
            Tokens.RPAREN (1003, 0),
            Tokens.RPAREN (1004, 0),
            Tokens.IN (1009, 0),
            Tokens.IF (1012, 0),
            Tokens.ID (("i"), 1015, 0),
            Tokens.LT (1016, 0),
            Tokens.INT ((0), 1017, 0),
            Tokens.THEN (1019, 0),
            Tokens.LPAREN (1024, 0),
            Tokens.ID (("print"), 1025, 0),
            Tokens.LPAREN (1030, 0),
            Tokens.STRING (("-"), 1031, 0),
            Tokens.RPAREN (1034, 0),
            Tokens.SEMICOLON (1035, 0),
            Tokens.ID (("f"), 1037, 0),
            Tokens.LPAREN (1038, 0),
            Tokens.MINUS (1039, 0),
            Tokens.ID (("i"), 1040, 0),
            Tokens.RPAREN (1041, 0),
            Tokens.RPAREN (1042, 0),
            Tokens.ELSE (1050, 0),
            Tokens.IF (1055, 0),
            Tokens.ID (("i"), 1058, 0),
            Tokens.GT (1059, 0),
            Tokens.INT ((0), 1060, 0),
            Tokens.THEN (1062, 0),
            Tokens.ID (("f"), 1067, 0),
            Tokens.LPAREN (1068, 0),
            Tokens.ID (("i"), 1069, 0),
            Tokens.RPAREN (1070, 0),
            Tokens.ELSE (1078, 0),
            Tokens.ID (("print"), 1083, 0),
            Tokens.LPAREN (1088, 0),
            Tokens.STRING (("0"), 1089, 0),
            Tokens.RPAREN (1092, 0),
            Tokens.END (1096, 0),
            Tokens.FUNCTION (1102, 0),
            Tokens.ID (("printlist"), 1111, 0),
            Tokens.LPAREN (1120, 0),
            Tokens.ID (("l"), 1121, 0),
            Tokens.COLON (1122, 0),
            Tokens.ID (("list"), 1124, 0),
            Tokens.RPAREN (1128, 0),
            Tokens.EQ (1130, 0),
            Tokens.IF (1135, 0),
            Tokens.ID (("l"), 1138, 0),
            Tokens.EQ (1139, 0),
            Tokens.NIL (1140, 0),
            Tokens.THEN (1144, 0),
            Tokens.ID (("print"), 1149, 0),
            Tokens.LPAREN (1154, 0),
            Tokens.STRING (("\n"), 1155, 0),
            Tokens.RPAREN (1159, 0),
            Tokens.ELSE (1164, 0),
            Tokens.LPAREN (1169, 0),
            Tokens.ID (("printint"), 1170, 0),
            Tokens.LPAREN (1178, 0),
            Tokens.ID (("l"), 1179, 0),
            Tokens.DOT (1180, 0),
            Tokens.ID (("first"), 1181, 0),
            Tokens.RPAREN (1186, 0),
            Tokens.SEMICOLON (1187, 0),
            Tokens.ID (("print"), 1189, 0),
            Tokens.LPAREN (1194, 0),
            Tokens.STRING((" "), 1195, 0),
            Tokens.RPAREN (1198, 0),
            Tokens.SEMICOLON (1199, 0),
            Tokens.ID (("printlist"), 1201, 0),
            Tokens.LPAREN (1210, 0),
            Tokens.ID (("l"), 1211, 0),
            Tokens.DOT (1212, 0),
            Tokens.ID (("rest"), 1213, 0),
            Tokens.RPAREN (1217, 0),
            Tokens.RPAREN (1218, 0),
            Tokens.VAR (1224, 0),
            Tokens.ID (("list1"), 1228, 0),
            Tokens.ASSIGN (1234, 0),
            Tokens.ID (("readlist"), 1237, 0),
            Tokens.LPAREN (1245, 0),
            Tokens.RPAREN (1246, 0),
            Tokens.VAR (1251, 0),
            Tokens.ID (("list2"), 1255, 0),
            Tokens.ASSIGN (1261, 0),
            Tokens.LPAREN (1264, 0),
            Tokens.ID (("buffer"), 1265, 0),
            Tokens.ASSIGN (1271, 0),
            Tokens.ID (("getchar"), 1273, 0),
            Tokens.LPAREN (1280, 0),
            Tokens.RPAREN (1281, 0),
            Tokens.SEMICOLON (1282, 0),
            Tokens.ID (("readlist"), 1284, 0),
            Tokens.LPAREN (1292, 0),
            Tokens.RPAREN (1293, 0),
            Tokens.RPAREN (1294, 0),
            Tokens.IN (1328, 0),
            Tokens.ID (("printlist"), 1331, 0),
            Tokens.LPAREN (1340, 0),
            Tokens.ID (("merge"), 1341, 0),
            Tokens.LPAREN (1346, 0),
            Tokens.ID (("list1"), 1347, 0),
            Tokens.COMMA (1352, 0),
            Tokens.ID (("list2"), 1353, 0),
            Tokens.RPAREN (1358, 0),
            Tokens.RPAREN (1359, 0),
            Tokens.END (1361, 0),
            Tokens.EOF (1365, 0)
        ]
    )
]
end;

OS.Process.exit (if Test.test_all Test.test_answers then OS.Process.success
                 else OS.Process.failure)