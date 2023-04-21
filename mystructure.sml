structure IntSet = BinarySetFn (struct type ord_key = int
                                       val compare = Int.compare
                                end)
structure IntMap = BinaryMapFn (struct type ord_key = int
                                       val compare = Int.compare
                                end) 
structure StringSet = BinarySetFn (struct type ord_key = string
                                          val compare = String.compare
                                   end)
structure StringMap = BinaryMapFn (struct type ord_key = string
                                          val compare = String.compare
                                   end)                              
