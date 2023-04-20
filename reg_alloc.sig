signature REG_ALLOC =
sig
    type allocation = Frame.register Temp.Table.table
    val reset : unit -> unit
    val alloc : Assem.instr list * Frame.frame -> Assem.instr list * allocation
end
