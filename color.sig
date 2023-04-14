signature COLOR =
sig
    type allocation = Frame.register Temp.Table.table
    val color: {interference: Liveness.igraph, initial: allocation,
                spillCost: Liveness.node -> int, registers: Frame.register list} -> allocation * Temp.temp list
end
