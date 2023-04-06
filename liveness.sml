signature Liveness =
sig
    datatype igraph =
             IGRAPH of {graph: IGraph.graph,
                        tnode: Temp.temp -> IGraph.node,
                        gtemp: IGraph.node -> Temp.temp,
                        moves: (IGraph.node * IGraph.node) list}
    val interferenceGraph : Flow.flowInfo Flow.Graph.graph ->
                            igraph * (Flow.Graph.node -> Temp.temp list)
    val show : outstream * igraph -> unit
end
