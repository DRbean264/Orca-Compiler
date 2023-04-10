signature LIVENESS =
sig
    type graph
    type node
    type live
    type liveInfo = {livein: live,
                     liveout: live}
    datatype igraph =
             IGRAPH of {graph: graph,
                        tnode: Temp.temp -> node,
                        gtemp: node -> Temp.temp,
                        moves: (node * node) list}
    val interferenceGraph : (Flow.flowInfo Flow.Graph.graph * Flow.flowInfo Flow.Graph.node list) ->
                            igraph * (Flow.flowInfo Flow.Graph.node -> Temp.temp list)
    val show : igraph -> unit
    val show' : TextIO.outstream * igraph -> unit

    (* Debugging *)
    val constructLiveMap : Flow.flowInfo Flow.Graph.graph * Flow.flowInfo Flow.Graph.node list -> liveInfo IntMap.map
    val displayLiveMap : (Assem.temp -> string) -> liveInfo IntMap.map * Flow.flowInfo Flow.Graph.node list -> unit
end
