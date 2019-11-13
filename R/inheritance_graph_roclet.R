#' @export
#' @importFrom igraph graph

inheritance_graph_roclet <- function() {
  roclet("inheritance_graph")
}

#' @export
roclet_process.roclet_inheritance_graph<- function(x, blocks, env, base_path) {
  g3 <- igraph::graph( c("John", "Jim", "Jim", "Jill", "Jill", "John")) # named vertices
  links<-data.frame(from ="John", to="Jim")
  links<-rbind(links,data.frame(from ="Jim" , to="Jill"))
  links<-rbind(links,data.frame(from ="Jill", to="John"))

  nodes<-data.frame(id="John")
  nodes<-rbind(nodes,data.frame(id ="Jim" ))
  nodes<-rbind(nodes,data.frame(id ="Jill"))
  nodes<-rbind(nodes,data.frame(id ="Karl"))

  #g2 <- igraph::graph_from_data_frame( d=links,vertices=nodes,directed=TRUE) 
  #graph_attr(g2, "layout") <- layout_with_sugiyama(g2)$layout
  #plot(g2)
  list(links=links,nodes=nodes) 
}

#' @export
#' @importFrom igraph graph_from_data_frame
#' @importFrom igraph layout_with_sugiyama
#' @importFrom igraph graph_attr<-
roclet_output.roclet_inheritance_graph<- function(x, results,  base_path,...) {
  links<-results$links
  nodes<-results$nodes
  g2 <- igraph::graph_from_data_frame( d=links,vertices=nodes,directed=TRUE) 
  graph_attr(g2, "layout") <- layout_with_sugiyama(g2)$layout
  # When the edge list has vertex names, the number of nodes is not needed
    plot(
       g2 
        ,vertex.shape='circle'
        #,vertex.size=4
        #,vertex.label=as.character(nodes$id)
        ,alpha=0.1
        ,edge.arrow.size=.4
        ,edge.arrow.width=.8
    )
}
