#' @export
#' @importFrom igraph graph

inheritance_graph_roclet <- function() {
  roclet("inheritance_graph")
}

#' @export
roclet_process.roclet_inheritance_graph<- function(x, blocks, env, base_path) {
  cl_names<-methods::getClasses(env)
  cl_reps<-lapply(
    cl_names,
    function(name){getClass(name,where=env)}
  )
  links<-Reduce(
    rbind,
    lapply(
      cl_reps,
      function(rep){
        cl_name<-as.character(attr(rep,'className'))
        Reduce(
          rbind,
          lapply(
            names(attr(rep,'subclasses')),
            function(sub_cl_name){
              data.frame(from=cl_name,to=sub_cl_name)}
            )
        )
      }
    )
  )
  
  nodes<-Reduce(
    rbind,
    lapply(
      cl_names,
      function(cl_name) data.frame(id=cl_name)
    )
  )

  #links<-data.frame(from ="John", to="Jim")
  #links<-rbind(links,data.frame(from ="Jim" , to="Jill"))
  #links<-rbind(links,data.frame(from ="Jill", to="John"))

  #nodes<-data.frame(id="John")
  #nodes<-rbind(nodes,data.frame(id ="Jim" ))
  #nodes<-rbind(nodes,data.frame(id ="Jill"))
  #nodes<-rbind(nodes,data.frame(id ="Karl"))

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
  igraph::graph_attr(g2, "layout") <- layout_with_sugiyama(g2)$layout
  # When the edge list has vertex names, the number of nodes is not needed
    #dir_path<-file.path(base_path,'inst','doc')
    dir_path<-file.path(base_path,'vignettes')
    if(!file.exists(dir_path)){
      dir.create(path=dir_path,recursive=TRUE)
    }
    graph_file_name<-'InheretanceGraph.pdf'
    p<-file.path(dir_path,graph_file_name)
    p2<-file.path(dir_path,'.install_extras')
    if(!file.exists(p2)){
      file.create(p2)
      Sys.sleep(1)
    }
    l<-read_lines(p2)
    l<-append(l,paste0('^',graph_file_name,'$'))
    write_lines(l,p2)

    #browser()
    pdf(p)
      plot(
         g2 
          ,vertex.shape='circle'
          #,vertex.size=4
          #,vertex.label=as.character(nodes$id)
          ,alpha=0.1
          ,edge.arrow.size=.4
          ,edge.arrow.width=.8
      )
  dev.off()
}
