#' Plot decision tree
#' @description
#' This function is used to plot decision tree graph given the output of \code{buildSubTree()}. The arguments of this function include the input list from \code{buildSubTree()}, node shape, arrow size, node size fit and whether it is a circular tree graph. In order to use the plotting function in package igraph, the judgments and directions of each nodes as well as edge directions are needed. Since the format of the input is lists nested by lists as the tree grows, this function first unlists and create a new vector including judgements, classifications and their positions. Then, the directions of each node are created from the vector above. 'x' represents left and 'y' represents right. After using node directions to create edge directions, everything that \code{plot()} function in igraph package requires is created. The final step of this function is to plot the decision tree based on the results above. In addition, some arguments, like arrow size and node shape, are added to improve the basic tree graph.
#' @param or_list A decision tree list
#' @param shape Node shape
#' @param arrow_size Size of pointing arrow
#' @param circular do you want the node shape to be circular?
#' @param node_fit Size of node frame
#' @return A tree-like graph
#' @export

dt_graph <- function(or_list,shape="rectangle",arrow_size=0.6,circular = F, node_fit=0){
  # read output(list) from decision tree to a vector
  un_list <- unlist(or_list)
  sr_name_v <- names(un_list)
  names(un_list) <- NULL
  ii<- 1
  tmp_unlist <- c()
  tmp_name <- c()
  while (ii < length(sr_name_v)){
    if (str_detect(sr_name_v[ii],'splitRule.')==T) {
      tmp_unlist <- c(tmp_unlist, paste0(un_list[ii],un_list[ii+1],un_list[ii+2]))
      tmp_name <- c(tmp_name,str_sub(sr_name_v[ii],1,(str_locate(sr_name_v[ii],'splitRule.')[2]-1)))
      ii = ii+3
    } else {
      tmp_unlist <- c(tmp_unlist,un_list[ii])
      tmp_name <- c(tmp_name,sr_name_v[ii])
      ii = ii+1
    }
  }
  names(tmp_unlist) <- tmp_name

  # hf gives the directions until each node for above vector (x represents left and y represents right)
  r_node <- tmp_unlist[is.na(tmp_unlist)==F] # subset node
  name_v <- names(r_node)
  names(r_node) <- NULL
  hf <- c('r')
  for (ii in 2:length(r_node)) {
    vec <- name_v[ii]
    element <- 'r'
    while (substr(vec,1,7)=='childT.' || substr(vec,1,7)=='childF.')
    {
      if (substr(vec,1,7)=='childT.') element <- paste0(element,'x') else element <- paste0(element,'y')

      vec <- substr(vec,8,nchar(vec))
    }
    hf <- c(hf,element)
  }

  # Create edge directions
  edges<-c()
  for(i in 1:(length(hf)-1))
  {
    bch<-hf[i]
    for (j in (i+1):length(hf))
    {
      ch<-hf[j]
      if ((substr(ch,1,nchar(bch))==bch)&&(nchar(ch)==(nchar(bch)+1)))
      {edges<-c(edges,i,j)}
    }
  }

  # Plotting
  g <- graph.empty (length(hf), directed = T) #creating empty plot
  g<-add.edges(g, edges) #add edges
  V(g)$name <- r_node
  tmp_v <- sapply(V(g)$name,nchar)
  names(tmp_v) <- NULL
  V(g)$size <- (tmp_v*node_fit+6)
  par(mar = c(0,0,0,0), ps=15,cex=0.6 )
  V(g)$color="white"
  plot(g, layout = layout.reingold.tilford(g, root = 1, flip.y = T, circular = circular),
       vertex.size=V(g)$size, vertex.shape=shape,edge.arrow.size=arrow_size)
}



