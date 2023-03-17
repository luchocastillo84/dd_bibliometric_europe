netPlot <-
  function(NetMatrix,
           normalize = NULL,
           n = NULL,
           degree = NULL,
           Title = "Plot",
           type = "auto",
           label = TRUE,
           labelsize = 1,
           label.cex = FALSE,
           label.color = FALSE,
           label.n = NULL,
           halo = FALSE,
           cluster = "leading_eigen",
           community.repulsion = 0.1,
           vos.path = NULL,
           size = 3,
           size.cex = FALSE,
           curved = FALSE,
           noloops = TRUE,
           remove.multiple = TRUE,
           remove.isolates = FALSE,
           weighted = NULL,
           edgesize = 1,
           edges.min = 0,
           alpha = 0.5,
           verbose = TRUE) {
    
    S <- NULL
    
    colnames(NetMatrix) <- rownames(NetMatrix) <- str_to_title(colnames(NetMatrix))
    bsk.S <- TRUE
    l <- NA
    net_groups <- list()
    #if (type == "vosviewer") {
    #  cluster <- "none"
    #}
    
    if (!is.null(normalize)) {
      S <- normalizeSimilarity(NetMatrix, type = normalize)
      bsk.S <- graph.adjacency(S, mode = "undirected", weighted = T)
    }
    
    ## legacy with version <1.9.4
    if (isTRUE(size)) {
      size <- 20
      size.cex <- T
    }
    
    if (alpha < 0 & alpha > 1) {
      alpha <- 0.5
    }
    
    # Create igraph object
    bsk.network <-
      graph.adjacency(NetMatrix, mode = "undirected", weighted = weighted)
    
    
    # vertex labels
    V(bsk.network)$name <- colnames(NetMatrix)
    
    # node degree plot
    #deg <- igraph::degree_distribution(bsk.network, cumulative=T, mode="all")
    deg <- degree(bsk.network, mode = "all")
    deg.dist <- data.frame(node=V(bsk.network)$name, degree=deg) %>% 
      arrange(desc(.data$degree)) %>% 
      mutate(degree = .data$degree/max(.data$degree))
    
    
    # Compute node degrees (#links) and use that to set node size:
    #deg <- degree(bsk.network, mode = "all")
    V(bsk.network)$deg <- deg
    if (isTRUE(size.cex)) {
      V(bsk.network)$size <- (deg / max(deg)[1]) * size
    } else{
      V(bsk.network)$size <- rep(size, length(V(bsk.network)))
    }
    
    # label size
    if (isTRUE(label.cex)) {
      lsize <- log(1 + (deg / max(deg)[1])) * labelsize
      lsize[lsize < 0.5] <- 0.5  ### min labelsize is fixed to 0.5
      V(bsk.network)$label.cex <- lsize
    } else{
      V(bsk.network)$label.cex <- labelsize
    }
    
    # Select number of vertices to plot
    if (!is.null(degree)) {
      Deg <- deg - diag(NetMatrix)
      Vind <- Deg < degree
      if (sum(!Vind) == 0) {
        cat("\ndegree argument is to high!\n\n")
        return()
      }
      bsk.network <- delete.vertices(bsk.network, which(Vind))
      if (!isTRUE(bsk.S)) {
        bsk.S <- delete.vertices(bsk.S, which(Vind))
      }
    } else if (!is.null(n)) {
      if (n > dim(NetMatrix)[1]) {
        n <- dim(NetMatrix)[1]
      }
      nodes <- names(sort(deg, decreasing = TRUE)[1:n])
      
      bsk.network <- delete.vertices(bsk.network, which(!(V(bsk.network)$name %in% nodes)))
      if (!isTRUE(bsk.S)) {
        bsk.S <- delete.vertices(bsk.S,  which(!(V(bsk.S)$name %in% nodes)))
      }
    }
    
    # Remove loops and multiple edges
    if (edges.min > 1) {
      remove.multiple = FALSE
    }
    bsk.network <-
      simplify(bsk.network,
               remove.multiple = remove.multiple,
               remove.loops = noloops)
    if (!isTRUE(bsk.S)) {
      bsk.S <-
        simplify(bsk.S,
                 remove.multiple = remove.multiple,
                 remove.loops = noloops)
    }
    
    ### graph to write in pajek format ###
    bsk.save <- bsk.network
    V(bsk.save)$id <- V(bsk.save)$name
    
    ###
    
    # Clustering
    #if (type != "vosviewer") {
    ## Edge size
    E(bsk.network)$num <- E(bsk.save)$num <- count_multiple(bsk.network, eids = E(bsk.network))
    if (is.null(weighted)) {
      E(bsk.save)$weight <- E(bsk.save)$num
    }
    
    if (!is.null(weighted)) {
      E(bsk.network)$width <-
        (E(bsk.network)$weight + min(E(bsk.network)$weight)) / max(E(bsk.network)$weight + min(E(bsk.network)$weight)) *
        edgesize
    } else{
      if (isTRUE(remove.multiple)) {
        E(bsk.network)$width <- edgesize
      }
      else{
        edges <- E(bsk.network)$num
        E(bsk.network)$width <- edges / max(edges) * edgesize
      }
    }
    
    bsk.network <- delete.edges(bsk.network, which(E(bsk.network)$num < edges.min))
    if (!isTRUE(bsk.S)) {
      bsk.S <- delete.edges(bsk.S, which(E(bsk.network)$num < edges.min))
    }
    
    # delete not linked vertices
    if (isTRUE(remove.isolates)) {
      bsk.network <- delete.isolates(bsk.network, mode = 'all')
      if (!isTRUE(bsk.S)) {
        bsk.S <-
          delete.vertices(bsk.S, which(V(bsk.S)$name %in% setdiff(
            V(bsk.S)$name, V(bsk.network)$name
          )))
      }
    }
    
    
    
    
    # Community Detection
    
    cl <- clusteringNetwork(bsk.network, cluster)
    
    bsk.network <- cl$bsk.network
    if (!isTRUE(bsk.S)) {
      V(bsk.S)$color <-  V(bsk.network)$color
      V(bsk.S)$community <- V(bsk.network)$community
    }
    net_groups <- cl$net_groups
    
    # Choose Network layout
    if (!isTRUE(bsk.S)) {
      layout_results <- switchLayout(bsk.S, type, community.repulsion)
      bsk.S <- layout_results$bsk.S
    } else{
      layout_results <- switchLayout(bsk.network, type, community.repulsion)
      bsk.network <- layout_results$bsk.network
    }
    l <- layout_results$l
    
    ## Labelling the network
    LABEL = ""
    if (isTRUE(label)) {
      LABEL <- V(bsk.network)$name
      if (!is.null(label.n)) {
        q <- 1 - (label.n / length(V(bsk.network)$deg))
        if (q <= 0) {
          #LABEL <- rep("", length(LABEL))
          V(bsk.network)$labelsize <- 10
        } else {
          if (q > 1) {
            q <- 1
          }
          q <- quantile(V(bsk.network)$deg, q)
          LABEL[V(bsk.network)$deg < q] <- ""
          V(bsk.network)$labelsize <- 10
          V(bsk.network)$labelsize[V(bsk.network)$deg < q] <- 0
        }
      }
    }
    
    
    
    
    if (isTRUE(label.color)) {
      lab.color <- V(bsk.network)$color
    } else{
      lab.color <- "black"
    }
    
    
    
    #l <- layout.norm(l)
    
    ## Setting Network Attributes
    igraph::graph_attr(bsk.network, "alpha") <- alpha
    igraph::graph_attr(bsk.network, "ylim") <- c(-1,1)
    igraph::graph_attr(bsk.network, "xlim") <- c(-1,1)
    igraph::graph_attr(bsk.network, "rescale") <- TRUE
    igraph::graph_attr(bsk.network, "asp") <- 0
    igraph::graph_attr(bsk.network, "layout") <- l
    igraph::graph_attr(bsk.network, "main") <- Title
    E(bsk.network)$curved = curved
    V(bsk.network)$label.dist = 0.7
    V(bsk.network)$frame.color = adjustcolor('black', alpha)
    V(bsk.network)$color <- adjustcolor(V(bsk.network)$color, alpha)
    V(bsk.network)$label.color <- adjustcolor('black', min(c(1, alpha + 0.1)))
    V(bsk.network)$label.font = 2
    V(bsk.network)$label = LABEL
    
    
    ## Plot the network
    if (isTRUE(halo) & cluster != "none") {
      
      if (isTRUE(verbose)){plot(net_groups,bsk.network)}
      
    } else{
      E(bsk.network)$color = adjustcolor(E(bsk.network)$color, alpha / 2)
      
      if (isTRUE(verbose)){plot(bsk.network)}
      
    }
    
    ## Output
    if (cluster != "none") {
      cluster_res <- data.frame(net_groups$names,
                                net_groups$membership,
                                as.numeric(betweenness(
                                  bsk.network, directed = F, normalized = F
                                )),
                                suppressWarnings(as.numeric(closeness(
                                  bsk.network))),
                                as.numeric(page.rank(bsk.network)$vector))
      names(cluster_res) <- c("vertex", "cluster", "btw_centrality", "clos_centrality","pagerank_centrality")
      cluster_res <- cluster_res[order(cluster_res$cluster), ]
    } else {
      cluster_res <- NA
    }
    
    params <- list(normalize = normalize,
                   n = n,
                   degree = degree,
                   Title = Title,
                   type = type,
                   label = label,
                   labelsize = labelsize,
                   label.cex = label.cex,
                   label.color = label.color,
                   label.n = label.n,
                   halo = halo,
                   cluster = cluster,
                   community.repulsion = community.repulsion,
                   vos.path = vos.path,
                   size = size,
                   size.cex = size.cex,
                   curved = curved,
                   noloops = noloops,
                   remove.multiple = remove.multiple,
                   remove.isolates = remove.isolates,
                   weighted = weighted,
                   edgesize = edgesize,
                   edges.min = edges.min,
                   alpha = alpha,
                   verbose = verbose)
    params <- data.frame(params=names(unlist(params)),values=unlist(params), row.names = NULL)
    
    net <- list(
      graph = bsk.network,
      graph_pajek = bsk.save,
      cluster_obj = net_groups,
      cluster_res = cluster_res,
      community_obj = cl$net_groups,
      layout = l,
      S = S,
      nodeDegree = deg.dist,
      params = params
    )
    
    return(net)
  }






### internal functions:

### deleteIsolates

delete.isolates <- function(graph, mode = 'all') {
  isolates <- which(degree(graph, mode = mode) == 0) - 1
  delete.vertices(graph, names(isolates))
}

### clusteringNetwork

clusteringNetwork <- function(bsk.network, cluster) {
  colorlist <- c(
    brewer.pal(9, 'Set1')[-6],
    brewer.pal(8, 'Set2')[-7],
    brewer.pal(12, 'Paired')[-11],
    brewer.pal(12, 'Set3')[-c(2, 8, 12)]
  )
  
  switch(
    cluster,
    none = {
      net_groups = list(membership = rep(1, vcount(bsk.network)))
    },
    optimal = {
      net_groups <- cluster_optimal(bsk.network)
    },
    leiden = {
      net_groups <- cluster_leiden(bsk.network, objective_function = "modularity",
                                   n_iterations = 3, resolution_parameter = 0.75)
    },
    louvain = {
      net_groups <- cluster_louvain(bsk.network)
      #net_groups <- louvain(bsk.network)
    },
    fast_greedy = {
      net_groups <- cluster_fast_greedy(bsk.network)
    },
    leading_eigen = {
      net_groups <- cluster_leading_eigen(bsk.network)
    },
    spinglass = {
      net_groups <- cluster_spinglass(bsk.network)
    },
    infomap = {
      net_groups <- cluster_infomap(bsk.network)
    },
    edge_betweenness = {
      net_groups <- cluster_edge_betweenness(bsk.network)
    },
    walktrap = {
      net_groups <- cluster_walktrap(bsk.network)
    },
    
    ## default statement
    {
      cat("\nUnknown cluster argument. Using default algorithm\n")
      net_groups <- cluster_walktrap(bsk.network)
    }
  )
  
  V(bsk.network)$color <- colorlist[net_groups$membership]
  
  ### set egde intra-class colors
  V(bsk.network)$community <- net_groups$membership
  El <- as.data.frame(get.edgelist(bsk.network, names = F))
  
  colorlist <- c(
    brewer.pal(9, 'Set1')[-6],
    brewer.pal(8, 'Set2')[-7],
    brewer.pal(12, 'Paired')[-11],
    brewer.pal(12, 'Set3')[-c(2, 8, 12)]
  )
  E(bsk.network)$color <- apply(El, 1, function(x) {
    if (V(bsk.network)$community[x[1]] == V(bsk.network)$community[x[2]]) {
      C <- colorlist[V(bsk.network)$community[x[1]]]
    } else{
      C <- 'gray70'
    }
    return(C)
  })
  E(bsk.network)$lty <- 1
  E(bsk.network)$lty[E(bsk.network)$color == "gray70"] <- 5
  ### end
  
  cl <- list()
  cl$bsk.network <- bsk.network
  cl$net_groups <- net_groups
  return(cl)
}

### switchLayout

switchLayout <- function(bsk.network, type, community.repulsion) {
  
  if (community.repulsion>0){
    community.repulsion = round(community.repulsion*100)
    row <- get.edgelist(bsk.network)
    membership <- V(bsk.network)$community
    names(membership) <- V(bsk.network)$name
    
    if (is.null(E(bsk.network)$weight[1])){
      E(bsk.network)$weight <-  apply(row,1,weight.community,membership,community.repulsion,1)
    } else {
      E(bsk.network)$weight <- E(bsk.network)$weight + apply(row,1,weight.community,membership,community.repulsion,1)
      
    }
  }
  
  switch(
    type,
    auto = {
      l <- layout.auto(bsk.network)
    },
    circle = {
      l <- layout.circle(bsk.network)
    },
    star = {
      l <- layout.star(bsk.network)
    },
    sphere = {
      l <- layout.sphere(bsk.network)
    },
    mds = {
      l <- layout.mds(bsk.network)
    },
    fruchterman = {
      l <- layout.fruchterman.reingold(bsk.network)
    },
    kamada = {
      l <- layout.kamada.kawai(bsk.network)
    }
  )
  l <- layout.norm(l)
  
  layout_results <- list(l=l, bsk.network=bsk.network)
  return(layout_results)
}


# Community repulsion

weight.community=function(row,membership,weigth.within,weight.between){
  if(as.numeric(membership[which(names(membership)==row[1])])==as.numeric(membership[which(names(membership)==row[2])])){
    weight=weigth.within
  }else{
    weight=weight.between
  }
  return(weight)
}

# # louvain community detection
# louvain <- function(g){
#   cl <- 50 %>% 
#     rerun(cluster_louvain(g)$membership) %>% 
#     do.call(rbind, .) %>%
#     data.frame() %>% 
#     summarize_all(Mode)
#   cl <- list(membership=as.numeric(cl), names=V(g)$name)
#   return(cl)
# }
