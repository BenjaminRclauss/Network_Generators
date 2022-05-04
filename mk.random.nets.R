
######### setup and functions ############
library(igraph)
library(combinat)
motifs <- c(30,21,38,28,39,29,37,20,31,12,4,1,2,35,44,11,8,9,36,32,26,3,14,15,43,41,10,5,23,6,33,13,19,17,18,34,7,16,25,0)
get_motif <- function(number){
  A_1 <- matrix(c(0,1,0,0), nrow = 2, ncol = 2)
  A_2 <- matrix(c(1,1,0,0), nrow = 2, ncol = 2)
  A_3 <- matrix(c(1,1,0,1), nrow = 2, ncol = 2)
  A_4 <- matrix(c(0,1,0,1), nrow = 2, ncol = 2)
  A_5 <- matrix(c(2,1,0,0), nrow = 2, ncol = 2)
  A_6 <- matrix(c(0,1,0,2), nrow = 2, ncol = 2)
  A_7 <- matrix(c(2,1,0,2), nrow = 2, ncol = 2)
  A_8 <- matrix(c(1,1,0,2), nrow = 2, ncol = 2)
  A_9 <- matrix(c(2,1,0,1), nrow = 2, ncol = 2)
  
  ##B 10 : 18
  B_1 <- matrix(c(0,2,0,0), nrow = 2, ncol = 2)
  B_2 <- matrix(c(1,2,0,0), nrow = 2, ncol = 2)
  B_3 <- matrix(c(1,2,0,1), nrow = 2, ncol = 2)
  B_4 <- matrix(c(0,2,0,1), nrow = 2, ncol = 2)
  B_5 <- matrix(c(2,2,0,0), nrow = 2, ncol = 2)
  B_6 <- matrix(c(0,2,0,2), nrow = 2, ncol = 2)
  B_7 <- matrix(c(2,2,0,2), nrow = 2, ncol = 2)
  B_8 <- matrix(c(1,2,0,2), nrow = 2, ncol = 2)
  B_9 <- matrix(c(2,2,0,1), nrow = 2, ncol = 2)
  
  #C 19 : 27
  C_1 <- matrix(c(0,1,1,0), nrow = 2, ncol = 2)
  C_2 <- matrix(c(1,1,1,0), nrow = 2, ncol = 2)
  C_3 <- matrix(c(1,1,1,1), nrow = 2, ncol = 2)
  C_4 <- matrix(c(0,1,1,1), nrow = 2, ncol = 2)
  C_5 <- matrix(c(2,1,1,0), nrow = 2, ncol = 2)
  C_6 <- matrix(c(0,1,1,2), nrow = 2, ncol = 2)
  C_7 <- matrix(c(2,1,1,2), nrow = 2, ncol = 2)
  C_8 <- matrix(c(1,1,1,2), nrow = 2, ncol = 2)
  C_9 <- matrix(c(2,1,1,1), nrow = 2, ncol = 2)
  
  #D 28 : 36
  D_1 <- matrix(c(0,1,2,0), nrow = 2, ncol = 2)
  D_2 <- matrix(c(1,1,2,0), nrow = 2, ncol = 2)
  D_3 <- matrix(c(1,1,2,1), nrow = 2, ncol = 2)
  D_4 <- matrix(c(0,1,2,1), nrow = 2, ncol = 2)
  D_5 <- matrix(c(2,1,2,0), nrow = 2, ncol = 2)
  D_6 <- matrix(c(0,1,2,2), nrow = 2, ncol = 2)
  D_7 <- matrix(c(2,1,2,2), nrow = 2, ncol = 2)
  D_8 <- matrix(c(1,1,2,2), nrow = 2, ncol = 2)
  D_9 <- matrix(c(2,1,2,1), nrow = 2, ncol = 2)
  
  #E 37 : 45
  E_1 <- matrix(c(0,2,2,0), nrow = 2, ncol = 2)
  E_2 <- matrix(c(1,2,2,0), nrow = 2, ncol = 2)
  E_3 <- matrix(c(1,2,2,1), nrow = 2, ncol = 2)
  E_4 <- matrix(c(0,2,2,1), nrow = 2, ncol = 2)
  E_5 <- matrix(c(2,2,2,0), nrow = 2, ncol = 2)
  E_6 <- matrix(c(0,2,2,2), nrow = 2, ncol = 2)
  E_7 <- matrix(c(2,2,2,2), nrow = 2, ncol = 2)
  E_8 <- matrix(c(1,2,2,2), nrow = 2, ncol = 2)
  E_9 <- matrix(c(2,2,2,1), nrow = 2, ncol = 2)
  
  #F 46 : 54
  F_1 <- matrix(c(0,2,1,0), nrow = 2, ncol = 2)
  F_2 <- matrix(c(1,2,1,0), nrow = 2, ncol = 2)
  F_3 <- matrix(c(1,2,1,1), nrow = 2, ncol = 2)
  F_4 <- matrix(c(0,2,1,1), nrow = 2, ncol = 2)
  F_5 <- matrix(c(2,2,1,0), nrow = 2, ncol = 2)
  F_6 <- matrix(c(0,2,1,2), nrow = 2, ncol = 2)
  F_7 <- matrix(c(2,2,1,2), nrow = 2, ncol = 2)
  F_8 <- matrix(c(1,2,1,2), nrow = 2, ncol = 2)
  F_9 <- matrix(c(2,2,1,1), nrow = 2, ncol = 2)
  
  # G 55:63
  G_1 <- matrix(c(0,0,1,0), nrow = 2, ncol = 2)
  G_2 <- matrix(c(1,0,1,0), nrow = 2, ncol = 2)
  G_3 <- matrix(c(1,0,1,1), nrow = 2, ncol = 2)
  G_4 <- matrix(c(0,0,1,1), nrow = 2, ncol = 2)
  G_5 <- matrix(c(2,0,1,0), nrow = 2, ncol = 2)
  G_6 <- matrix(c(0,0,1,2), nrow = 2, ncol = 2)
  G_7 <- matrix(c(2,0,1,2), nrow = 2, ncol = 2)
  G_8 <- matrix(c(1,0,1,2), nrow = 2, ncol = 2)
  G_9 <- matrix(c(2,0,1,1), nrow = 2, ncol = 2)
  
  #H 64:72
  H_1 <- matrix(c(0,0,2,0), nrow = 2, ncol = 2)
  H_2 <- matrix(c(1,0,2,0), nrow = 2, ncol = 2)
  H_3 <- matrix(c(1,0,2,1), nrow = 2, ncol = 2)
  H_4 <- matrix(c(0,0,2,1), nrow = 2, ncol = 2)
  H_5 <- matrix(c(2,0,2,0), nrow = 2, ncol = 2)
  H_6 <- matrix(c(0,0,2,2), nrow = 2, ncol = 2)
  H_7 <- matrix(c(2,0,2,2), nrow = 2, ncol = 2)
  H_8 <- matrix(c(1,0,2,2), nrow = 2, ncol = 2)
  H_9 <- matrix(c(2,0,2,1), nrow = 2, ncol = 2)
  
  k <- 0
  motif_list <- list()
  for(i in 1:8){
    for (j in 1:9){
      k <- k + 1
      motif_list[k] <- paste0(LETTERS[i], "_", j)
    }
  }
  
  tmp <- get(motif_list[[number]])
  colnames(tmp) <- c("A", "B")
  rownames(tmp) <- c("A", "B")
  tmp2 <- as.data.frame(matrix(ncol = 4))
  colnames(tmp2) <- c("from", "to", "arrows.to.type", "color")
  k <- 0
  for (i in 1:2){
    for (j in 1:2){
      if (tmp[[i,j]] == 1){
        k <- k+1
        tmp2[k,1] <- colnames(tmp)[j]
        tmp2[k,2] <- rownames(tmp)[i]
        tmp2[k,3] <- "arrow"
        tmp2[k,4] <- "blue"
      } else {
        if (tmp[[i,j]] == 2){
          k <- k+1
          tmp2[k,1] <- colnames(tmp)[j]
          tmp2[k,2] <- rownames(tmp)[i]
          tmp2[k,3] <- "circle"
          tmp2[k,4] <- "red"
        }
      }
    }
  }
  #tmp2
  edges <- tmp2
  nodes <- data.frame(id = c("A", "B"), label = c("A", "B"))
  return(c(nodes,edges))
}
motif.to.adj <- function(number){
  A_1 <- matrix(c(0,1,0,0), nrow = 2, ncol = 2)
  A_2 <- matrix(c(1,1,0,0), nrow = 2, ncol = 2)
  A_3 <- matrix(c(1,1,0,1), nrow = 2, ncol = 2)
  A_4 <- matrix(c(0,1,0,1), nrow = 2, ncol = 2)
  A_5 <- matrix(c(2,1,0,0), nrow = 2, ncol = 2)
  A_6 <- matrix(c(0,1,0,2), nrow = 2, ncol = 2)
  A_7 <- matrix(c(2,1,0,2), nrow = 2, ncol = 2)
  A_8 <- matrix(c(1,1,0,2), nrow = 2, ncol = 2)
  A_9 <- matrix(c(2,1,0,1), nrow = 2, ncol = 2)
  
  ##B 10 : 18
  B_1 <- matrix(c(0,2,0,0), nrow = 2, ncol = 2)
  B_2 <- matrix(c(1,2,0,0), nrow = 2, ncol = 2)
  B_3 <- matrix(c(1,2,0,1), nrow = 2, ncol = 2)
  B_4 <- matrix(c(0,2,0,1), nrow = 2, ncol = 2)
  B_5 <- matrix(c(2,2,0,0), nrow = 2, ncol = 2)
  B_6 <- matrix(c(0,2,0,2), nrow = 2, ncol = 2)
  B_7 <- matrix(c(2,2,0,2), nrow = 2, ncol = 2)
  B_8 <- matrix(c(1,2,0,2), nrow = 2, ncol = 2)
  B_9 <- matrix(c(2,2,0,1), nrow = 2, ncol = 2)
  
  #C 19 : 27
  C_1 <- matrix(c(0,1,1,0), nrow = 2, ncol = 2)
  C_2 <- matrix(c(1,1,1,0), nrow = 2, ncol = 2)
  C_3 <- matrix(c(1,1,1,1), nrow = 2, ncol = 2)
  C_4 <- matrix(c(0,1,1,1), nrow = 2, ncol = 2)
  C_5 <- matrix(c(2,1,1,0), nrow = 2, ncol = 2)
  C_6 <- matrix(c(0,1,1,2), nrow = 2, ncol = 2)
  C_7 <- matrix(c(2,1,1,2), nrow = 2, ncol = 2)
  C_8 <- matrix(c(1,1,1,2), nrow = 2, ncol = 2)
  C_9 <- matrix(c(2,1,1,1), nrow = 2, ncol = 2)
  
  #D 28 : 36
  D_1 <- matrix(c(0,1,2,0), nrow = 2, ncol = 2)
  D_2 <- matrix(c(1,1,2,0), nrow = 2, ncol = 2)
  D_3 <- matrix(c(1,1,2,1), nrow = 2, ncol = 2)
  D_4 <- matrix(c(0,1,2,1), nrow = 2, ncol = 2)
  D_5 <- matrix(c(2,1,2,0), nrow = 2, ncol = 2)
  D_6 <- matrix(c(0,1,2,2), nrow = 2, ncol = 2)
  D_7 <- matrix(c(2,1,2,2), nrow = 2, ncol = 2)
  D_8 <- matrix(c(1,1,2,2), nrow = 2, ncol = 2)
  D_9 <- matrix(c(2,1,2,1), nrow = 2, ncol = 2)
  
  #E 37 : 45
  E_1 <- matrix(c(0,2,2,0), nrow = 2, ncol = 2)
  E_2 <- matrix(c(1,2,2,0), nrow = 2, ncol = 2)
  E_3 <- matrix(c(1,2,2,1), nrow = 2, ncol = 2)
  E_4 <- matrix(c(0,2,2,1), nrow = 2, ncol = 2)
  E_5 <- matrix(c(2,2,2,0), nrow = 2, ncol = 2)
  E_6 <- matrix(c(0,2,2,2), nrow = 2, ncol = 2)
  E_7 <- matrix(c(2,2,2,2), nrow = 2, ncol = 2)
  E_8 <- matrix(c(1,2,2,2), nrow = 2, ncol = 2)
  E_9 <- matrix(c(2,2,2,1), nrow = 2, ncol = 2)
  
  #F 46 : 54
  F_1 <- matrix(c(0,2,1,0), nrow = 2, ncol = 2)
  F_2 <- matrix(c(1,2,1,0), nrow = 2, ncol = 2)
  F_3 <- matrix(c(1,2,1,1), nrow = 2, ncol = 2)
  F_4 <- matrix(c(0,2,1,1), nrow = 2, ncol = 2)
  F_5 <- matrix(c(2,2,1,0), nrow = 2, ncol = 2)
  F_6 <- matrix(c(0,2,1,2), nrow = 2, ncol = 2)
  F_7 <- matrix(c(2,2,1,2), nrow = 2, ncol = 2)
  F_8 <- matrix(c(1,2,1,2), nrow = 2, ncol = 2)
  F_9 <- matrix(c(2,2,1,1), nrow = 2, ncol = 2)
  
  # G 55:63
  G_1 <- matrix(c(0,0,1,0), nrow = 2, ncol = 2)
  G_2 <- matrix(c(1,0,1,0), nrow = 2, ncol = 2)
  G_3 <- matrix(c(1,0,1,1), nrow = 2, ncol = 2)
  G_4 <- matrix(c(0,0,1,1), nrow = 2, ncol = 2)
  G_5 <- matrix(c(2,0,1,0), nrow = 2, ncol = 2)
  G_6 <- matrix(c(0,0,1,2), nrow = 2, ncol = 2)
  G_7 <- matrix(c(2,0,1,2), nrow = 2, ncol = 2)
  G_8 <- matrix(c(1,0,1,2), nrow = 2, ncol = 2)
  G_9 <- matrix(c(2,0,1,1), nrow = 2, ncol = 2)
  
  #H 64:72
  H_1 <- matrix(c(0,0,2,0), nrow = 2, ncol = 2)
  H_2 <- matrix(c(1,0,2,0), nrow = 2, ncol = 2)
  H_3 <- matrix(c(1,0,2,1), nrow = 2, ncol = 2)
  H_4 <- matrix(c(0,0,2,1), nrow = 2, ncol = 2)
  H_5 <- matrix(c(2,0,2,0), nrow = 2, ncol = 2)
  H_6 <- matrix(c(0,0,2,2), nrow = 2, ncol = 2)
  H_7 <- matrix(c(2,0,2,2), nrow = 2, ncol = 2)
  H_8 <- matrix(c(1,0,2,2), nrow = 2, ncol = 2)
  H_9 <- matrix(c(2,0,2,1), nrow = 2, ncol = 2)
  
  k <- 0
  motif_list <- list()
  for(i in 1:8){
    for (j in 1:9){
      k <- k + 1
      motif_list[k] <- paste0(LETTERS[i], "_", j)
    }
  }
  
  tmp <- get(motif_list[[number]])
  colnames(tmp) <- c("A", "B")
  rownames(tmp) <- c("A", "B")
  return(tmp)
}
plot.net <- function(tf_links = tf_links){
  require(visNetwork)
  tf_links <- tf_links[which(tf_links[,3] !=0),]
  topology=data.frame(as.matrix(tf_links), stringsAsFactors = F)
  node_list <- unique(c(topology[,1], topology[,2]))
  nodes <- data.frame(id = node_list,  font.size =30, value=c(rep(1,length(node_list))))
  label <- as.character(nodes$id)
  nodes <- cbind(nodes, label)
  edge_col <- data.frame(c(1,2),c("blue","darkred"))
  colnames(edge_col) <- c("relation", "color")
  arrow_type <- data.frame(c(1,2),c("arrow","circle"))
  colnames(arrow_type) <- c("type", "color")
  edges <- data.frame(from =c(topology[,1]), to = c(topology[,2])
                      , arrows.to.type	=arrow_type$color[c(as.numeric(topology[,3]))]
                      , width = 3
                      , color = edge_col$color[c(as.numeric(topology[,3]))]
  )
  visNetwork(nodes, edges, height = "1000px", width = "100%") %>%
    visEdges(arrows = "to") %>%
    visOptions(manipulation = F) %>%
    visLayout(randomSeed = 123) %>%
    visNodes(scaling = list(label = list(enabled = F))) %>%
    visPhysics(solver = "forceAtlas2Based", stabilization = FALSE)%>%
    visNodes(size = 10)
}
motif.to.circuit <- function(number){
  tmp <- get_motif(number)
  Source <- tmp$from
  Target <- tmp$to
  Type <- tmp$color
  tmp <- as.data.frame(cbind(Source,Target,Type))
  i.r <- which(tmp$Type == "red")
  i.b <- which(tmp$Type == "blue")
  Type <- vector(length = sum(length(i.r),length(i.b)))
  Type[i.r] <- 2
  Type[i.b] <- 1
  tmp$Type <- Type
  return(tmp)
}
scalefree.net.gen <- function(num.nodes, motif.choice){
  n.nodes <- num.nodes/2
  g <- sample_pa(n.nodes)
  g.adj <- as_adjacency_matrix(g, sparse = F)
  inter <- which(g.adj == 1, arr.ind = T)
  dimnames(g.adj) <- list(as.character(1:nrow(g.adj)), as.character(1:nrow(g.adj)))
  ###################expand matrix to double
  new.mat <- matrix(ncol = n.nodes*2, nrow = n.nodes*2, rep(0,n.nodes*4))
  for(i in 1: nrow(inter)){
    source = inter[i,2]
    target = inter[i,1]
    new.source = source + source
    new.target = (target - 1) +target
    new.mat[new.target, new.source] <-1
  }
  ##########################add in inhibition
  new.inter = which(new.mat == 1, arr.ind = T)
    for(i in 1:nrow(new.inter)){
      new.mat[new.inter[i,1], new.inter[i,2]] <- sample(1:2, 1, replace = T, prob = NULL)
    }
  ################# replace interactions with motifs
  for(i in 1:n.nodes){
    x = i
    y1 = (x-1)+x
    y2 = x+x
    if(length(motif.choice) == 1){
      choice <- sample(1:2, 1)
      if(choice == 1){
        new.mat[y1:y2, y1:y2] <- motif.to.adj(motif.choice)
      } else {
        new.mat[y2:y1,y2:y1] <- motif.to.adj(motif.choice)
      }
    } else {
      choice <- sample(1:2, 1)
      if(choice == 1){
        new.mat[y1:y2, y1:y2] <- motif.to.adj(sample(motif.choice,1))
      } else {
        new.mat[y2:y1,y2:y1] <- motif.to.adj(sample(motif.choice,1))
      }
    }
    
    
  }
  dimnames(new.mat) <- list(as.character(1:nrow(new.mat)), as.character(1:nrow(new.mat)))
  return(new.mat)
}
random.net.gen <- function(num.nodes, motif.choice, edge.param){
  n.nodes <- num.nodes/2
  g <- erdos.renyi.game(n.nodes, directed = T, type = "gnm", p.or.m = ((edge.param*n.nodes)/2))
  components <- igraph::clusters(g, mode="weak")
  biggest_cluster_id <- which.max(components$csize)
  vert_ids <- V(g)[components$membership == biggest_cluster_id]
  g <- igraph::induced_subgraph(g, vert_ids)
  
  
  g.adj <- as_adjacency_matrix(g, sparse = F)
  inter <- which(g.adj == 1, arr.ind = T)
  dimnames(g.adj) <- list(as.character(1:nrow(g.adj)), as.character(1:nrow(g.adj)))
  ###################expand matrix to double
  new.mat <- matrix(ncol = ncol(g.adj)*2, nrow = ncol(g.adj)*2, rep(0,ncol(g.adj)*4))
  for(i in 1: nrow(inter)){
    source = inter[i,2]
    target = inter[i,1]
    new.source = source + source
    new.target = (target - 1) +target
    new.mat[new.target, new.source] <-1
  }
  ##########################add in inhibition
  new.inter = which(new.mat == 1, arr.ind = T)
  for(i in 1:nrow(new.inter)){
    new.mat[new.inter[i,1], new.inter[i,2]] <- sample(1:2, 1, replace = T, prob = NULL)
  }
  ################# replace interactions with motifs
  for(i in 1:ncol(g.adj)){
    x = i
    y1 = (x-1)+x
    y2 = x+x
    if(length(motif.choice) == 1){
      choice <- sample(1:2, 1)
      if(choice == 1){
        new.mat[y1:y2, y1:y2] <- motif.to.adj(motif.choice)
      } else {
        new.mat[y2:y1,y2:y1] <- motif.to.adj(motif.choice)
      }
    } else {
      choice <- sample(1:2, 1)
      if(choice == 1){
        new.mat[y1:y2, y1:y2] <- motif.to.adj(sample(motif.choice,1))
      } else {
        new.mat[y2:y1,y2:y1] <- motif.to.adj(sample(motif.choice,1))
      }
    }
    
    
  }
  dimnames(new.mat) <- list(as.character(1:nrow(new.mat)), as.character(1:nrow(new.mat)))
  return(new.mat)
  
  
  
  
  
  
  
}
pairs <- function(n.nodes, motif.choice){
  seq.adj <- matrix(nrow = n.nodes, ncol = n.nodes, rep(0, n.nodes*n.nodes))
  x <- 0
  while(x < n.nodes){
    x <- x+1
    #choice <- sample(1:2, 1)
    #if(choice ==1 ){
    #  seq.adj[c(x, x+1), c(x, x+1)] <- motif.to.adj(sample(motif.choice, 1))
    #} else {
    #  seq.adj[c(x+1,x ), c(x+1,x)] <- motif.to.adj(sample(motif.choice, 1))
    #}
    seq.adj[c(x, x+1), c(x, x+1)] <- motif.to.adj(sample(motif.choice, 1))
    x <- x+1
  }
  return(seq.adj)
}
add.seq.int <- function(seq.adj, n.nodes){
  x<- 0
  while(x < n.nodes){
    x <- x+1
    node.1 <- sample(x:(x+1),1)
    x <- x+1
    if(x != n.nodes){
      y <- x +1
      node.2 <- sample(y:(y+1),1)
      if(colSums(seq.adj)[y] == 0){
        seq.adj[x,y] <- sample(1:2,1)
      } else {
        seq.adj[y,x] <- sample(1:2,1)
      }
    
    }
  }
  return(seq.adj)
}
seq.net.gen <- function(n.motifs, motif.choice){
  n.nodes <- n.motifs*2
  seq.adj <- pairs(n.nodes, motif.choice)
  dimnames(seq.adj) <- list(1:n.nodes, 1:n.nodes)
  seq.adj <- add.seq.int(seq.adj, n.nodes)
  return(seq.adj)
}
plot.adj <- function(adj){
  seq.adj <- adj
  tmp2 <- as.data.frame(matrix(ncol = 3))
  colnames(tmp2) <- c("from", "to", "arrows.to.type")
  k <- 0
  for (i in 1:ncol(seq.adj)){
    for (j in 1:ncol(seq.adj)){
      if (seq.adj[[i,j]] == 1){
        k <- k+1
        tmp2[k,1] <- colnames(seq.adj)[j]
        tmp2[k,2] <- rownames(seq.adj)[i]
        tmp2[k,3] <- 1
      } else if(seq.adj[[i,j]] == 2){
        k <- k+1
        tmp2[k,1] <- colnames(seq.adj)[j]
        tmp2[k,2] <- rownames(seq.adj)[i]
        tmp2[k,3] <- 2
      }else if(seq.adj[[i,j]] == 0){
        k <- k+1
        tmp2[k,1] <- colnames(seq.adj)[j]
        tmp2[k,2] <- rownames(seq.adj)[i]
        tmp2[k,3] <- 0
      }
    }
  }
  
  plot.net(tmp2)
}
adj.to.mean.degree <- function(n.nodes, g.adj){
  tmp2 <- as.data.frame(matrix(ncol = 3))
  colnames(tmp2) <- c("from", "to", "arrows.to.type")
  k <- 0
  for (i in 1:n.nodes){
    for (j in 1:n.nodes){
      if (g.adj[[i,j]] == 1){
        k <- k+1
        tmp2[k,1] <- colnames(g.adj)[j]
        tmp2[k,2] <- rownames(g.adj)[i]
        tmp2[k,3] <- 1
        
      } else if(g.adj[[i,j]] == 2){
        k <- k+1
        tmp2[k,1] <- colnames(g.adj)[j]
        tmp2[k,2] <- rownames(g.adj)[i]
        tmp2[k,3] <- 2
      }else if(g.adj[[i,j]] == 0){
        k <- k+1
        tmp2[k,1] <- colnames(g.adj)[j]
        tmp2[k,2] <- rownames(g.adj)[i]
        tmp2[k,3] <- 0
      }
    }
  }
  
  g.topo <- tmp2
  g.topo <- g.topo[-(which(g.topo$arrows.to.type == 0)),]
  nodes <- unique(c(g.topo$from, g.topo$to))
  degree <- lapply(nodes, function(x)(sum(g.topo$from == x)+sum(g.topo$to == x) - sum(g.topo$from ==x & g.topo$to == x)))
  degree <- as.numeric(unlist(degree))
  return(mean(degree))
}
adj.to.tpo <- function(adj){
  seq.adj <- adj
  tmp2 <- as.data.frame(matrix(ncol = 3))
  colnames(tmp2) <- c("source", "target", "type")
  k <- 0
  for (i in 1:ncol(seq.adj)){
    for (j in 1:ncol(seq.adj)){
      if (seq.adj[[i,j]] == 1){
        k <- k+1
        tmp2[k,1] <- colnames(seq.adj)[j]
        tmp2[k,2] <- rownames(seq.adj)[i]
        tmp2[k,3] <- 1
      } else if(seq.adj[[i,j]] == 2){
        k <- k+1
        tmp2[k,1] <- colnames(seq.adj)[j]
        tmp2[k,2] <- rownames(seq.adj)[i]
        tmp2[k,3] <- 2
      }else if(seq.adj[[i,j]] == 0){
        k <- k+1
        tmp2[k,1] <- colnames(seq.adj)[j]
        tmp2[k,2] <- rownames(seq.adj)[i]
        tmp2[k,3] <- 0
      }
    }
  }
  tmp2 <- tmp2[tmp2$type != 0,]
  return(tmp2)
}


######### ######### #########         top motifs              ######### ######### #########
#HH
hh.motifs <- c(30,21,39)
plot.net(motif.to.circuit(21))
plot.net(motif.to.circuit(39))
plot.net(motif.to.circuit(30))
#FF
ff.motifs <- c(1,10,19)
######### ######### #########   scale free 10 node  ######### ######### ######### 

apply.me <- rep(10,10)
sc.10.hh <- lapply(FUN = scalefree.net.gen, X = apply.me, motif.choice = hh.motifs)
sc.10.hh <- lapply(FUN = adj.to.tpo, X = sc.10.hh)
for(i in 1:length(sc.10.hh)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/ScaleFreeNetworks/sc.10.hh.", i)
  saveRDS(sc.10.hh[[i]], file = nme)
}
###

sc.10.ff <- lapply(FUN = scalefree.net.gen, X = apply.me, motif.choice = ff.motifs)
sc.10.ff <- lapply(FUN = adj.to.tpo, X = sc.10.ff)
for(i in 1:length(sc.10.ff)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/ScaleFreeNetworks/sc.10.ff.", i)
  saveRDS(sc.10.ff[[i]], file = nme)
}


sc.10.mixed <- lapply(FUN = scalefree.net.gen, X = apply.me, motif.choice = c(hh.motifs, ff.motifs))
sc.10.mixed <- lapply(FUN = adj.to.tpo, X = sc.10.mixed)
for(i in 1:length(sc.10.mixed)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/ScaleFreeNetworks/sc.10.mixed.", i)
  saveRDS(sc.10.mixed[[i]], file = nme)
}
######### ######### #########   scale free 20 node  ######### ######### ######### 

apply.me <- rep(20,10)
sc.20.hh <- lapply(FUN = scalefree.net.gen, X = apply.me, motif.choice = hh.motifs)
sc.20.hh <- lapply(FUN = adj.to.tpo, X = sc.20.hh)
for(i in 1:length(sc.20.hh)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/ScaleFreeNetworks/sc.20.hh.", i)
  saveRDS(sc.20.hh[[i]], file = nme)
}


sc.20.ff <- lapply(FUN = scalefree.net.gen, X = apply.me, motif.choice = ff.motifs)
sc.20.ff <- lapply(FUN = adj.to.tpo, X = sc.20.ff)
for(i in 1:length(sc.20.ff)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/ScaleFreeNetworks/sc.20.ff.", i)
  saveRDS(sc.20.ff[[i]], file = nme)
}



sc.20.mixed <- lapply(FUN = scalefree.net.gen, X = apply.me, motif.choice = c(hh.motifs, ff.motifs))
sc.20.mixed <- lapply(FUN = adj.to.tpo, X = sc.20.mixed)
for(i in 1:length(sc.20.mixed)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/ScaleFreeNetworks/sc.20.mixed.", i)
  saveRDS(sc.20.mixed[[i]], file = nme)
}
######### ######### #########   scale free 30 node  ######### ######### ######### 

apply.me <- rep(30,10)
sc.30.hh <- lapply(FUN = scalefree.net.gen, X = apply.me, motif.choice = hh.motifs)
sc.30.hh <- lapply(FUN = adj.to.tpo, X = sc.30.hh)
for(i in 1:length(sc.30.hh)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/ScaleFreeNetworks/sc.30.hh.", i)
  saveRDS(sc.30.hh[[i]], file = nme)
}


sc.30.ff <- lapply(FUN = scalefree.net.gen, X = apply.me, motif.choice = ff.motifs)
sc.30.ff <- lapply(FUN = adj.to.tpo, X = sc.30.ff)
for(i in 1:length(sc.30.ff)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/ScaleFreeNetworks/sc.30.ff.", i)
  saveRDS(sc.30.ff[[i]], file = nme)
}
sc.30.mixed <- lapply(FUN = scalefree.net.gen, X = apply.me, motif.choice = c(hh.motifs, ff.motifs))
sc.30.mixed <- lapply(FUN = adj.to.tpo, X = sc.30.mixed)
for(i in 1:length(sc.30.mixed)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/ScaleFreeNetworks/sc.30.mixed.", i)
  saveRDS(sc.30.mixed[[i]], file = nme)
}
######### ######### #########   scale free 40 node  ######### ######### ######### 

apply.me <- rep(40,10)
sc.40.hh <- lapply(FUN = scalefree.net.gen, X = apply.me, motif.choice = hh.motifs)
sc.40.hh <- lapply(FUN = adj.to.tpo, X = sc.40.hh)
for(i in 1:length(sc.40.hh)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/ScaleFreeNetworks/sc.40.hh.", i)
  saveRDS(sc.40.hh[[i]], file = nme)
}
sc.40.ff <- lapply(FUN = scalefree.net.gen, X = apply.me, motif.choice = ff.motifs)
sc.40.ff <- lapply(FUN = adj.to.tpo, X = sc.40.ff)
for(i in 1:length(sc.40.ff)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/ScaleFreeNetworks/sc.40.ff.", i)
  saveRDS(sc.40.ff[[i]], file = nme)
}
sc.40.mixed <- lapply(FUN = scalefree.net.gen, X = apply.me, motif.choice = c(hh.motifs, ff.motifs))
sc.40.mixed <- lapply(FUN = adj.to.tpo, X = sc.40.mixed)
for(i in 1:length(sc.40.mixed)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/ScaleFreeNetworks/sc.40.mixed.", i)
  saveRDS(sc.40.mixed[[i]], file = nme)
}
######### ######### #########   scale free 50 node  ######### ######### ######### 

apply.me <- rep(50,10)
sc.50.hh <- lapply(FUN = scalefree.net.gen, X = apply.me, motif.choice = hh.motifs)
sc.50.hh <- lapply(FUN = adj.to.tpo, X = sc.50.hh)
for(i in 1:length(sc.50.hh)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/ScaleFreeNetworks/sc.50.hh.", i)
  saveRDS(sc.50.hh[[i]], file = nme)
}


sc.50.ff <- lapply(FUN = scalefree.net.gen, X = apply.me, motif.choice = ff.motifs)
sc.50.ff <- lapply(FUN = adj.to.tpo, X = sc.50.ff)
for(i in 1:length(sc.50.ff)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/ScaleFreeNetworks/sc.50.ff.", i)
  saveRDS(sc.50.ff[[i]], file = nme)
}

sc.50.mixed <- lapply(FUN = scalefree.net.gen, X = apply.me, motif.choice = c(hh.motifs, ff.motifs))
sc.50.mixed <- lapply(FUN = adj.to.tpo, X = sc.50.mixed)
for(i in 1:length(sc.50.mixed)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/ScaleFreeNetworks/sc.50.mixed.", i)
  saveRDS(sc.50.mixed[[i]], file = nme)
}








######### ######### #########   sequential 10 node  ######### #########
apply.me <- rep(5,10)
sq.10.hh <- lapply(FUN = seq.net.gen, X = apply.me, motif.choice = hh.motifs)
sq.10.hh <- lapply(FUN = adj.to.tpo, X = sq.10.hh)
for(i in 1:length(sq.10.hh)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/SequentialNetworks/sq.10.hh.", i)
  saveRDS(sq.10.hh[[i]], file = nme)
}

sq.10.ff <- lapply(FUN = seq.net.gen, X = apply.me, motif.choice = ff.motifs)
sq.10.ff <- lapply(FUN = adj.to.tpo, X = sq.10.ff)
for(i in 1:length(sq.10.ff)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/SequentialNetworks/sq.10.ff.", i)
  saveRDS(sq.10.ff[[i]], file = nme)
}

sq.10.mixed <- lapply(FUN = seq.net.gen, X = apply.me, motif.choice = c(ff.motifs, hh.motifs))
sq.10.mixed <- lapply(FUN = adj.to.tpo, X = sq.10.mixed)
for(i in 1:length(sq.10.mixed)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/SequentialNetworks/sq.10.mixed.", i)
  saveRDS(sq.10.mixed[[i]], file = nme)
}

######### ######### #########   sequential 20 node  ######### #########
apply.me <- rep(10,10)
sq.20.hh <- lapply(FUN = seq.net.gen, X = apply.me, motif.choice = c(21,39,30))
plot.adj(sq.20.hh[[1]])
sq.20.ff <- lapply(FUN = seq.net.gen, X = apply.me, motif.choice = c(1,10))
plot.adj(sq.20.ff[[1]])




sq.10.ff[[1]]





apply.me <- rep(10,10)
sq.20.hh <- lapply(FUN = seq.net.gen, X = apply.me, motif.choice = hh.motifs)
sq.20.hh <- lapply(FUN = adj.to.tpo, X = sq.20.hh)
for(i in 1:length(sq.20.hh)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/SequentialNetworks/sq.20.hh.", i)
  saveRDS(sq.20.hh[[i]], file = nme)
}

sq.20.ff <- lapply(FUN = seq.net.gen, X = apply.me, motif.choice = ff.motifs)
sq.20.ff <- lapply(FUN = adj.to.tpo, X = sq.20.ff)
for(i in 1:length(sq.20.ff)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/SequentialNetworks/sq.20.ff.", i)
  saveRDS(sq.20.ff[[i]], file = nme)
}

sq.20.mixed <- lapply(FUN = seq.net.gen, X = apply.me, motif.choice = c(ff.motifs, hh.motifs))
sq.20.mixed <- lapply(FUN = adj.to.tpo, X = sq.20.mixed)
for(i in 1:length(sq.20.mixed)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/SequentialNetworks/sq.20.mixed.", i)
  saveRDS(sq.20.mixed[[i]], file = nme)
}



######### ######### #########   sequential 30 node  ######### #########
apply.me <- rep(15,10)
sq.30.hh <- lapply(FUN = seq.net.gen, X = apply.me, motif.choice = hh.motifs)
sq.30.hh <- lapply(FUN = adj.to.tpo, X = sq.30.hh)
for(i in 1:length(sq.30.hh)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/SequentialNetworks/sq.30.hh.", i)
  saveRDS(sq.30.hh[[i]], file = nme)
}

sq.30.ff <- lapply(FUN = seq.net.gen, X = apply.me, motif.choice = ff.motifs)
sq.30.ff <- lapply(FUN = adj.to.tpo, X = sq.30.ff)
for(i in 1:length(sq.30.ff)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/SequentialNetworks/sq.30.ff.", i)
  saveRDS(sq.30.ff[[i]], file = nme)
}

sq.30.mixed <- lapply(FUN = seq.net.gen, X = apply.me, motif.choice = c(ff.motifs, hh.motifs))
sq.30.mixed <- lapply(FUN = adj.to.tpo, X = sq.30.mixed)
for(i in 1:length(sq.30.mixed)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/SequentialNetworks/sq.30.mixed.", i)
  saveRDS(sq.30.mixed[[i]], file = nme)
}

######### ######### #########   sequential 40 node  ######### #########
apply.me <- rep(20,10)
sq.40.hh <- lapply(FUN = seq.net.gen, X = apply.me, motif.choice = hh.motifs)
sq.40.hh <- lapply(FUN = adj.to.tpo, X = sq.40.hh)
for(i in 1:length(sq.40.hh)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/SequentialNetworks/sq.40.hh.", i)
  saveRDS(sq.40.hh[[i]], file = nme)
}

sq.40.ff <- lapply(FUN = seq.net.gen, X = apply.me, motif.choice = ff.motifs)
sq.40.ff <- lapply(FUN = adj.to.tpo, X = sq.40.ff)
for(i in 1:length(sq.40.ff)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/SequentialNetworks/sq.40.ff.", i)
  saveRDS(sq.40.ff[[i]], file = nme)
}

sq.40.mixed <- lapply(FUN = seq.net.gen, X = apply.me, motif.choice = c(ff.motifs, hh.motifs))
sq.40.mixed <- lapply(FUN = adj.to.tpo, X = sq.40.mixed)
for(i in 1:length(sq.40.mixed)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/SequentialNetworks/sq.40.mixed.", i)
  saveRDS(sq.40.mixed[[i]], file = nme)
}

######### ######### #########   sequential 50 node  ######### #########
apply.me <- rep(25,10)
sq.50.hh <- lapply(FUN = seq.net.gen, X = apply.me, motif.choice = hh.motifs)
sq.50.hh <- lapply(FUN = adj.to.tpo, X = sq.50.hh)
for(i in 1:length(sq.50.hh)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/SequentialNetworks/sq.50.hh.", i)
  saveRDS(sq.50.hh[[i]], file = nme)
}

sq.50.ff <- lapply(FUN = seq.net.gen, X = apply.me, motif.choice = ff.motifs)
sq.50.ff <- lapply(FUN = adj.to.tpo, X = sq.50.ff)
for(i in 1:length(sq.50.ff)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/SequentialNetworks/sq.50.ff.", i)
  saveRDS(sq.50.ff[[i]], file = nme)
}

sq.50.mixed <- lapply(FUN = seq.net.gen, X = apply.me, motif.choice = c(ff.motifs, hh.motifs))
sq.50.mixed <- lapply(FUN = adj.to.tpo, X = sq.50.mixed)
for(i in 1:length(sq.50.mixed)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/SequentialNetworks/sq.50.mixed.", i)
  saveRDS(sq.50.mixed[[i]], file = nme)
}











######### ######### #########   random     10 node  ######### #########
apply.me <- rep(10,10)
rn.10.hh <- lapply(FUN = random.net.gen, X = apply.me, motif.choice = hh.motifs, edge.param = 2)
rn.10.hh <- lapply(FUN = adj.to.tpo, X = rn.10.hh)
for(i in 1:length(rn.10.hh)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/RandomNetworks/rn.10.hh.", i)
  saveRDS(rn.10.hh[[i]], file = nme)
}

tmp <- readRDS("/Users/c-clausb/Desktop/Lijia/RandomNetworks/rn.20.hh.2")
plot.net(tmp)

rn.10.ff <- lapply(FUN = random.net.gen, X = apply.me, motif.choice = ff.motifs, edge.param = 2)
rn.10.ff <- lapply(FUN = adj.to.tpo, X = rn.10.ff)
for(i in 1:length(rn.10.ff)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/RandomNetworks/rn.10.ff.", i)
  saveRDS(rn.10.ff[[i]], file = nme)
}


rn.10.mixed <- lapply(FUN = random.net.gen, X = apply.me, motif.choice = c(hh.motifs, ff.motifs), edge.param = 2)
rn.10.mixed <- lapply(FUN = adj.to.tpo, X = rn.10.mixed)
for(i in 1:length(rn.10.mixed)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/RandomNetworks/rn.10.mixed.", i)
  saveRDS(rn.10.mixed[[i]], file = nme)
}


######### ######### #########   random     20 node  ######### #########
apply.me <- rep(20,10)
rn.20.hh <- lapply(FUN = random.net.gen, X = apply.me, motif.choice = hh.motifs, edge.param = 2)
rn.20.hh <- lapply(FUN = adj.to.tpo, X = rn.20.hh)
for(i in 1:length(rn.20.hh)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/RandomNetworks/rn.20.hh.", i)
  saveRDS(rn.20.hh[[i]], file = nme)
}

rn.20.ff <- lapply(FUN = random.net.gen, X = apply.me, motif.choice = ff.motifs, edge.param = 2)
rn.20.ff <- lapply(FUN = adj.to.tpo, X = rn.20.ff)
for(i in 1:length(rn.20.ff)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/RandomNetworks/rn.20.ff.", i)
  saveRDS(rn.20.ff[[i]], file = nme)
}


rn.20.mixed <- lapply(FUN = random.net.gen, X = apply.me, motif.choice = c(hh.motifs, ff.motifs), edge.param = 2)
rn.20.mixed <- lapply(FUN = adj.to.tpo, X = rn.20.mixed)
for(i in 1:length(rn.20.mixed)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/RandomNetworks/rn.20.mixed.", i)
  saveRDS(rn.20.mixed[[i]], file = nme)
}
######### ######### #########   random     30 node  ######### #########
apply.me <- rep(30,10)
rn.30.hh <- lapply(FUN = random.net.gen, X = apply.me, motif.choice = hh.motifs, edge.param = 2)
rn.30.hh <- lapply(FUN = adj.to.tpo, X = rn.30.hh)
for(i in 1:length(rn.30.hh)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/RandomNetworks/rn.30.hh.", i)
  saveRDS(rn.30.hh[[i]], file = nme)
}

rn.30.ff <- lapply(FUN = random.net.gen, X = apply.me, motif.choice = ff.motifs, edge.param = 2)
rn.30.ff <- lapply(FUN = adj.to.tpo, X = rn.30.ff)
for(i in 1:length(rn.30.ff)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/RandomNetworks/rn.30.ff.", i)
  saveRDS(rn.30.ff[[i]], file = nme)
}


rn.30.mixed <- lapply(FUN = random.net.gen, X = apply.me, motif.choice = c(hh.motifs, ff.motifs), edge.param = 2)
rn.30.mixed <- lapply(FUN = adj.to.tpo, X = rn.30.mixed)
for(i in 1:length(rn.30.mixed)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/RandomNetworks/rn.30.mixed.", i)
  saveRDS(rn.30.mixed[[i]], file = nme)
}
######### ######### #########   random     40 node  ######### #########
apply.me <- rep(40,10)
rn.40.hh <- lapply(FUN = random.net.gen, X = apply.me, motif.choice = hh.motifs, edge.param = 2)
rn.40.hh <- lapply(FUN = adj.to.tpo, X = rn.40.hh)
for(i in 1:length(rn.40.hh)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/RandomNetworks/rn.40.hh.", i)
  saveRDS(rn.40.hh[[i]], file = nme)
}

rn.40.ff <- lapply(FUN = random.net.gen, X = apply.me, motif.choice = ff.motifs, edge.param = 2)
rn.40.ff <- lapply(FUN = adj.to.tpo, X = rn.40.ff)
for(i in 1:length(rn.40.ff)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/RandomNetworks/rn.40.ff.", i)
  saveRDS(rn.40.ff[[i]], file = nme)
}


rn.40.mixed <- lapply(FUN = random.net.gen, X = apply.me, motif.choice = c(hh.motifs, ff.motifs), edge.param = 2)
rn.40.mixed <- lapply(FUN = adj.to.tpo, X = rn.40.mixed)
for(i in 1:length(rn.40.mixed)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/RandomNetworks/rn.40.mixed.", i)
  saveRDS(rn.40.mixed[[i]], file = nme)
}
######### ######### #########   random     50 node  ######### #########
apply.me <- rep(50,10)
rn.50.hh <- lapply(FUN = random.net.gen, X = apply.me, motif.choice = hh.motifs, edge.param = 2)
rn.50.hh <- lapply(FUN = adj.to.tpo, X = rn.50.hh)
for(i in 1:length(rn.50.hh)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/RandomNetworks/rn.50.hh.", i)
  saveRDS(rn.50.hh[[i]], file = nme)
}


plot.net(rn.50.mixed[[8]])


rn.50.ff <- lapply(FUN = random.net.gen, X = apply.me, motif.choice = ff.motifs, edge.param = 2)
rn.50.ff <- lapply(FUN = adj.to.tpo, X = rn.50.ff)
for(i in 1:length(rn.50.ff)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/RandomNetworks/rn.50.ff.", i)
  saveRDS(rn.50.ff[[i]], file = nme)
}


rn.50.mixed <- lapply(FUN = random.net.gen, X = apply.me, motif.choice = c(hh.motifs, ff.motifs), edge.param = 2)
rn.50.mixed <- lapply(FUN = adj.to.tpo, X = rn.50.mixed)
for(i in 1:length(rn.50.mixed)){
  nme <- paste0("/Users/c-clausb/Desktop/Lijia/RandomNetworks/rn.50.mixed.", i)
  saveRDS(rn.50.mixed[[i]], file = nme)
}










