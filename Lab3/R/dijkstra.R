wiki_graph <-
        data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
                   v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
                   w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

###

#' @param a dataframe and a starting node (scalar)
#' @description takes a graph and starting node. 
#' Runs dijkstra's algorithm, 
#' i.e. finds the shortest distance to each other node.
#' @return a vector of shortest distances to each other node.
#' @references https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
#'

dijkstra<-function(graph,init_node){
        if(!is.data.frame(graph) || (!is.vector(init_node)$$length(init_node==1))){
                stop("Wrong type of parameters")
        }
        find_neighbours<-function(node){
                return(graph[,2][graph[,1]==node])
        }
        distance <- function(node1, node2){
                distance <- Inf
                if(node1 == node2){distance <- 0}
                if(node2 %in% find_neighbours(node1)){
                        for(i in 1:length(graph[,1])){
                                if(graph[i,1]==node1 && graph[i,2]==node2){
                                        distance <- graph[i,3]
                                }
                        }
                }
                return(distance)
        }
        generate_matrix<-function(graph){
                nodes<-unique(graph[,1])
                char_nodes<-as.character(nodes)
                grph_mtrx<-matrix(c(rep(Inf,times=length(nodes)^2)),ncol=length(nodes))
                colnames(grph_mtrx)<-c(char_nodes)
                rownames(grph_mtrx)<-c(init_node,rep(NA,length(char_nodes)-1))
                return(grph_mtrx)
        }
        
        update_matrix <- function(matrix, current_node){
                for(i in find_neighbours(current_node)){
                        cn <- as.character(current_node)
                        if(min(matrix[,i]) != Inf){
                                if(matrix[cn,i]>=distance(current_node,i) + matrix[cn,cn]){
                                        matrix[cn,i] <- distance(current_node,i) + matrix[cn, cn]
                                }
                        }else{matrix[cn,i] <- distance(current_node,i) + matrix[cn, cn]}
                }
                return(matrix)
        }
        
        output<-generate_matrix(graph)
        output[1,init_node]<-0
        current_node<-init_node
        non_visited<-unique(graph[,1])
        
        iter<-1
        rownames(output)[iter] <- as.character(init_node)
        output <- update_matrix(output, init_node)
        output[iter+1,]<- output[iter,]
        while(!is.na(current_node)){
                iter<-iter+1
                non_visited<-non_visited[-which(non_visited==current_node)]
                current_node<-colnames(output)[output[current_node,]==min(output[current_node,non_visited])][1]
                if(!is.na(current_node)){rownames(output)[iter]<-as.character(current_node)}
                if(!is.na(current_node)){output<-update_matrix(output,current_node)}
                if((iter+1)<=length(output[,1])){output[iter+1,]<- output[iter,]}
        }
        result <- as.vector((output[length(output[,1]),]))
        return(result)
}

test <- data.frame(v1=c(1,1,2,2,2,3,3,3,3,4,4,4,4,5,5,5,6,6),
                   v2=c(2,3,1,3,4,1,2,4,5,2,3,5,6,3,4,6,4,5),
                   w=c(4,2,4,1,5,2,1,8,10,5,8,2,6,10,2,3,6,3))