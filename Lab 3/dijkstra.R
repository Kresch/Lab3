Wiki_graph <-
        data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
                   v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
                   w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

###

dijkstra<-function(graph,init_node){
        find_neighbours<-function(node){
                return(graph[,2][graph[,1]==node])
        }
        find_min<-function(currentnode){      #in this case, the neighbours
                neighbours<-find_neighbours(currentnode)
                mini<-min(graph[,3][graph[,2]==neighbours])
                return(mini)
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
                rownames(grph_mtrx)<-c(char_nodes)
                return(grph_mtrx)
        }
        
        update_matrix <- function(matrix, current_node){
                for(i in find_neighbours(current_node)){
                        if(min(matrix[,i])>=distance(current_node,i) + matrix[current_node,current_node]){
                                matrix[current_node,i] <- distance(current_node,i) + matrix[current_node, current_node]
                        }else{matrix[current_node,i] <- min(matrix[,i])}
                }
                return(matrix)
        }
        
        output<-generate_matrix(graph)
        rownames(output)[init_node]<-as.character(init_node)
        output[1,init_node]<-0
        current_node<-init_node
        
        
        find_neighbours(init_node)
        minimum<-find_min(current_node)
        distance(current_node,minimum)
        
        
}