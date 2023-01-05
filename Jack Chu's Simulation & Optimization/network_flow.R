#################
### Flow Data ###
### Two Ways  ###
#################

# To this point, we have mostly lived in the world of
# creating objects that reflect our problem. While we 
# did do some manipulation with the advertising data
# it wasn't complex -- you will look back at that
# code within a month and find that it is suddenly
# much clearer to you.

# Let's consider something graphically:

install.packages("DiagrammeR")

library(DiagrammeR)
library(ROI)
library(ROI.plugin.glpk)

grViz("
digraph {
  graph [overlap = true, fontsize = 10, rankdir = LR]
  
  node [shape = box, style = filled, color = black, fillcolor = aliceblue]
  A [label = '1']
  B [label = '2']
  C [label = '3']
  D [label = '4']
  E [label = '5']
  F [label = '6']
  G [label = '7']

  A->B [label = ' 3']
  A->C [label = ' 1']
  A->D [label = ' 7']
  B->D [label = ' 2']
  B->G [label = ' 12']
  C->E [label = ' 10']
  D->E [label = ' 1']
  D->F [label = ' 1']
  E->G [label = ' 4']
  F->G [label = ' 4']
}
")

# We can create the vectors/matrices as per normal:

cvec <- c(var1_2 = 3, 
          var1_3 = 1, 
          var1_4 = 7, 
          var2_4 = 2, 
          var2_7 = 12, 
          var3_5 = 10, 
          var4_5 = 1, 
          var4_6 = 1, 
          var5_7 = 4, 
          var6_7 = 4)

amat <- rbind(nodeS = c(-1, -1, -1, 0, 0, 0, 0, 0, 0, 0),
              node2 = c(1, 0, 0, -1, -1, 0, 0, 0, 0, 0), 
              node3 = c(0, 1, 0, 0, 0, -1, 0, 0, 0, 0), 
              node4 = c(0, 0, 1, 1, 0, 0, -1, -1, 0, 0), 
              node5 = c(0, 0, 0, 0, 0, 1, 1, 0, -1, 0), 
              node6 = c(0, 0, 0, 0, 0, 0, 0, 1, 0, -1), 
              nodeT = c(0, 0, 0, 0, 1, 0, 0, 0, 1, 1))

colnames(amat) <- names(cvec)

amat

bvec <- c(-1, rep(0, 5), 1) 

constraints <- rep("==", 7)

test <- lpSolve::lp("min", cvec, amat, constraints, bvec)

test

names(test$solution) <- names(cvec)

test$solution

# Let's make a new assumption: we actually
# have something that resembles data.

flow_data <- data.frame(from = c(1,1,1,2,2,3,4,4,5,6), 
                        to = c(2,3,4,4,7,5,5,6,7,7), 
                        ID = 1:10, 
                        cost = c(3,1,7,2,12,10,1,1,4,4))

# We can spin up some code to make life easier:

all_nodes <- c(flow_data$from, flow_data$to) |> 
  unique()


node_flow_complete <- matrix(0, ncol = nrow(flow_data), 
                             nrow = length(all_nodes), 
                             dimnames = list(all_nodes, 
                                             flow_data$ID))

for(i in all_nodes) {
  input_node <- flow_data$ID[flow_data$to == i]
  
  output_node <- flow_data$ID[flow_data$from == i]
  
  node_flow_complete[rownames(node_flow_complete)  == i, 
                     colnames(node_flow_complete) %in% input_node] <- 1
  
  node_flow_complete[rownames(node_flow_complete) == i, 
                     colnames(node_flow_complete) %in% output_node] <- -1
  
}

node_flow_complete

dimnames(node_flow_complete) <- list(paste0("node_", all_nodes), 
               paste0("from_", flow_data$from, 
                      "_to_", flow_data$to))

node_flow_complete

model_create <- OP(objective = flow_data$cost, # Notice anything cool?
                   constraints = L_constraint(node_flow_complete, 
                                              rep("==", 7), 
                                              c(-1, rep(0, 5), 1)), 
                   maximum = FALSE)

model_solve <- ROI_solve(model_create)

solution(model_solve)
