
all_results <- replicate(1000, expr={
  alive_plants <- 1000
  number_days <- 65
  
  for(i in 1:number_days){
    
    pump_failure <- rbinom(1, 1, 0.01)
    
    if(pump_failure == 1){
      live_dead <- rbinom(alive_plants, 1, 0.9)
    } else live_dead <- rbinom(alive_plants, 1, 0.999)
    
    daily_dead <- sum(live_dead==0)
    
    alive_plants <- alive_plants - daily_dead
  }
  
  alive_plants
})

all_results

hist(all_results)


rbinom(alive_plants, 1, 0.999)

####################################################

# 30candy + 40gummies + 80bags
# 1candy + 1gummy - 10bag <= 500
# 4candy+3gummies-20bag <= 200
#1candy + 0gummies - 2bag <= 100
# 1candy + 1gummies + 0bag >= 1000 #Demand
library(ROI)
library(ROI.plugin.glpk)

test_values <- round(runif(1000, 900, 1100))

purrr::map_df(.x = test_values, ~{
  cvec <- c(30, 40, 80)
  
  b_vec <- c(500, 200, 100, .x)
  
  cons_dir <- c("<=", "<=", "<=", ">=")
  
  Amat <- rbind(c(1, 1, -10),
                c(4, 3, -20),
                c(1, 0, -2),
                c(1,1,0))
  
  total_constraints <- L_constraint(Amat, cons_dir, b_vec)
  
  my_solution <- OP(cvec, 
                    total_constraints, 
                    types = rep("I", length(cvec)), 
                    maximum = FALSE) |>
    ROI_solve() |>
    solution("primal")
  
  solution_df <- data.frame(t(my_solution))
  
  colnames(solution_df) <- c("candy", "gummies", "bags" )
  
  solution_df$demand <- .x
  
  solution_df
  
})

