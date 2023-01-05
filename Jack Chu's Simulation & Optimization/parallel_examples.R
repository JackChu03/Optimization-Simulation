#################################
### Different Ways To Iterate ###
###   With Different Speeds   ###
#################################

# You will probably need most of these packages.

install.packages(c("doParallel", "foreach", "furrr", 
                   "future", "gganimate"))

library(doParallel)
library(foreach)
library(furrr)
library(future)
library(gganimate)
library(ggplot2)
library(purrr)
library(rvest)

alive_plants <- 1000

number_days <- 65

for(i in 1:number_days) {
  
  day <- i
  
  live_dead <- rbinom(alive_plants, 1, .999)
  
  daily_dead <- sum(live_dead == 0)
  
  alive_plants <- alive_plants - daily_dead
}

alive_plants

# Just to return to where we were last week, but with an additional
# source of variation -- mechanical failures. There is a small chance
# that a pump could fail (the object called pump_failure). If a pump
# fails, then we increase the probability of a plant dying. 

all_results <- replicate(1000, expr = {
  alive_plants <- 1000
  
  number_days <- 65
  
  for(i in 1:number_days) {
    
    day <- i
    
    pump_failure <- rbinom(1, 1, .01)
    
    # Remember, we are taking a draw from the binomial distribution, 
    # so a 1 here would indicate that a pump failed.
    if(pump_failure == 1) {
      # If we experience a pump failure, the probability of a plant
      # living drops down to .9
      live_dead <- rbinom(alive_plants, 1, .90)
      # If we don't experience a failure, then we have the normal
      # probability of staying alive:
    } else live_dead <- rbinom(alive_plants, 1, .999)
    
    daily_dead <- sum(live_dead == 0)
    
    alive_plants <- alive_plants - daily_dead
  }
  
  alive_plants
})

hist(all_results)

# Now, we are going to switch up the problem, but start testing for speed.
# Our problem now is how much money we can make everyday (according to a
# normal distribution), but that is subject to breakdowns occuring. 
# Since breakdowns happen according to a binomial distribution, but the days
# that something is broken down will be a draw from the Poisson distribution.

### Testing replicate ###

# This proc.time is going to help us keep track of when computation started:
t1 <- proc.time()
# We will just rock with our standard replicate here:
dollar_breaks <- replicate(10000, expr = {
  
  # Now, we just need to specify some starting values:
  day <- 0
  brokedown <- 0
  total_breakdown <- 0
  counter <- 0
  dollars_made <- 0
  
  # And we are going to do this for a whole year
  for(i in 1:365) {
    # The brokedown variable started at 0, so taking -1 away will make it go
    # negative. A negative or 0 means we are not broken down:
    brokedown <- brokedown - 1
    
    if(rbinom(1, 1, .1) == 1 & brokedown <= 0) { # Break downs happen 10%
      # You could use the sample function or the rpois function
      # brokedown <- sample(1:3, 1, prob = c(.1, .8, .1)) # Usually for 2 days
      # No matter what, if a breakdown happens, how many days will it stay
      # broken down. 
      brokedown <- rpois(1, 2)
      # I just want to add these together to know how many days in total
      # I have been broken down:
      total_breakdown <- total_breakdown + brokedown
    }
    
    # I am only going to make money on the days where I am not broken down,
    # so I will only sample and add dollars if brokedown is less than 1:
    if(brokedown < 1) {
      dollar_sample <- rnorm(1, 180, 25)
      dollars_made <- dollars_made + dollar_sample
    }  
    # This just increments the day count:
    day <- day + 1
  }
  
  # Now I am going to put my results into a data.frame for returning results:
  data.frame(dollars = dollars_made, 
             days = total_breakdown)
}, simplify = FALSE)
# Always worth remembering that replicate likes to return a list, 
# so we will need to row bind the individual data frames into one data frame.
dollar_breaks <- do.call(rbind, dollar_breaks)
proc.time() - t1

### Testing purrr::map_dfr ###

t1 <- proc.time()
# Now we can try a map_dfr from purrr.
dollar_breaks <- map_dfr(1:10000, ~{
  
  day <- 0
  brokedown <- 0
  total_breakdown <- 0
  counter <- 0
  dollars_made <- 0
  
  for(i in 1:365) {
    brokedown <- brokedown - 1
    
    if(rbinom(1, 1, .1) == 1 & brokedown <= 0) { # Break downs happen 10%
      # brokedown <- sample(1:3, 1, prob = c(.1, .8, .1)) # Usually for 2 days
      brokedown <- rpois(1, 2)
      total_breakdown <- total_breakdown + brokedown
    }
    
    if(brokedown < 1) {
      dollar_sample <- rnorm(1, 180, 25)
      dollars_made <- dollars_made + dollar_sample
    }  
    day <- day + 1
  }
  
  data.frame(dollars = dollars_made, 
             days = total_breakdown, 
             run = .x)
})
proc.time() - t1


### Testing nested for loops ###

# And now with a nested for loop.
# Let's pre-allocate a blank data frame:

for_loop_data <- data.frame(dollars = numeric(), 
                            days = numeric(), 
                            run = numeric())[1:10000, ]

# Now, we just need to make sure that we don't mess things up with 
# the nesting!
t1 <- proc.time()
for(runs in 1:10000) {
  day <- 0
  brokedown <- 0
  total_breakdown <- 0
  counter <- 0
  dollars_made <- 0
  
  for(i in 1:365) {
    brokedown <- brokedown - 1
    
    if(rbinom(1, 1, .1) == 1 & brokedown <= 0) { # Break downs happen 10%
      # brokedown <- sample(1:3, 1, prob = c(.1, .8, .1)) # Usually for 2 days
      brokedown <- rpois(1, 2)
      total_breakdown <- total_breakdown + brokedown
    }
    
    if(brokedown < 1) {
      dollar_sample <- rnorm(1, 180, 25)
      dollars_made <- dollars_made + dollar_sample
    }  
    day <- day + 1
  }
  
  for_loop_data[runs, ] <- data.frame(dollars = dollars_made, 
             days = total_breakdown, 
             run = runs)
}
proc.time() - t1

### Testing parallelized map_dfr ###

# This is different. We are going to use the furrr and future package
# to parallelize our computation. 
# We are going to plan to use a multisession process and we will
# utilize the number of available cores - 1. 
plan(multisession, workers = parallel::detectCores() - 1)

t1 <- proc.time()
# Just like we did with map, but with future appended to the front.
dollar_breaks <- future_map_dfr(1:10000, ~{
  
  day <- 0
  brokedown <- 0
  total_breakdown <- 0
  counter <- 0
  dollars_made <- 0
  
  for(i in 1:365) {
    brokedown <- brokedown - 1
    
    if(rbinom(1, 1, .1) == 1 & brokedown <= 0) { # Break downs happen 10%
      # brokedown <- sample(1:3, 1, prob = c(.1, .8, .1)) # Usually for 2 days
      brokedown <- rpois(1, 2)
      total_breakdown <- total_breakdown + brokedown
    }
    
    if(brokedown < 1) {
      dollar_sample <- rnorm(1, 180, 25)
      dollars_made <- dollars_made + dollar_sample
    }  
    day <- day + 1
  }
  
  data.frame(dollars = dollars_made, 
             days = total_breakdown, 
             run = .x)
}, .options = furrr_options(seed = TRUE))
proc.time() - t1

### Testing parallelized loops with foreach ###

# And another way to handle the same task, but with the foreach package:
# We have to explicitly make our clusters:
cl <- makeCluster(parallel::detectCores() - 1)

# And then register them:
registerDoParallel(cl)

t1 <- proc.time()
dollar_breaks <- foreach(i = 1:10000, .combine = "rbind") %dopar% {
  day <- 0
  brokedown <- 0
  total_breakdown <- 0
  counter <- 0
  dollars_made <- 0
  
  for(i in 1:365) {
    brokedown <- brokedown - 1
    
    if(rbinom(1, 1, .1) == 1) { # Break downs happen 10%
      # brokedown <- sample(1:3, 1, prob = c(.1, .8, .1)) # Usually for 2 days
      brokedown <- rpois(1, 2)
      total_breakdown <- total_breakdown + brokedown
    }
    
    if(brokedown < 1) {
      dollar_sample <- rnorm(1, 180, 25)
      dollars_made <- dollars_made + dollar_sample
    }  
    day <- day + 1
  }
  
  data.frame(dollars = dollars_made, 
             days = total_breakdown, 
             run = i)
}
proc.time() - t1

stopCluster(cl)

ggplot(dollar_breaks, aes(days, dollars)) +
  geom_point() +
  theme_minimal()

ggplot(dollar_breaks, aes(dollars)) +
  geom_density() +
  theme_minimal()

ggplot(dollar_breaks, aes(days)) +
  geom_density() +
  theme_minimal()

