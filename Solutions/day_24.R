input <- readLines("input_day_24.txt")
input <- unlist(strsplit(input,"\n"))
inp   <- matrix(as.numeric(unlist(strsplit(input,"/"))), ncol = 2, byrow = TRUE)
inp   <- cbind(inp, 0)

bridge  <- 0
max_w   <- 0
max_l_w <- 0
max_l   <- 0
connect <- function(con){
  for(i in 1:nrow(inp)){
    if(inp[i, 3] != 1){
      left <- inp[i,1]
      right <- inp[i,2]
      if(left == con | right == con){
        inp[i,3] <<- 1
        if(left == con){
          bridge <<- c(bridge, right)
          connect(right)
        }else{
          bridge <<- c(bridge, left)
          connect(left)
        }
        inp[i,3] <<- 0
        weight <- 2 * sum(bridge) - bridge[length(bridge)]
        if(weight > max_w){
          max_w <<- weight
        }
        if(length(bridge) >= max_l){
          max_l <<- length(bridge)
          if(weight > max_l_w){
            max_l_w <<- weight
          }
        }
        bridge <<- bridge[-length(bridge)]
      }
    }
  }
}
connect(0)

# Part One
max_w

# Part Two
max_l_w
