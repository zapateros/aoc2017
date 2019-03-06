input <- readLines("input_day_23.txt")
inp   <- matrix(unlist(strsplit(input, " ")), ncol = 3, byrow = TRUE)
rn    <- letters[1:8]

# Part One
regs  <- rep(0,length(rn)) 
count <- 0
i     <- 1
while(TRUE){
  rel <- inp[i,]
  if(rel[1] == "set"){
    if(rel[3] %in% rn){
      regs[which(rn == rel[2])] <- regs[which(rn == rel[3])]
    }else{
      regs[which(rn == rel[2])] <- as.numeric(rel[3])
    }
    i <- i + 1 
  }else if(rel[1] == "sub"){
    if(rel[3] %in% rn){
      regs[which(rn == rel[2])] <- regs[which(rn == rel[2])] - regs[which(rn == rel[3])]
    }else{
      regs[which(rn == rel[2])] <- regs[which(rn == rel[2])] - as.numeric(rel[3])
    }
    i <- i + 1
  }else if(rel[1] == "mul"){
    count <- count + 1
    if(rel[3] %in% rn){
      regs[which(rn == rel[2])] <- regs[which(rn == rel[2])] * regs[which(rn == rel[3])]
    }else{
      regs[which(rn == rel[2])] <- regs[which(rn == rel[2])] * as.numeric(rel[3])
    }
    i <- i + 1
  }else if(rel[1] == "jnz"){
    if(rel[2] %in% rn){
      if(regs[which(rn == rel[2])] != 0){
          i <- i + as.numeric(rel[3])
      }else{
        i <- i + 1
      }
    }else{
      if(as.numeric(rel[2]) != 0){
          i <- i + as.numeric(rel[3])
      }else{
        i <- i + 1
      }
    }
  }
  if(i > 32 | i < 1){
    break
  }
}
cat(count)

# Part Two (Reverse engineered)
is.prime <- function(n){
  n == 2L || all(n %% 2L:max(2, floor(sqrt(n))) != 0)
} 

b     <- 108400
count <- 0
for(i in 1:1001){
  m <- b + (i - 1) * 17
  if(is.prime(m) == FALSE){
    count <- count + 1
  }
}
cat(count) 
