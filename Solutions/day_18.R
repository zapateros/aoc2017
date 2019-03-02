input <- readLines("input_day_18.txt")

regs <- NULL
for(i in 1:length(input)){
  rel <- unlist(strsplit(input[i], " "))
  if(rel[2] %in% letters){
    regs <- c(regs,rel[2])
  }  
}
rn   <- unique(regs)
regs <- rep(0,length(rn)) 

# Part One
sound <- 0
i     <- 1
while(TRUE){
  rel <- unlist(strsplit(input[i], " "))
  if(rel[1] == "set"){
    if(rel[3] %in% rn){
      regs[which(rn == rel[2])] <- regs[which(rn == rel[3])]
    }else{
      regs[which(rn == rel[2])] <- as.numeric(rel[3])
    }
   i <- i + 1 
  }else if(rel[1] == "mul"){
    regs[which(rn == rel[2])] <- regs[which(rn == rel[2])] * as.numeric(rel[3])
    i <- i + 1
  }else if(rel[1] == "jgz"){
    if(regs[which(rn == rel[2])] > 0){
      if(rel[3] %in% rn){
        i <- i + regs[which(rn == rel[3])]
      }else{
        i <- i + as.numeric(rel[3])
      }
    }else{
      i <- i + 1
    }
  }else if(rel[1] == "add"){
    if(rel[3] %in% rn){
      regs[which(rn == rel[2])] <- regs[which(rn == rel[2])] + regs[which(rn == rel[3])]
    }else{
      regs[which(rn == rel[2])] <- regs[which(rn == rel[2])] + as.numeric(rel[3])
    }
    i <- i + 1
  }else if(rel[1] == "mod"){
    if(rel[3] %in% rn){
      regs[which(rn == rel[2])] <- regs[which(rn == rel[2])] %% regs[which(rn == rel[3])]
    }else{
      regs[which(rn == rel[2])] <- regs[which(rn == rel[2])] %% as.numeric(rel[3])
    }
    i <- i + 1
  }else if(rel[1] == "rcv"){
    if(regs[which(rn == rel[2])] != 0){
      break
    }
    i <- i + 1
  }else if(rel[1] == "snd"){
    sound <- regs[which(rn == rel[2])]
    i <- i + 1
  }
}
result <- sound
cat(result)


# Part Two
regs  <- rep(0, length(rn)) 
i     <- 1
j     <- 1
regs1 <- regs
regs1[which(rn == "p")] <- 1
c0    <- NULL
c1    <- NULL
count <- 0
while(TRUE){
  i_old <- i
  j_old <- j

  ### ID 0 
  rel <- unlist(strsplit(input[i], " "))
  if(rel[1] == "set"){
    if(rel[3] %in% rn){
      regs[which(rn == rel[2])] <- regs[which(rn == rel[3])]
    }else{
      regs[which(rn == rel[2])] <- as.numeric(rel[3])
    }
    i <- i + 1 
  }else if(rel[1] == "mul"){
    regs[which(rn == rel[2])] <- regs[which(rn == rel[2])] * as.numeric(rel[3])
    i <- i + 1
  }else if(rel[1] == "jgz"){
    if(rel[2] %in% rn){
      if(regs[which(rn == rel[2])] > 0){
        if(rel[3] %in% rn){
          i <- i + regs[which(rn == rel[3])]
        }else{
          i <- i + as.numeric(rel[3])
        }
      }else{
        i <- i + 1
      }
    }else{
      if(as.numeric(rel[2]) > 0){
        if(rel[3] %in% rn){
          i <- i + regs[which(rn == rel[3])]
        }else{
          i <- i + as.numeric(rel[3])
        }
      }else{
        i <- i + 1
      }
    }
  }else if(rel[1] == "add"){
    if(rel[3] %in% rn){
      regs[which(rn == rel[2])] <- regs[which(rn == rel[2])] + regs[which(rn == rel[3])]
    }else{
      regs[which(rn == rel[2])] <- regs[which(rn == rel[2])] + as.numeric(rel[3])
    }
    i <- i + 1
  }else if(rel[1] == "mod"){
    if(rel[3] %in% rn){
      regs[which(rn == rel[2])] <- regs[which(rn == rel[2])] %% regs[which(rn == rel[3])]
    }else{
      regs[which(rn == rel[2])] <- regs[which(rn == rel[2])] %% as.numeric(rel[3])
    }
    i <- i + 1
  }else if(rel[1] == "rcv"){
    if(length(c0) > 0){
      regs[which(rn == rel[2])] <- c0[1]
      c0 <- c0[-1]
      i  <- i + 1
    }
  }else if(rel[1] == "snd"){
    c1 <- c(c1, regs[which(rn == rel[2])])
    i  <- i + 1
  }

  
  ### ID 1
  rel1 <- unlist(strsplit(input[j]," "))
  if(rel1[1] == "set"){
    if(rel1[3] %in% rn){
      regs1[which(rn == rel1[2])] <- regs1[which(rn == rel1[3])]
    }else{
      regs1[which(rn == rel1[2])] <- as.numeric(rel1[3])
    }
    j <- j + 1 
  }else if(rel1[1] == "mul"){
    regs1[which(rn == rel1[2])] <- regs1[which(rn == rel1[2])] * as.numeric(rel1[3])
    j <- j + 1
  }else if(rel1[1] == "jgz"){
    if(rel1[2] %in% rn){
      if(regs1[which(rn == rel1[2])] > 0){
        if(rel1[3] %in% rn){
          j <- j + regs1[which(rn == rel1[3])]
        }else{
          j <- j + as.numeric(rel1[3])
        }
      }else{
        j <- j + 1
      }
    }else{
      if(as.numeric(rel1[2]) > 0){
        if(rel1[3] %in% rn){
          j <- j + regs1[which(rn == rel1[3])]
        }else{
          j <- j + as.numeric(rel1[3])
        }
      }else{
        j <- j + 1
      }
    }
  }else if(rel1[1] == "add"){
    if(rel1[3] %in% rn){
      regs1[which(rn == rel1[2])] <- regs1[which(rn == rel1[2])] + regs1[which(rn == rel1[3])]
    }else{
      regs1[which(rn == rel1[2])] <- regs1[which(rn == rel1[2])] + as.numeric(rel1[3])
    }
    j <- j + 1
  }else if(rel1[1] == "mod"){
    if(rel1[3] %in% rn){
      regs1[which(rn == rel1[2])] <- regs1[which(rn == rel1[2])] %% regs1[which(rn == rel1[3])]
    }else{
      regs1[which(rn == rel1[2])] <- regs1[which(rn == rel1[2])] %% as.numeric(rel1[3])
    }
    j <- j + 1
  }else if(rel1[1] == "rcv"){
    if(length(c1) > 0){
      regs1[which(rn == rel1[2])] <- c1[1]
      c1 <- c1[-1]
      j  <- j + 1
    }
  }else if(rel1[1] == "snd"){
    c0 <- c(c0, regs1[which(rn == rel1[2])])
    j  <- j + 1
    count <- count + 1
  }
  if((j == j_old) & (i == i_old)){
    break
  }
}
result <- count
cat(count)
