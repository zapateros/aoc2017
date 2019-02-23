input <- readLines("input_day_08.txt")
inp   <- NULL
for(i in 1:length(input)){
  x <- unlist(strsplit(input[i]," "))
  inp <- rbind(inp,x)
}

reg_names <-unique(c(inp[, 1], inp[, 5]))
reg_vals  <- rep(0, length(reg_names))
mt        <- NULL
sign      <- NULL
for(i in 1:nrow(inp)){
  rel <- inp[i,]
  if(rel[2] == "dec"){
    sign <- -1
  }else{
    sign <- 1
  }
  if(rel[6]=="<"){
    if(reg_vals[which(rel[5]==reg_names)] < as.numeric(rel[7])){
      reg_vals[which(reg_names==rel[1])] <- reg_vals[which(reg_names==rel[1])] + sign * as.numeric(rel[3]) 
    }
  }else if(rel[6]=="!="){
      if(reg_vals[which(rel[5]==reg_names)] != as.numeric(rel[7])){
        reg_vals[which(reg_names==rel[1])] <- reg_vals[which(reg_names==rel[1])] + sign * as.numeric(rel[3]) 
      }
  }else if(rel[6]==">="){
    if(reg_vals[which(rel[5]==reg_names)] >= as.numeric(rel[7])){
      reg_vals[which(reg_names==rel[1])] <- reg_vals[which(reg_names==rel[1])] + sign * as.numeric(rel[3]) 
    }
  }else if(rel[6]=="=="){
    if(reg_vals[which(rel[5]==reg_names)] == as.numeric(rel[7])){
      reg_vals[which(reg_names==rel[1])] <- reg_vals[which(reg_names==rel[1])] + sign * as.numeric(rel[3]) 
    }
  }else if(rel[6]=="<="){
    if(reg_vals[which(rel[5]==reg_names)] <= as.numeric(rel[7])){
      reg_vals[which(reg_names==rel[1])] <- reg_vals[which(reg_names==rel[1])] + sign * as.numeric(rel[3]) 
    }
  }else if(rel[6]==">"){
    if(reg_vals[which(rel[5]==reg_names)] > as.numeric(rel[7])){
      reg_vals[which(reg_names==rel[1])] <- reg_vals[which(reg_names==rel[1])] + sign * as.numeric(rel[3]) 
    }
  }
   mt <- rbind(mt, reg_vals)
}

# Part One
res <- max(reg_vals)
cat(res)

# Part Two
res <- max(mt)
cat(res)
