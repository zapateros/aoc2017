input <- readLines("input_day_14.txt")

# Part One
matr <- NULL
for(jj in 0:127){
  sz  <- c(utf8ToInt(paste0(paste0(input,"-",jj,collapse=""),collapse=",")), 17, 31, 73, 47, 23)
  vc  <- c(0:255)
  hlp <- c(1:length(vc))
  st  <- 0
  pos <- 1
  for(k in 1:64){
    for(i in 1:length(sz)){
      if((sz[i] <= length(vc)) & (sz[i] > 0 )){
        if((pos + sz[i] - 1) > length(hlp)){
          nms <- c(hlp[pos:length(hlp)], hlp[1:(sz[i] - length(hlp) + pos -1)])
        }else{
          nms <- c(pos:(pos + sz[i] - 1))
        }
        lspos   <- nms[length(nms)]
        vc[nms] <- vc[rev(nms)]
      }
      pos <- (lspos + st) %% length(vc) + 1
      st  <- st + 1
    }
  }
  
  # BitwXor
  mt <- matrix(vc, ncol = 16, byrow = TRUE)
  dh <- NULL
  for(row in 1:16){
    tmp <- 0
    for(col in 1:16){
      tmp <- bitwXor(tmp, mt[row, col])
    }
    dh <- c(dh, tmp)
  }
  result <- paste0(as.raw(dh), collapse = "")
 
  # Create matrix
  hex <- paste0("0x",unlist(strsplit(result,"")))
  int <- strtoi(hex)
  mt_row <- NULL
  for(i in 1:length(int)){
    four   <- rev(as.numeric(rawToBits(as.raw(int[i])))[1:4])
    mt_row <- c(mt_row,four)
  }
  matr <- cbind(matr, mt_row)
}
result <- sum(matr == 1)
cat(result)


# Part Two
mt <-rbind(0,cbind(0,matr,0),0)
n  <- 2
for(i in 2:129){
  for(j in 2:129){
    if(mt[i, j] == 1){
      mt[i, j] <- n
      n <- n + 1
    }
  }
}

while(TRUE){
  if(!any(!(mt_old == mt))){
    break
  }
  mt_old <- mt
  for(i in 2:129){
    for(j in 2:129){
      k <- mt[i, j]
      if(k > 1){
        nms <- c(mt[i - 1, j], mt[i + 1, j], mt[i, j - 1], mt[i, j + 1])
        if(any(nms > 1)){
          mt[i, j] <- min(nms[which(nms > 1)])
        }
      }
    }
  }
}
result <- length(table(mt)) - 1
cat(result)
