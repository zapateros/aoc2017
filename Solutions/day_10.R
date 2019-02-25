input <- readLines("input_day_10.txt")
sz    <- as.numeric(unlist(strsplit(input,",")))

# Part One
vc  <- c(0:255)
hlp <- c(1:length(vc))
st  <- 0
pos <- 1
for(i in 1:length(sz)){
  if((sz[i] <= length(vc)) & (sz[i] > 0 )){
    if((pos + sz[i] - 1) > length(hlp)){
      nms <- c(hlp[pos:length(hlp)], hlp[1:(sz[i] - length(hlp) + pos -1)])
    }else{
      nms <- c(pos:(pos + sz[i] - 1))
    }
    lspos <- nms[length(nms)]
    vc[nms] <- vc[rev(nms)]
  }
  pos <- (lspos + st) %% length(vc) + 1
  st <- st + 1
  vc
}
result <- vc[1] * vc[2]
cat(result)

# Part Two
sz  <- c(utf8ToInt(paste0(sz,collapse=",")), 17, 31, 73, 47, 23)
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
cat(result)
