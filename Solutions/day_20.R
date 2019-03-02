input <- readLines("input_day_20.txt")
inp   <- as.numeric(unlist(regmatches(input, gregexpr("[-[:digit:]]+", input))))

# Part One
mt<- matrix(inp, ncol = 9, byrow = TRUE)
it <- 1
while(it < 10^3){
  it     <- it + 1
  mt[,4] <- mt[,4] + mt[,7]
  mt[,5] <- mt[,5] + mt[,8]
  mt[,6] <- mt[,6] + mt[,9]
  mt[,1] <- mt[,1] + mt[,4] 
  mt[,2] <- mt[,2] + mt[,5]
  mt[,3] <- mt[,3] + mt[,6] 
  md     <- abs(mt[,1]) + abs(mt[,2]) + abs(mt[,3]) 
}
result <- which.min(md) - 1
cat(result)

# Part Two
mt  <- matrix(inp, ncol = 9, byrow = TRUE)
it <- 1
while(it < 10^3){
  it  <- it + 1
  pos <- paste0(mt[,1], mt[,2], mt[,3])
  dup <- duplicated(pos) | duplicated(pos, fromLast=TRUE)
  if(any(dup)){
    mt <- mt[!dup,]
  }
  mt[,4] <- mt[,4] + mt[,7]
  mt[,5] <- mt[,5] + mt[,8]
  mt[,6] <- mt[,6] + mt[,9]
  mt[,1] <- mt[,1] + mt[,4] 
  mt[,2] <- mt[,2] + mt[,5]
  mt[,3] <- mt[,3] + mt[,6] 
}
result <- nrow(mt)
cat(result)
