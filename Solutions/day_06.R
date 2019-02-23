input <- readLines("input_day_06.txt")
inp <- as.numeric(unlist(strsplit(input,"\t")))

# Part One
seen <- NULL
it   <- 0
while(TRUE){
  ps <- paste0(inp,collapse="")
  if(ps %in% seen){
    break
  }
  seen <- c(seen, ps )
  mx   <- which.max(inp)[1]
  max  <- max(inp)[1]
  inp[mx]<-0
  for(i in 1:max){
    j      <- (i + mx -1)  %% length(inp) + 1
    inp[j] <- inp[j]+1
  }
  it <- it + 1
}
cat(it)

# Part Two
n <- length(seen) + 1 - which(seen == ps)
