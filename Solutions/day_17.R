input <- readLines("input_day_17.txt")
inp   <- as.numeric(input)

# Part One
step <- inp
pos  <- 1
vec  <- 0
for(n in 1:2017){
  pos <- (pos + step - 1) %% n + 1
  vec <- append(vec, n, after = pos)
  pos <- pos %% (n+1) + 1
}
ind    <- which(vec == 2017)
result <- vec[ind + 1]
cat(result)


# Part Two
step <- inp
pos  <- 1
sec  <- NULL
for(n in 1:50000000){
  pos <- (pos + step - 1) %% n + 1
  pos <- pos %% (n + 1) + 1
  if(pos == 2){
    sec <- n
  }
}
result <- sec
cat(result)
