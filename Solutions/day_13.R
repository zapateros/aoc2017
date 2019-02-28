input <- readLines("input_day_13.txt")
inp   <- matrix(as.numeric(unlist(strsplit(input,": "))), ncol = 2, byrow = TRUE)

x <- inp[, 1]
n <- inp[, 2]
m <- 2 * (n - 1)

# Part One
k <- x / m
j <- round(k) == k
result <- sum((x * n)[j])
cat(result)


# Part Two
i <- 0
while(TRUE){
  i <- i + 1
  k <- (x + i) / m
  j <- !any(round(k) == k)
  if(j == TRUE){
    break
  }
}
result <- i
cat(result)
