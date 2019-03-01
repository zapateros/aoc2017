setwd("C:/Users/paul/Documents/R-projects/AoC 2017/day_15")
input <- readLines("input_day_15.txt")
inp   <- as.numeric(unlist(regmatches(input, gregexpr("[[:digit:]]+", input))))

af  <- 16807
bf  <- 48271
mdl <- sum(2 ^ c(0:15)) + 1

# Part One
xa   <- inp[1]
xb    <- inp[2]
count <- 0
for(i in 1:40000000){
  xa <- (xa * af) %% 2147483647
  xb <- (xb * bf) %% 2147483647
  mda <- xa %% mdl
  mdb <- xb %% mdl
  if(mda == mdb){
    count <- count + 1
  }
}
result <- count
cat(result)


# Part Two
xa <- inp[1]
xb <- inp[2]
count <- 0
for(i in 1:5000000){
  while(TRUE){
    xa <- (xa * af) %% 2147483647
    if((xa %% 4) == 0){
      break
    }
  }
  while(TRUE){
    xb <- (xb * bf) %% 2147483647
    if((xb %% 8) == 0){
      break
    }
  }
  mda <- xa %% mdl
  mdb <- xb %% mdl
  if(mda == mdb){
    count <- count + 1
  }
}
result <- count
cat(result)
