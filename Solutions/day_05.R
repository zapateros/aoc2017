# Part One
input <- as.numeric(readLines("input_day_05.txt"))
i  <- 1
it <- 0
while(TRUE){
  it       <- it + 1
  input[i] <- input[i] + 1
  i        <- i + input[i] - 1
  if(i > length(input)){
    break
  }
}

# Part Two
input <- as.numeric(readLines("input_day_05.txt"))
i  <- 1
it <- 0
while(TRUE){
  it       <- it + 1
  if(input[i] > 2){
    input[i] <- input[i] - 1
    i        <- i + input[i] + 1
  }else{
    input[i] <- input[i] + 1
    i        <- i + input[i] - 1
  }
  if(i > length(input)){
    break
  }
}
