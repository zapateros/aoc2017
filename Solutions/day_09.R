input <- readLines("input_day_09.txt")
inp   <- unlist(strsplit(input, ""))

# Part One
# Remove exclamation marks
i <- 1
while(TRUE){
  if(inp[i] == "!"){
    inp <- inp[-c(i, i + 1)]
  }else{
    i <- i + 1
  }
  if(i > length(inp)){
    break
  }
}

# Remove garbage and count for part two
garb <- 0
while(TRUE){
  ope <- which(inp == "<")
  if(length(ope) == 0){
    break
  }
  clo <- which(inp == ">")
  io  <- min(ope)
  ic  <- min(clo[clo > io])
  inp <- inp[-c(io:ic)]
  garb <- garb + ic - io - 1
}

# Remove unnecessary comma's
tmp <- paste0(inp, collapse = "")
tmp <- gsub(",}", "}", tmp)
tmp <- gsub("\\{,", "{", tmp)
inp <- unlist(strsplit(tmp, ""))

# Open and Close vectors
open <- close <- rep(0, length(inp)) 
open[which(inp == "{")]  <- 1
close[which(inp == "}")] <- 1

# Calculate score
comma <- 0
value <- 0
score <- 0
vals  <- NULL
for(i in 1:length(inp)){
  if(inp[i] == "{"){
    if(comma == 1){
      j  <- i
      op <- 0
      cl <- 0
      while(TRUE){
        j  <- j - 1
        op <- op + open[j]
        cl <- cl + close[j]
        if(op == cl & (op != 0)){
            value <- vals[j]
          break
        }
      }
      comma <- 0
    }else{
      value <- value + 1
    }
    score <-  score + value
  }else if(inp[i] == "}"){
    value <- 0
    comma <- 0
  }else if(inp[i] == ","){
    value <- 0
    comma <- 1
  }
  vals <- c(vals, value)
}
cat(score)

# Part Two
cat(garb)
