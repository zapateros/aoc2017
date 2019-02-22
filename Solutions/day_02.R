input <- read.table("input_day_02.txt")

# Part One
cnt <- 0
for(i in 1:nrow(input)){
  cs  <- max(input[i,])-min(input[i,])
  cnt <- cnt + cs
}

# Part Two
cnt <- 0
for(i in 1:nrow(input)){
  rel <- input[i,]
  for(j in 1:length(rel)){
    calc <- rel / as.numeric(rel[j])
    trs  <- (calc == round(calc) & calc != 1)
    if(any(trs)){
      cnt <- cnt + sum(calc[trs])
    }
  }
}
