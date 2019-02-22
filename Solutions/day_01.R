input <- readLines("input_day_01.txt")
inp <- as.numeric(unlist(strsplit(input,"")))
n <- length(inp)

# Part One
cnt<- 0
for(i in 1:n){
  i_s <- (i %% n) + 1
  if(inp[i] == inp[i_s]){
    cnt <- cnt + inp[i]
  }
}
cat(cnt)

# Part Two
cnt<- 0
for(i in 1:n){
  i_s <- ((i + n / 2 - 1) %% n) + 1
  if(inp[i] == inp[i_s]){
    cnt <- cnt + inp[i]
  }
}
cat(cnt)
