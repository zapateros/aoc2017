input <- readLines("input_day_04.txt")

# Part One
cnt <- 0
for(i in 1:length(input)){
  if(any(duplicated(unlist(strsplit(input[i]," "))))){
    cnt <- cnt + 1
  }
}
result <- length(input) - cnt
cat(result)


# Part Two
cnt <- 0
for(i in 1:length(input)){
  wrds <- unlist(strsplit(input[i]," "))
  for(j in 1:length(wrds)){
    wrds[j]<- paste0(sort(unlist(strsplit(wrds[j],""))),collapse="")
  }
  if(any(duplicated(unlist(strsplit(wrds," "))))){
    cnt <- cnt + 1
  }
}
result <- length(input) - cnt
cat(result)
