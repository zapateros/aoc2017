input <- readLines("input_day_19.txt")
mt<- NULL
for(i in 1:length(input)){
  row <- unlist(strsplit(input[i],""))
  mt <- rbind(mt, row)
}

# Start
y   <- 1 
x   <- which(mt[1,]=="|")
dir <- "d"

# Directions
dir_c <- c("r", "d", "l", "u")
dx    <- c(1, 0, -1, 0)
dy    <- c(0, 1, 0, -1)
dir_n <- which(dir_c == dir)

# Run 
count <- 0
word  <- NULL
while(TRUE){
  count <- count + 1
  if(mt[y, x] == "|"){
    if(dir == "l" | dir == "r"){
      x <- x + dx[dir_n]
    }else{
      y <- y + dy[dir_n]
    }
  }else if(mt[y, x] == "-"){
    if(dir == "d" | dir == "u"){
      y <- y + dy[dir_n]
    }else{
      x <- x + dx[dir_n]
    }
  }else if(mt[y, x] == "+"){
    if(dir == "d" | dir == "u"){
      if(mt[y, x + 1] == "-"){
        dir <- "r"
      }else{
        dir <- "l"
      }
    }else{
      if(mt[y + 1, x] == "|"){
        dir <- "d"
      }else{
        dir <- "u"
      }
    }
    dir_n <- which(dir_c == dir)
    x <- x + dx[dir_n]
    y <- y + dy[dir_n]
  }else{
    word <- c(word, mt[y, x])
    if(dir == "d" | dir == "u"){
      y <- y + dy[dir_n]
    }else{
      x <- x + dx[dir_n]
    }
    if(mt[y, x] == " "){
      break
    }
  }
}

# Part One
result <- paste0(word,collapse = "")
cat(result)

# Part Two
result <- count
cat(result)
