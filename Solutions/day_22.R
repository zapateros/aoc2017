input    <- readLines("input_day_22.txt")
mat1     <- matrix(unlist(strsplit(input, "")), ncol = nchar(input[1]), byrow = TRUE)
infected <- which(mat1 == "#", arr.ind = TRUE)

# Initiate
x   <- 13
y   <- 13
dir <- 4
dx  <- c(1, 0, -1, 0)
dy  <- c(0, 1, 0, -1)

# Part One
count <- 0
for(i in 1:10000){
  node <- which(infected[, 1] == y & infected[, 2] == x)
  if(length(node) == 0){
    infected <- rbind(infected, c(y, x))
    dir      <- ((dir - 2) %% 4) + 1
    count    <- count + 1
  }else{
    infected <- infected[-node,]
    dir      <- (dir %% 4) + 1
  }
  x <- x + dx[dir]
  y <- y + dy[dir]
}
cat(count)

# Part Two
# Infected: 1, Weakened: 2, Flagged: 3
left  <- matrix(rep(".", 25*500), nrow = 25)
up    <- matrix(rep(".", 500 * 1025), nrow = 500)
mat   <- rbind(up, cbind(left, mat1, left), up)
x     <- 513
y     <- 513
count <- 0
for(i in 1:10000000){
  node <- mat[y, x]
  if(node == "."){
    mat[y, x] <- "W"
    dir       <- ((dir - 2) %% 4) + 1
  }else if(node == "#"){
    mat[y, x] <- "F"
    dir       <- (dir %% 4) + 1
  }else if(node == "W"){
    mat[y, x] <- "#"
    count     <- count + 1
  }else if(node == "F"){
    mat[y, x] <- "."
    dir       <- ((dir + 1) %% 4) + 1
    }
  x <- x + dx[dir]
  y <- y + dy[dir]
}
cat(count)
