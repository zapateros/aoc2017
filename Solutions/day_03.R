input <- readLines("input_day_03.txt")
inp   <- as.numeric(input)

# Part One
# Find matrix dimensions (odd n)
n_in <- ceiling(sqrt(inp))
if(n_in %% 2 == 0){
  n_in <- n_in + 1
}

# Initialize matrix
n       <- n_in
mt      <- matrix(rep(0,n*n),ncol=n)
center  <- (n-1)/2 + 1
x       <- center 
y       <- center 
mt[y,x] <- 1
dir     <- 1

# Direction vectors
dir_x <- c(0, 1, 0, -1)
dir_y <- c(1, 0, -1, 0)

# Determine direction and fill with numbers
for(i in 2:(n * n)){
  dir_n <- dir %% 4 + 1
  y_l   <- y + dir_y[dir_n]
  x_l   <- x + dir_x[dir_n]
  lft   <- mt[y_l, x_l]
  if(lft == 0){
    y   <- y_l
    x   <- x_l
    dir <- dir_n
  }else{
    y <- y + dir_y[dir]
    x <- x + dir_x[dir]
  }
  mt[y, x] <- i
}

# Find Manhattan Distance
inds  <- which(mt == inp, arr.ind = TRUE)
man_x <- abs(center - inds[2])
man_y <- abs(center - inds[1])
man_r <- man_x + man_y
cat(man_r)


# Part Two
# run part one with n = 15 and then the next script
n       <- 15
mt2     <- matrix(rep(0,n*n),ncol=n)
center  <- (n-1)/2 + 1
x       <- center 
y       <- center 
mt2[y,x] <- 1

# Functions
nb_sum <- function(x, y, matrix){
  sms <<- matrix[y + 1, x] + matrix[y - 1, x] + 
    matrix[y - 1, x - 1] + matrix[y, x - 1] + matrix[y + 1, x - 1] +
    matrix[y - 1, x + 1] + matrix[y, x + 1] + matrix[y + 1, x + 1]
}

i<-1
while(TRUE){
  i<- i + 1
  inds <- which(mt == i, arr.ind = TRUE)
  nb_sum(inds[2],inds[1],mt2)
  mt2[inds[1],inds[2]] <- sms
  if(sms > inp){
    break
    }
}
cat(sms)



