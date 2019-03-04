input <- readLines("input_day_21.txt")
inp   <- gsub("/","", input)
inp   <- gsub(" => ",",",inp)
inp   <- matrix(unlist(strsplit(inp,",")), ncol = 2, byrow = TRUE)

# Rotation function
rotate <- function(x, times){
  if(times > 0){
    for(i in 1:times){
      x <- t(apply(x, 2, rev))
    }
  }
  paste(t(x),collapse="")
} 

# Conversion table with rotations
conv <- NULL
for(i in 1:nrow(inp)){
  rel <- inp[i,]
  mt  <- matrix(unlist(strsplit(rel[1],"")), ncol = sqrt(nchar(rel[1])), byrow = TRUE)
  rot <- c(rotate(mt,0) , rotate(mt, 1), rotate(mt, 2), rotate(mt, 3), 
           rotate(t(mt),0), rotate(t(mt), 1), rotate(t(mt), 2), rotate(t(mt), 3) )  
  conv <- rbind(conv, cbind(rot, inp[i, 2]))
}
conv <- unique(conv)

# Run function
blocks <- function(x){
  mat_c  <- ".#...####"
  out_rc <- matrix(unlist(strsplit(mat_c, "")), ncol = sqrt(nchar(mat_c)), byrow = TRUE)
  it     <- 0
  while(it < x){
    it     <- it + 1
    mat    <- out_rc
    n      <- ncol(mat)
    out_rc <- NULL 
    if(n %% 2 == 0){
      sz <- 2
    }else{
      sz <- 3
    }
    for(i in 1:(n / sz)){
      rs     <- c(1:sz) + (i - 1) * sz
      rel_r  <- mat[rs,]
      out_r  <- NULL
      for(j in 1:(n / sz)){
        cs     <- c(1:sz) + (j - 1) * sz
        rel_rc <- rel_r[, cs]
        rel_ch <- paste(t(rel_rc), collapse = "") 
        repl_c <- conv[which(conv[, 1] == rel_ch), 2]
        repl_m <- matrix(unlist(strsplit(repl_c, "")), ncol = sqrt(nchar(repl_c)), byrow = TRUE) 
        out_r  <- cbind(out_r, repl_m)
      }
      out_rc <- rbind(out_rc, out_r)
    }
  }
  result <- sum(out_rc == "#")
  cat(result)
}

# Part One
blocks(5)

# Part Two
blocks(18)
