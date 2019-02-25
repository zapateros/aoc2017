input <- readLines("input_day_12.txt")
inp   <- gsub(" <->",",",input)

# Part One
cc   <- "0"
tb_s <- "1"
while(TRUE){
  if(i == length(inp)){
    i <- 1
    if(tb_s == length(table(cc))){
      break
    }
    tb_s <- length(table(cc))
  }
  for(i in 1:length(inp)){
    te <- unlist(strsplit(inp[i],", "))
    if(any(te %in% cc)){
      cc <- c(cc, te)
    }
  } 
}
cat(tb_s)


# Part Two
tt<-NULL
unis <- NULL
# Part Two
for(i in 0:1999){
  cc   <- as.character(i)
  if(!(cc %in% unis)){
    tb_s <- "nn"
    while(TRUE){
      if(i == length(inp)){
        i <- 1
        if(tb_s == length(table(cc))){
          break
        }
        tb_s <- length(table(cc))
      }
      for(i in 1:length(inp)){
        te <- unlist(strsplit(inp[i],", "))
        if(any(te %in% cc)){
          cc <- c(cc, te)
        }
      } 
    }
    unis <- c(unis, cc)
    unis <- unique(unis)
    tt <- c(tt,tb_s) 
    }
}
result <- length(tt)
cat(result)
