input <- readLines("input_day_07.txt")
inp <- gsub("-> ","",input)
inp <- gsub(",","",inp)
inp <- gsub("\\(","",inp)
inp <- gsub("\\)","",inp)

# Part One
dg  <- NULL
mt  <- NULL
for(i in 1:length(inp)){
  rel <- unlist(strsplit(inp[i]," "))
  rl  <- rel[-2]
  mt  <- c(mt,rl[1])
  if(length(rl) > 1){
    dg <- c(dg, rl[-1])
  }
}
bottom <- mt[!(mt%in%dg)]
cat(bottom)


# Part Two
md <- NULL
base <- lapply(inp, function(x){
  y <- unlist(strsplit(x," "))
  ds <- length(y) - 2
  if(length(y) > 2){
    md <<- rbind(md, cbind(y[1],c(y[1],y[-c(1,2)])))
  }
  c(y[c(1,2)], ds)
})
base   <- matrix(unlist(base),ncol = 3, byrow = TRUE)
base   <- setNames(as.data.frame(base),c("disc","weight","daughters"))
md     <- setNames(as.data.frame(md),c("mother","daughter"))
df     <- merge(md, base, by.x = "daughter", by.y = "disc")
df[,1] <- as.character(df[,1])
df[,2] <- as.character(df[,2])
df[,3] <- as.numeric(as.character(df[,3]))
df[,4] <- as.numeric(as.character(df[,4]))

i  <- 0
un <- unique(df$mother)
while(TRUE){
  if(i == length(un)){
    un <- unique(df$mother)
    i  <- 0
  }
  i   <- i + 1
  rs  <- which(df$mother==un[i])
  rel <- df[rs,]
  ds  <- rel[which(rel$daughters == 0),]
  m   <- rel[which(rel$daughter == un[i]),]
  if(nrow(ds) == m$daughters){
    if(length(unique(ds$weight)) != 1){
      break
    }
    sm <- sum(rel$weight)
    df <- df[-rs,]
    dd <- which(df$daughter == un[i])
    df[dd,3] <- sm  
    df[dd,4] <- 0 
  }
}

# Only works for more than two daughters
tb <- table(ds$weight)
wr <- as.numeric(names(which(tb == 1)))
gd <- as.numeric(names(which(tb != 1)))
dif <- gd - wr
wd <- ds$daughter[which(ds$weight == wr)]
we <- as.numeric(as.character(base$weight[which(base$disc == wd)]))
result <- we + dif
cat(result)
