input <- readLines("input_day_16.txt")
inp   <- unlist(strsplit(input, ","))

# Part One
lt <- letters[1:16]
for(i in 1:length(inp)){
  rl    <- inp[i]
  dance <- substr(rl, 1, 1)
  rest  <- substr(rl, 2, nchar(rl))
  sw    <- unlist(strsplit(rest, "/"))
  if(dance == "s"){
    nm <- as.numeric(sw)
    lt <- c(lt[(16 - nm + 1):16],lt[1:(16 - nm )] )
  }else if(dance == "x"){
    nms <- as.numeric(sw) + 1
    fr  <- lt[nms[1]] 
    sc  <- lt[nms[2]] 
    lt[nms[1]] <- sc
    lt[nms[2]] <- fr
  }else if(dance == "p"){
    fr <- which(lt == sw[1])
    sc <- which(lt == sw[2])
    lt[fr] <- sw[2]
    lt[sc] <- sw[1]
  }
}
result <- paste(lt, collapse = "")
cat(result)


# Part Two
stack <- NULL
lt    <- letters[1:16]
n     <- length(inp)
while(TRUE){
   save  <- paste0(c(lt), collapse = "")
   if(save %in% stack){
     break
   }
   stack <- c(stack, save)
   for(i in 1:n){
    rl    <- inp[i]
    dance <- substr(rl, 1, 1)
    rest  <- substr(rl, 2, nchar(rl))
    sw    <- unlist(strsplit(rest, "/"))
    if(dance == "s"){
       nm <- as.numeric(sw)
       lt <- c(lt[(16 - nm + 1):16],lt[1:(16 - nm )] )
    }else if(dance == "x"){
       nms <- as.numeric(sw) + 1
       fr  <- lt[nms[1]] 
       sc  <- lt[nms[2]] 
       lt[nms[1]] <- sc
       lt[nms[2]] <- fr
    }else if(dance == "p"){
       fr <- which(lt == sw[1])
       sc <- which(lt == sw[2])
       lt[fr] <- sw[2]
       lt[sc] <- sw[1]
    }
  }
}
 
pattern <- length(stack)
out     <- 10^9 %% pattern + 1
result  <- stack[out]
cat(result)
  
