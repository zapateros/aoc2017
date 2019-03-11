# Initialize
state  <- 1
steps  <- 12208951
tape   <- 0
cursor <- 1
it     <- 0

# Functions
right <- function(sub_val, sub_state){
  tape[cursor] <<- sub_val
  state <<- sub_state
  cursor <<- cursor + 1
  if(cursor > length(tape)){
    tape <<- c(tape, 0)
  }
}

left <- function(sub_val, sub_state){
  tape[cursor] <<- sub_val
  state <<- sub_state
  if(cursor == 1){
    tape <<- c(0, tape)
  }else{
    cursor <<- cursor - 1
  }
}

# Run Turing machine
while(TRUE){
  it <- it + 1
  val <- tape[cursor]
  if(state == 1){
    if(val == 0){
      right(1, 2)
    }else{
      left(0, 5)
    }
  }else if(state == 2){
    if(val == 0){
      left(1, 3)
    }else{
      right(0, 1)
    }
  }else if(state == 3){
    if(val == 0){
      left(1, 4)
    }else{
      right(0, 3)
    }
  }else if(state == 4){
    if(val == 0){
      left(1, 5)
    }else{
      left(0, 6)
    }
  }else if(state == 5){
    if(val == 0){
      left(1, 1)
    }else{
      left(1, 3)
    }
  }else if(state == 6){
    if(val == 0){
      left(1, 5)
    }else{
      right(1, 1)
    }
  }
  if(it == steps){
    break
  }  
}
