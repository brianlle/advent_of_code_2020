### ADVENT OF CODE, DAY 11
# PART 1

input <- read.csv("input/day11.txt", header = FALSE, sep = "")

#split rows into columns of width-1, add buffers to all 4 sides
library(stringr)
input$V1 <- paste0(".", input$V1, ".")
input <- str_split_fixed(input$V1, "", 100)
input <- rbind(rep(".", 100), input)
input <- rbind(input, rep(".", 100))

seats <- input
occupied <- matrix(0, 101, 100)

#0 is empty, 1 is occupied
check_neighbors <- function(matrix, x, y){
  surround <- (matrix[x+1, y+1] + matrix[x+1, y] + matrix[x+1, y-1] + matrix[x, y-1] +
                 matrix[x-1, y-1] + matrix[x-1, y] + matrix[x-1, y+1] + matrix[x, y+1])
  return(surround)
}

# function to update every seat in matrix based on occupied neighbors
iterate <- function(matrix){
  matrix2 <- matrix
  for (x in 2:100){
    for (y in 2:99){
      if (seats[x,y] == "L"){
        surround <- check_neighbors(matrix, x, y)
        if (matrix2[x,y] == 0){
          if (surround == 0){
            matrix2[x,y] <- 1
          }
        } else if (matrix[x,y] == 1){
          if (surround >= 4){
            matrix2[x,y] <- 0
          }
        }
      }
    }
  }
  return(matrix2)
}

occupied_2 <- NA
while (TRUE){
  occupied_2 <- iterate(occupied)
  if (identical(occupied, occupied_2)){
    break
  }
  occupied <- occupied_2
}


# PART 2

# helper function to check if we are at an edge as we iterate line of sight
check_if_edge <- function(x, y){
  if (x %in% c(1, 101)){
    return(TRUE)
  } else if (y %in% c(1, 100)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}
  
#0 is empty, 1 is occupied
#in R with dataframes, delta-x technically iterates vertically and delta-y horizontally
#really clunky code checking each of 8 directions
check_diags <- function(matrix, x, y){
  left = 0
  right = 0
  up = 0
  down = 0
  upleft = 0
  upright = 0
  downleft = 0
  downright = 0
  
  i = 1
  while(TRUE){ #left
    if (check_if_edge(x,y) | check_if_edge(x, y-i+1)){
      break
    } else if (seats[x, y-i] == "L"){
      left = matrix[x, y-i]
      break
    } else {
      i = i + 1
    }
  }
  
  i = 1
  while(TRUE){ #right
    if (check_if_edge(x,y) | check_if_edge(x, y+i-1)){
      break
    } else if (seats[x, y+i] == "L"){
      right = matrix[x, y+i]
      break
    } else  {
      i = i + 1
    }
  }
  
  i = 1
  while(TRUE){ #down
    if (check_if_edge(x,y) | check_if_edge(x+i-1, y)){
      break
    } else if (seats[x+i, y] == "L"){
      down = matrix[x+i, y]
      break
    } else {
      i = i + 1
    }
  }

  i = 1
  while(TRUE){ #up
    if (check_if_edge(x,y) | check_if_edge(x-i+1, y)){
      break
    } else if (seats[x-i, y] == "L"){
      up = matrix[x-i, y]
      break
    } else  {
      i = i + 1
    }
  }

  i = 1
  while(TRUE){ #upleft
    if (check_if_edge(x,y) | check_if_edge(x-i+1, y-i+1)){
      break
    } else if (seats[x-i, y-i] == "L"){
      upleft = matrix[x-i, y-i]
      break
    } else  {
      i = i + 1
    }
  }
  
  i = 1
  while(TRUE){ #upright
    if (check_if_edge(x,y) | check_if_edge(x-i+1, y+i-1)){
      break
    } else if (seats[x-i, y+i] == "L"){
      upright = matrix[x-i, y+i]
      break
    } else  {
      i = i + 1
    }
  }
  
  i = 1
  while(TRUE){ #downleft
    if (check_if_edge(x,y) | check_if_edge(x+i-1, y-i+1)){
      break
    }  else if (seats[x+i, y-i] == "L"){
      downleft = matrix[x+i, y-i]
      break
    } else  {
      i = i + 1
    }
  }
  
  i = 1
  while(TRUE){ #downright
    if (check_if_edge(x,y) | check_if_edge(x+i-1, y+i-1)){
      break
    } else if (seats[x+i, y+i] == "L"){
      downright = matrix[x+i, y+i]
      break
    } else  {
      i = i + 1
    }
  }
return(up+down+left+right+upleft+upright+downleft+downright)
}

# function to update every seat in matrix based on occupied neighbors
iterate <- function(matrix){
  matrix2 <- matrix
  for (x in 2:100){
    for (y in 2:99){
      if (seats[x,y] == "L"){
        surround <- check_diags(matrix,x,y)
        if (matrix2[x,y] == 0){
          if (surround == 0){
            matrix2[x,y] <- 1
          }
        } else if (matrix[x,y] == 1){
          if (surround >= 5){
            matrix2[x,y] <- 0
          }
        }
      }
    }
  }
  return(matrix2)
}

occupied <- matrix(0, 101, 100)
occupied_2 <- NA

while (TRUE){
  print(i)
  occupied_2 <- iterate(occupied)
  if (identical(occupied, occupied_2)){
    break
  }
  occupied <- occupied_2
}
