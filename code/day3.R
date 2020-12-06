### ADVENT OF CODE, DAY 3
# Given pattern, traversing from top-left until you exit moving right 3, down 1
# how many trees are hit?

# modulo operator is %%

input <- read.delim("input/day3.txt", header = FALSE)

trees_hit = 0

for (i in 1:nrow(input)-1){
  x = 1 + (3*i) %% nchar(as.character(input[1,1]))
  y = 1 + i
  print(paste(x,y))
  
  if (substr(input[y, "V1"], x, x) == "#"){
    trees_hit = trees_hit + 1
  }
}

# Part 2: do the same thing, but for 5 slopes, and multiply together
# Slopes: right 1, down 1; right 3, down 1; right 5, down 1; right 7, down 1; right 1, down 2

trees <- function(input, x_move, y_move){
  trees_hit = 0
  if (y_move == 1){
    num_rows = nrow(input) - 1
  } else {num_rows = floor(nrow(input)/y_move)}
  for (i in 1:num_rows){
    x = 1 + (x_move*i) %% nchar(as.character(input[1,1]))
    y = 1 + y_move*i
    #print(paste(x,y))
    
    if (substr(input[y, "V1"], x, x) == "#"){
      trees_hit = trees_hit + 1
    }
  }
  
  return(trees_hit)
}

trees(input, 1, 1)*trees(input, 3, 1)*trees(input, 5, 1)*trees(input, 7, 1)*trees(input, 1, 2)
