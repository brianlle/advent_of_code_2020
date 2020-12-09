### ADVENT OF CODE, DAY 8

input <- read.csv("input/day9.txt", header = FALSE)

is_valid <- function(numbers, position, preamble){
  values_found <- FALSE
  for (i in (position-preamble):(position-1)){
    for (j in (position-preamble+1):position){
      if ((numbers[i] + numbers[j]) == numbers[position]){
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

input <- input$V1
for (i in 26:1000){
  if (!is_valid(input, i, 25)){
    print(paste(i, input[i]))
  }
}

#PART 2
target <- input[501]


for (i in 1:999){
  #print(i)
  list <- c(input[i])
  iter <- i+1
  while (sum(list) < target){
    list <- c(list, input[iter])
    iter <- iter+1
  }
  
  if ((length(list) > 1) & (sum(list) == target)){
    print(paste(i, iter))
    break
  }
}

min(input[387:404]) + max(input[387:404])
