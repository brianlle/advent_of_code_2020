### ADVENT OF CODE, DAY 5
# 

input <- read.csv("input/day5.txt", header = FALSE)

divide_in_half <- function(list, letter){
  if (letter %in% c("F", "L")){
    return(list[1:(length(list)/2)])
  } else {
    return(list[((length(list)/2)+1):length(list)])
  }
}

find_row <- function(pattern){
  row <- 0:127
 
  pattern <- substr(pattern, 1, 7)
  for (i in 1:7){
    row <- divide_in_half(row, substr(pattern, i, i))
  }
  return(row)
}

find_column <- function(pattern){
  column <- 0:7
  
  pattern <- substr(pattern, 8, 10)
  for (i in 1:3){
    column <- divide_in_half(column, substr(pattern, i, i))
  }
  return(column)
}

input$row <- NA
input$col <- NA
for (i in 1:nrow(input)){
  input[i,2] <- find_row(input[i,1])
  input[i,3] <- find_column(input[i,1])
}
input$product <- 8*input$row + input$col

#PART 2: find your seat (the missing number)
min(input$product)
max(input$product)

check <- c(input$product, min(input$product):max(input$product))
check <- table(check)
check[check == 1] #714
