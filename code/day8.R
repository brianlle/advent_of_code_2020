### ADVENT OF CODE, DAY 8

input <- read.csv("input/day8.txt", header = FALSE)

input$func <- gsub(" .*", "", input$V1)
input$value <- gsub(".* ", "", input$V1)
input$value <- as.numeric(input$value)

visited <- c()
has_visited <- FALSE
value <- 0
row <- 1

while (has_visited == FALSE){
  if (row %in% visited){
    has_visited <- TRUE
    break
  }
  if (!row %in% visited){
    print(row)
    visited <- c(visited, row)
    if (input[row,2] == "acc"){
      value <- value + input[row, 3]
      row <- row + 1
    } else if (input[row,2] == "jmp"){
      row <- row + input[row, 3]
    } else {
      row <- row + 1
    }
  }
}

# PART 2
for (i in 1:626){
  
  visited <- c()
  has_visited <- FALSE
  value <- 0
  row <- 1
  
  if (input[i, 2] == "jmp"){
    input2 <- input
    input2[i, 2] <- "nop"
    
    while (has_visited == FALSE){
      if (row %in% visited){
        has_visited <- TRUE
        break
      }
      if (row == 626){
        print(paste("finished:", value))
        break
      }
      if (!row %in% visited){
        #print(row)
        visited <- c(visited, row)
        if (input2[row,2] == "acc"){
          value <- value + input2[row, 3]
          row <- row + 1
        } else if (input2[row,2] == "jmp"){
          row <- row + input2[row, 3]
        } else {
          row <- row + 1
        }
      }
    }
  }
  
  if (input2[i, 2] == "nop"){
    input22 <- input2
    input22[i, 2] <- "jmp"
    
    while (has_visited == FALSE){
      if (row %in% visited){
        has_visited <- TRUE
        break
      }
      if (row == 626){
        print(paste("finished:", value))
      }
      if (!row %in% visited){
        #print(row)
        visited <- c(visited, row)
        if (input2[row,2] == "acc"){
          value <- value + input2[row, 3]
          row <- row + 1
        } else if (input2[row,2] == "jmp"){
          row <- row + input2[row, 3]
        } else {
          row <- row + 1
        }
      }
    }
  }
}
