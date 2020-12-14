### ADVENT OF CODE, DAY 14
# PART 1

input <- read.csv("input/day14.txt", header = FALSE, sep = "", stringsAsFactors = FALSE)

library(binaryLogic)

mem <- rep(0, 99999)

apply_mask <- function(int, mask){
  int_list <- as.binary(int, n = 36) #index 36 is 2^0, index 35 is 2^1, etc.
  
  for (j in 1:length(mask)){ #assume max is 36-long, index 1 is 2^0, etc
    if (mask[j] != "X"){
      int_list[j] <- as.integer(mask[j])
    }
  }
  
  int <- as.numeric(int_list)
  return(int)
}

input$mem <- gsub("]", "", input$V1)
input$mem <- gsub("\\[", "", input$mem)
input$mem <- gsub("[a-z]", "", input$mem)
input$val <- input$V3

for (i in 1:nrow(input)){
  if (input[i,1] == "mask"){
    mask <- strsplit(input[i,5], split = "")[[1]]
  }
  else {
    mem[as.numeric(input[i,4])] <- apply_mask(as.numeric(input[i,5]), mask)
  }
}

sum(mem)
print(sum(mem), digits = 20)

### PART 2

apply_mask <- function(int, mask){
  output_list <- c()
  int_list <- as.binary(int, n = 36) #index 36 is 2^0, index 35 is 2^1, etc.
  x_locs <- c()
  
  for (j in 1:length(mask)){ #assume max is 36-long, index 36 is 2^0, etc
    if (mask[j] == "1"){
      int_list[j] <- 1
    } else if (mask[j] == "X"){
      x_locs <- c(x_locs, j)
    }
  }
  
  if ("X" %in% mask){ # all wildcards can be 0 or 1, so replacing all together with binary perms
    possibilities <- 2^(length(x_locs))
    
    for (i in 1:possibilities){
      temp_int <- int_list
      temp_int[x_locs] <- as.binary(i-1, n = length(x_locs))
      output_list <- c(output_list, as.numeric(temp_int))
    }
  } else {
    output_list <- c(output_list, as.numeric(int_list))
  }
  # construct list of output ints, where Xs can be wildcards

  return(output_list)
}

#store values in a pseudo-dict using a data.frame
mem <- data.frame(location = integer(), value = integer())

input <- read.csv("input/day14.txt", header = FALSE, sep = "", stringsAsFactors = FALSE)
input$mem <- gsub("]", "", input$V1)
input$mem <- gsub("\\[", "", input$mem)
input$mem <- gsub("[a-z]", "", input$mem)
input$val <- input$V3

for (i in 1:nrow(input)){
  print(i)
  if (input[i,1] == "mask"){
    mask <- strsplit(input[i,5], split = "")[[1]]
  } else {
    output_list <- apply_mask(as.numeric(input[i,4]), mask)
    for (j in 1:length(output_list)){
      if (output_list[j] %in% mem$location){
        mem[mem$location == output_list[j], 2] <- as.numeric(input[i,5])
      } else {
        mem <- rbind(mem, data.frame(location = output_list[j], value = as.numeric(input[i,5])))
      }      
    }
  }
}

sum(mem$value)
print(sum(mem$value), digits = 20)
