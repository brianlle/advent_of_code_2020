### ADVENT OF CODE, DAY 17
# PART 1

input <- read.delim("input/day17.txt", header = FALSE,
                    blank.lines.skip = FALSE)

data <- paste0(input$V1, collapse = "")


# 1/# is active, 0/. is inactive, convert to list of 0s and 1s
data <- gsub("#", "1", data)
data <- gsub("\\.", "0", data)
data <- strsplit(data, split = "")[[1]]
data <- as.numeric(data)

# populate an array, and stick starting data in the middle
#only iterating 6 times, so z only needs to be 7 +/- 6
#x and y are 8x8, going 6 in either direction, so 1-7:14-20 should be enough
data <- matrix(data, nrow = nrow(input))
data_arr <- array(rep(0, 20*20*13), dim = c(20,20,13)) #set initial slice to z = 10
data_arr[7:14,7:14,7] <- data

check_neighbors <- function(x,y,z,data){
  value = 0
  
  x_min <- max(1, x-1)
  x_max <- min(20, x+1)
  y_min <- max(1, y-1)
  y_max <- min(20, y+1)
  z_min <- max(1, z-1)
  z_max <- min(13, z+1)
  
  for (i in x_min:x_max){
    for (j in y_min:y_max){
      for (k in z_min:z_max){
        if (i==x & j==y & k==z){
          #do nothing
        } else {
          value <- value + data[i,j,k]
        }
      }
    }
  }
  return(value)
}

iterate <- function(x, y, z, data){
  neighbors <- check_neighbors(x, y, z, data)
  
  if (data[x,y,z] == 1){
    if (!neighbors %in% c(2,3)){
      return(0)
    } else {
      return(1)
    }
  } else if (data[x,y,z] == 0){
    if (neighbors == 3){
      return(1)
    } else {
      return(0)
    }
  } else { print("help")}
}

iterate_all <- function(data){
  data2 <- data # make a copy to return
  for (i in 1:20){
    for (j in 1:20){
      for (k in 1:13){
        data2[i,j,k] <- iterate(i,j,k,data)
      }
    }
  }
  return(data2)
}

### test case
# .#.
# ..#
# ###
# 
# test <- t(matrix(c(0,1,0,0,0,1,1,1,1), nrow = 3, ncol = 3))
# 
# data_arr2 <- array(rep(0, 30*30*19), dim = c(30,30,19)) #set initial slice to z = 10
# data_arr2[11:13,11:13,10] <- test
# 
# data3 <- iterate_all(data_arr2)
# data3 <- iterate_all(data3)
# sum(data3) #11 then 21,... 6 times: 112, nice that's right

for (iter in 1:6){
  data_arr <- iterate_all(data_arr)
}
sum(data_arr)


### PART 2
# literally part 1 with a 4th dimension.... ok, treat exact same as z

data <- paste0(input$V1, collapse = "")

# 1/# is active, 0/. is inactive
data <- gsub("#", "1", data)
data <- gsub("\\.", "0", data)
data <- strsplit(data, split = "")[[1]]
data <- as.numeric(data)

data <- matrix(data, nrow = nrow(input))
data_arr <- array(rep(0, 20*20*13*13), dim = c(20,20,13,13)) #set initial slice to z = 10
data_arr[7:14,7:14,7,7] <- data

check_neighbors <- function(x,y,z,w,data){
  value = 0
  
  x_min <- max(1, x-1)
  x_max <- min(20, x+1)
  y_min <- max(1, y-1)
  y_max <- min(20, y+1)
  z_min <- max(1, z-1)
  z_max <- min(13, z+1)
  w_min <- max(1, w-1)
  w_max <- min(13, w+1)
  
  for (i in x_min:x_max){
    for (j in y_min:y_max){
      for (k in z_min:z_max){
        for (l in w_min:w_max){
          if (i==x & j==y & k==z & l==w){
            #do nothing
          } else {
            value <- value + data[i,j,k,l]
          }
        }
      }
    }
  }
  return(value)
}

iterate <- function(x, y, z, w, data){
  neighbors <- check_neighbors(x, y, z, w, data)
  
  if (data[x,y,z,w] == 1){
    if (!neighbors %in% c(2,3)){
      return(0)
    } else {
      return(1)
    }
  } else if (data[x,y,z,w] == 0){
    if (neighbors == 3){
      return(1)
    } else {
      return(0)
    }
  } else { print("help")}
}

iterate_all <- function(data){
  data2 <- data
  
  for (i in 1:20){
    for (j in 1:20){
      for (k in 1:13){
        for (l in 1:13){
          data2[i,j,k,l] <- iterate(i,j,k,l,data)
        }
      }
    }
  }
  return(data2)
}

for (iter in 1:6){
  data_arr <- iterate_all(data_arr)
}
sum(data_arr)
