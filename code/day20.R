### ADVENT OF CODE, DAY 20
# PART 1

input <- read.delim("input/day20.txt", header = FALSE,
                    blank.lines.skip = TRUE)

library(collections)

images <- dict()

#convert to dict
for (i in 1:144){
  tile <- gsub("[Tile :]", "", input[1+11*(i-1),1])
  start <- 2 + 11*(i-1)
  finish <- 11 + 11*(i-1)
  images$set(as.numeric(tile), input[start:finish,1])
}

#parse tiles into arrays
for (key in images$keys()){
  array <- images$get(key)
  array <- unlist(strsplit(as.character(array), split = ""))
  array <- matrix(array, nrow = 10, ncol = 10)
  images$set(key, array)
}

#function tha checks if two tiles can be adjacent
check_two <- function(matrix1, matrix2){
  
  # load in matrix1 sides and matrix2 sides
  bot1 <- matrix1[1:10,1]
  top1 <- matrix1[1:10,10]
  left1 <- matrix1[1, 1:10]
  right1 <- matrix1[10, 1:10]
  bot2 <- matrix2[1:10,1]
  top2 <- matrix2[1:10,10]
  left2 <- matrix2[1, 1:10]
  right2 <- matrix2[10, 1:10]
  
  sides1 <- c(paste0(bot1, collapse = ""),
              paste0(top1, collapse = ""),
              paste0(left1, collapse = ""),
              paste0(right1, collapse = ""),
              paste0(rev(bot1), collapse = ""),
              paste0(rev(top1), collapse = ""),
              paste0(rev(left1), collapse = ""),
              paste0(rev(right1), collapse = ""))
  
  sides2 <- c(paste0(bot2, collapse = ""),
              paste0(top2, collapse = ""),
              paste0(left2, collapse = ""),
              paste0(right2, collapse = ""),
              paste0(rev(bot2), collapse = ""),
              paste0(rev(top2), collapse = ""),
              paste0(rev(left2), collapse = ""),
              paste0(rev(right2), collapse = ""))
  
  for (i in 1:8){
    for (j in 1:8){
      if (sides1[i] == sides2[j]){
        return(TRUE)
      }
    }
  }

  return(FALSE) # if no matches, return false
}

# iterate through all keys to check for matches, and keep track of
# how many neighbors each tile has, along with which tiles belong
# to its set of neighbors
key_list <- c()
num_neigh <- c()
neighbors <- dict()
for (key1 in images$keys()){
  num <- 0
  for (key2 in images$keys()){
    if (key1 == key2){
      next
    }
    if (check_two(images$get(key1), images$get(key2))){
      if (key1 %in% neighbors$keys()){
        neighbors$set(key1, c(neighbors$get(key1), key2))
      } else {
        neighbors$set(key1, c(key2))
      }
      num <- num + 1
    }
  }
  key_list <- c(key_list, key1)
  num_neigh <- c(num_neigh, num)
}

keys_neigh <- data.frame(key = key_list, num = num_neigh)
print(prod(keys_neigh[keys_neigh$num == 2,"key"]), digits = 20) #corners have 2 neighbors


### PART 2
# with number of neighbors in hand, and which arrays neighbor which, let's construct
# the entire grid first, and then do rotations and flips to make things match

corners <- keys_neigh[keys_neigh$num == 2, "key"]
sides <- keys_neigh[keys_neigh$num == 3, "key"]
insides <- keys_neigh[keys_neigh$num == 4, "key"]

# initialize empty 12x12 array to store tile locations
image_locs <- matrix(NA, nrow = 12, ncol = 12)

# WLOG, pick a corner to start at [1,1], and then pick a neighbor and stick it in [2,1]
image_locs[1,1] <- corners[1]
image_locs[2,1] <- neighbors$get(image_locs[1,1])[1]
image_locs[1,2] <- neighbors$get(image_locs[1,1])[2]
neighbors$set(image_locs[1,1], c()) # to specify that we have used all neighbors attached to this key
neighbors$set(image_locs[2,1], neighbors$get(image_locs[2,1])[neighbors$get(image_locs[2,1]) != image_locs[1,1]])
neighbors$set(image_locs[1,2], neighbors$get(image_locs[1,2])[neighbors$get(image_locs[1,2]) != image_locs[1,1]])

# next, build out the edges horizontally/vertically
# build [1:12,1] first
for (i in 2:11){
  curr_image <- image_locs[i,1]
  if (i < 11){
    for (key in sides){
      if (curr_image %in% neighbors$get(key)){ #find adjacent side, and add to array
        image_locs[i+1,1] <- key
        neighbors$set(key, neighbors$get(key)[neighbors$get(key) != curr_image]) #remove neighbor adjacency
        neighbors$set(curr_image, neighbors$get(curr_image)[neighbors$get(curr_image) != key])
        break
      }
    }
  } else { # at i = 11, we will look for a corner piece to place in i =12
    for (key in corners){
      if (curr_image %in% neighbors$get(key)){ #find adjacent side, and add to array
        image_locs[i+1,1] <- key
        neighbors$set(key, neighbors$get(key)[neighbors$get(key) != curr_image]) #remove neighbor adjacency
        neighbors$set(curr_image, neighbors$get(curr_image)[neighbors$get(curr_image) != key])
        break
      }
    }
  }
}

#next, build [1,1:12]
for (i in 2:11){
  curr_image <- image_locs[1,i]
  if (i < 11){
    for (key in sides){
      if (curr_image %in% neighbors$get(key)){ #find adjacent side, and add to array
        image_locs[1,i+1] <- key
        neighbors$set(key, neighbors$get(key)[neighbors$get(key) != curr_image]) #remove neighbor adjacency
        neighbors$set(curr_image, neighbors$get(curr_image)[neighbors$get(curr_image) != key])
        break
      }
    }
  } else {
    for (key in corners){
      if (curr_image %in% neighbors$get(key)){ #find adjacent side, and add to array
        image_locs[1,i+1] <- key
        neighbors$set(key, neighbors$get(key)[neighbors$get(key) != curr_image]) #remove neighbor adjacency
        neighbors$set(curr_image, neighbors$get(curr_image)[neighbors$get(curr_image) != key])
        break
      }
    }
  }
}

#next, build [12, 1:12]
for (i in 1:11){
  curr_image <- image_locs[12,i]
  
  if (i < 11){
    for (key in sides){
      if (curr_image %in% neighbors$get(key)){ #find adjacent side, and add to array
        image_locs[12,i+1] <- key
        neighbors$set(key, neighbors$get(key)[neighbors$get(key) != curr_image]) #remove neighbor adjacency
        neighbors$set(curr_image, neighbors$get(curr_image)[neighbors$get(curr_image) != key])
        break
      }
    }
  } else {
    for (key in corners){
      if (curr_image %in% neighbors$get(key)){ #find adjacent side, and add to array
        image_locs[12,i+1] <- key
        neighbors$set(key, neighbors$get(key)[neighbors$get(key) != curr_image]) #remove neighbor adjacency
        neighbors$set(curr_image, neighbors$get(curr_image)[neighbors$get(curr_image) != key])
        break
      }
    }
  }
}

# last, build [1:12, 12]
for (i in 1:11){ #for redundancy, hit the last corner again
  curr_image <- image_locs[i,12]
  
  if (i < 11){
    for (key in sides){
      if (curr_image %in% neighbors$get(key)){ #find adjacent side, and add to array
        image_locs[i+1,12] <- key
        neighbors$set(key, neighbors$get(key)[neighbors$get(key) != curr_image]) #remove neighbor adjacency
        neighbors$set(curr_image, neighbors$get(curr_image)[neighbors$get(curr_image) != key])
        break
      }
    }
  } else {
    for (key in corners){
      if (curr_image %in% neighbors$get(key)){ #find adjacent side, and add to array
        image_locs[i+1,12] <- key
        neighbors$set(key, neighbors$get(key)[neighbors$get(key) != curr_image]) #remove neighbor adjacency
        neighbors$set(curr_image, neighbors$get(curr_image)[neighbors$get(curr_image) != key])
        break
      }
    }
  }
}

# with the outside in place, let's build [1:12,j] for 2 <= j <= 11
for (j in 2:11){
  for (i in 2:11){
    # need to find tile with 2 matching neighbors, [i,j-1] and [i-1,j]
    n_neigh <- c(image_locs[i,j-1], image_locs[i-1,j])
    
    for (key in insides){
      if (prod(n_neigh %in% neighbors$get(key))){
        image_locs[i,j] <- key
        for (k in 1:length(n_neigh)){
          neighbors$set(key, neighbors$get(key)[neighbors$get(key) != n_neigh[k]]) #remove neighbor adjacency
          neighbors$set(n_neigh[k], neighbors$get(n_neigh[k])[neighbors$get(n_neigh[k]) != key])
        }
        break
      }
    }
  }
}

### now we need to orient the map correctly!!!!! start by arranging [1,1], [2,1] and [1,2]

flip_x <- function(matrix){ #flip around x-axis
  matrix <- matrix[10:1, 1:10]
  return(matrix)
}

flip_y <- function(matrix){ #don't think we actually need this
  matrix <- matrix[1:10, 10:1]
  return(matrix)
}

rotate_cw <- function(matrix){ #rotate cw
  matrix <- t(apply(matrix, 2, rev))
  return(matrix)
}

# function to take in a matrix1, and return the matrices re-oriented
# such that it fits to the right; allow rotation of both matrices for this first step
find_orientation_init <- function(matrix1, matrix2){ 
  rotate_rem_1 <- 4
  while(rotate_rem_1 > 0){
    
    matrix1 <- rotate_cw(matrix1)
    right1 <- matrix1[1:10, 10]
    
    rotate_rem <- 4
    while(rotate_rem > 0){
      matrix2 <- rotate_cw(matrix2)
      left2 <- matrix2[1:10, 1]
      
      if (identical(right1, left2)){
        return(list(matrix1, matrix2))
      } else {
        rotate_rem <- rotate_rem - 1
      }
    }
    
    matrix2 <- flip_x(matrix2)
    rotate_rem <- 4
    while(rotate_rem > 0){
      matrix2 <- rotate_cw(matrix2)
      left2 <- matrix2[1:10, 1]
      
      if (identical(right1, left2)){
        return(list(matrix1, matrix2))
      } else {
        rotate_rem <- rotate_rem - 1
      }
    }
    
    rotate_rem_1 <- rotate_rem_1 - 1
  }
  print("NO MATCH DETECTED")
}

# similar as above function, but instead assumes matrix1 is fixed, so re-orients matrix2 to fit
# correctly and then returns matrix2
find_orientation <- function(matrix1, matrix2){
  right1 <- matrix1[1:10, 10]
  
  rotate_rem <- 4
  while(rotate_rem > 0){
    matrix2 <- rotate_cw(matrix2)
    left2 <- matrix2[1:10, 1]
    
    if (identical(right1, left2)){
      return(matrix2)
    } else {
      rotate_rem <- rotate_rem - 1
    }
  }
  
  matrix2 <- flip_x(matrix2)
  rotate_rem <- 4
  while(rotate_rem > 0){
    matrix2 <- rotate_cw(matrix2)
    left2 <- matrix2[1:10, 1]
    
    if (identical(right1, left2)){
      return(matrix2)
    } else {
      rotate_rem <- rotate_rem - 1
    }
  }
  
  matrix2 <- flip_y(matrix2)
  rotate_rem <- 4
  while(rotate_rem > 0){
    matrix2 <- rotate_cw(matrix2)
    left2 <- matrix2[1, 1:10]
    
    if (identical(right1, left2)){
      return(matrix2)
    } else {
      rotate_rem <- rotate_rem - 1
    }
  }
  print("NO MATCH DETECTED")
}

# find_orientation was for left-right, need a function for top-bottom, again
# returns matrix2 and assumes matrix1 is fixed
find_orientation_topbot <- function(matrix1, matrix2){
  top1 <- matrix1[10, 1:10]
  
  rotate_rem <- 4
  while(rotate_rem > 0){
    matrix2 <- rotate_cw(matrix2)
    bot2 <- matrix2[1, 1:10]
    
    if (identical(top1, bot2)){
      return(matrix2)
    } else {
      rotate_rem <- rotate_rem - 1
    }
  }
  
  matrix2 <- flip_x(matrix2)
  rotate_rem <- 4
  while(rotate_rem > 0){
    matrix2 <- rotate_cw(matrix2)
    bot2 <- matrix2[1, 1:10]
    
    if (identical(top1, bot2)){
      return(matrix2)
    } else {
      rotate_rem <- rotate_rem - 1
    }
  }
  
  matrix2 <- flip_y(matrix2)
  rotate_rem <- 4
  while(rotate_rem > 0){
    matrix2 <- rotate_cw(matrix2)
    bot2 <- matrix2[1, 1:10]
    
    if (identical(top1, bot2)){
      return(matrix2)
    } else {
      rotate_rem <- rotate_rem - 1
    }
  }
  print("NO MATCH DETECTED")
}

### with helper functions in place, let's orient the image, again starting from
# [1,1] and [1,2]

temp <- find_orientation_init(images$get(image_locs[1,1]), images$get(image_locs[1,2]))
images$set(image_locs[1,1], temp[[1]])
images$set(image_locs[1,2], temp[[2]])

for (i in 3:12){
  temp <- find_orientation(images$get(image_locs[1,i-1]), images$get(image_locs[1,i]))
  images$set(image_locs[1,i], temp)
}

# top row oriented! so that's [1,1:12]
# next, we orient the following rows, with the 1st tile in the row oriented
# to match the preceding tile in the same column

for (j in 2:12){
  for (i in 1:12){
    if (i == 1){ # for 1st column, need to orient with respect to row above, can do that by applying rotations
      temp <- find_orientation_topbot(images$get(image_locs[j-1,i]), images$get(image_locs[j,i]))
      images$set(image_locs[j,i], temp)
    } else {
      temp <- find_orientation(images$get(image_locs[j,i-1]), images$get(image_locs[j,i]))
      images$set(image_locs[j,i], temp)
    }
  }
}

# images are oriented correctly in the dict, now need to construct the full image
# each image will be 8x8 (remove outside edges), and a total of 12x12 images, so need a total array of 96x96
complete_image <- matrix(NA, ncol = 96, nrow = 96)

for (i in 1:12){
  for (j in 1:12){
    complete_image[(8*(i-1)+1):(8*(i-1)+8),(8*(j-1)+1):(8*(j-1)+8)] <- images$get(image_locs[i,j])[2:9, 2:9]
  }
}

# replace "#" with 1 and "." with 0 so that we can sum over appropriate squares
complete_image[complete_image == "#"] <- 1
complete_image[complete_image == "."] <- 0
complete_image <- matrix(as.numeric(complete_image), ncol = 96, nrow = 96)

# with complete image built, now need to prase through it a total of 8 times for sea monsters!

                  # 
#    ##    ##    ###
 #  #  #  #  #  #   

#using Rs extremely bad positional matrix that we've been using anyway.... we are looking for, then:
#from a starting point of (i,j) being the starting point of the tail:

flip_y_CI <- function(matrix){ # for flipping the complete image
  matrix <- matrix[1:96, 96:1]
  return(matrix)
}

### there are 8 orientations to check:
# unflipped: no rotate, rotate once, rotate twice, rotate thrice
# flipped: no rotate, rotate once, rotate twice, rotate thrice
# let's just iterate through all of them, since sea monsters can only be found in one orientation

count_monsters <- function(complete_image){
  monsters_found <- 0
  for (i in 2:95){ # sea monster is 20 wide, 3 tall
    for (j in 1:77){
      monster_check <- complete_image[i,j] + complete_image[i+1,j+1] + complete_image[i+1, j+4] + complete_image[i, j+5] +
        complete_image[i, j+6] + complete_image[i+1, j+7] + complete_image[i+1, j+10] + complete_image[i, j+11] +
        complete_image[i, j+12] + complete_image[i+1, j+13] + complete_image[i+1, j+16] + complete_image[i, j+17] +
        complete_image[i, j+18] + complete_image[i, j+19] + complete_image[i-1, j+18]
      
      if (monster_check == 15){
        monsters_found <- monsters_found + 1
      }
    }
  }
  return(monsters_found)
}

monsters_found <- count_monsters(complete_image) +
  count_monsters(rotate_cw(complete_image)) + count_monsters(rotate_cw(rotate_cw(complete_image))) +
  count_monsters(rotate_cw(rotate_cw(rotate_cw(complete_image)))) + count_monsters(flip_y_CI(complete_image)) +
  count_monsters(rotate_cw(flip_y_CI(complete_image))) +
  count_monsters(rotate_cw(rotate_cw(flip_y_CI(complete_image)))) +
  count_monsters(rotate_cw(rotate_cw(rotate_cw(flip_y_CI(complete_image)))))

### want to count up total number of 1s in map - number of 1s in monsters (monsters are 15 spaces)
print(sum(complete_image) - 15*monsters_found)

