### ADVENT OF CODE, DAY 24
# PART 1

input <- read.delim("input/day24.txt", header = FALSE,
                    blank.lines.skip = FALSE)

### 6 directions: e/se/sw/w/nw/we
# let's convert this to almost-cartesian coordinates

# E: move +1/+0
# SE: move 0.5/-0.866
# SW: move -0.5/-0.866
# W: move -1/+0
# NW: move -0.5/+0.866
# NE: move +0.5/+0.866

move_dir <- function(loc, direction){ #loc is list(x,y)
  if (direction == "e"){
    x <- loc[[1]] + 1
    y <- loc[[2]]
    return(list(x,y))
  }
  if (direction == "se"){
    x <- loc[[1]] + 0.5
    y <- loc[[2]] - 0.866
    return(list(x,y))
  }
  if (direction == "sw"){
    x <- loc[[1]] - 0.5
    y <- loc[[2]] - 0.866
    return(list(x,y))
  }
  if (direction == "w"){
    x <- loc[[1]] - 1
    y <- loc[[2]]
    return(list(x,y))
  }
  if (direction == "nw"){
    x <- loc[[1]] - 0.5
    y <- loc[[2]] + 0.866
    return(list(x,y))
  }
  if (direction == "ne"){
    x <- loc[[1]] + 0.5
    y <- loc[[2]] + 0.866
    return(list(x,y))
  }
}

# parse input
input$V2 <- gsub("", " ", input$V1)
input$V2 <- gsub("n w", "nw", input$V2)
input$V2 <- gsub("n e", "ne", input$V2)
input$V2 <- gsub("s e", "se", input$V2)
input$V2 <- gsub("s w", "sw", input$V2)

input$X <- NA
input$Y <- NA

for (i in 1:nrow(input)){
  directions <- input[i,2]
  directions <- strsplit(directions, split = " ")[[1]]
  directions <- directions[directions != ""]
  
  location <- list(0,0)
  for (dir in directions){
    location <- move_dir(location, dir)
  }
  
  input[i,3] <- location[[1]]
  input[i,4] <- location[[2]]
}

input$Y <- round(input$Y)
table(table(input$X, input$Y))
512 + 23



### PART 2
# first, create a table identifying black and white tiles
# e.g. if table(table(...)) == 2 (or 0), it's white, if 1 it's black

#with janky coordinate system, valid tiles are diamonds then hexes then diamonds
# let's re-do part 1 using a real coordinate system of 3 vectors
# which then reduces down to horizontal + either diagonal

# E: move +1/0/0
# SE: move 0/0/1
# SW: move 0/-1/0
# W: move -1/0/0
# NW: move 0/0/-1
# NE: move 0/+1/0

# x is move east/west
# y is move NE/SW
# z is move NW/SE

move_dir <- function(loc, direction){ #loc is list(x,y,z)
  if (direction == "e"){
    x <- loc[[1]] + 1
    y <- loc[[2]]
    z <- loc[[3]]
    return(list(x,y,z))
  }
  if (direction == "se"){
    x <- loc[[1]] 
    y <- loc[[2]]
    z <- loc[[3]] + 1
    return(list(x,y,z))
  }
  if (direction == "sw"){
    x <- loc[[1]]
    y <- loc[[2]] - 1
    z <- loc[[3]]
    return(list(x,y,z))
  }
  if (direction == "w"){
    x <- loc[[1]] - 1
    y <- loc[[2]]
    z <- loc[[3]]
    return(list(x,y,z))
  }
  if (direction == "nw"){
    x <- loc[[1]] 
    y <- loc[[2]]
    z <- loc[[3]] - 1
    return(list(x,y,z))
  }
  if (direction == "ne"){
    x <- loc[[1]] 
    y <- loc[[2]] + 1
    z <- loc[[3]]
    return(list(x,y,z))
  }
}

# E: move +1/0/0     # SE: move 0/0/1
# SW: move 0/-1/0    # W: move -1/0/0
# NW: move 0/0/-1    # NE: move 0/+1/0
# NE + SE becomes pure E
# NW + SW becomes pure W

# function to reduce combinations of diagonals
# want to convert to a system of [x,y,z], where every hex
# can be defined by either [x,0,z] or [x,y,0], where x > 0 and y > 0
red_diag <- function(loc){
  x <- loc[[1]]
  y <- loc[[2]]
  z <- loc[[3]]
  
  if (y > 0 & z > 0){
    while (y > 0 & z > 0){
      x <- x + 1
      y <- y - 1
      z <- z - 1
    }
    return(list(x,y,z))
  } else if (y < 0 & z < 0){
    while (y < 0 | z < 0){ #want both to be positive
      x <- x - 1
      y <- y + 1
      z <- z + 1
    }
    return(list(x,y,z))
  } else if (y > 0 & z < 0){
    while (y > 0 & z < 0){
      x <- x - 1
      y <- y + 1
      z <- z + 1
    }
    return(list(x,y,z))
  } else if (y < 0 & z > 0){
    while (y < 0 & z > 0){
      x <- x - 1
      y <- y + 1
      z <- z + 1
    }
    return(list(x,y,z))
  } else if (y == 0 & z < 0){
    while (z < 0){
      x <- x - 1
      y <- y + 1
      z <- z + 1
    }
    return(list(x,y,z))
  } else if (y < 0 & z == 0){
    while (y < 0){
      x <- x - 1
      y <- y + 1
      z <- z + 1
    }
    return(list(x,y,z))
  } else {
    return(list(x,y,z))
    # print(list(x,y,z))
    # print("fuck")
  }
}


input <- read.delim("input/day24.txt", header = FALSE,
                    blank.lines.skip = FALSE)

# parse input
input$V2 <- gsub("", " ", input$V1)
input$V2 <- gsub("n w", "nw", input$V2)
input$V2 <- gsub("n e", "ne", input$V2)
input$V2 <- gsub("s e", "se", input$V2)
input$V2 <- gsub("s w", "sw", input$V2)

input$X <- NA
input$Y <- NA
input$Z <- NA

for (i in 1:nrow(input)){
  directions <- input[i,2]
  directions <- strsplit(directions, split = " ")[[1]]
  directions <- directions[directions != ""]
  
  location <- list(0,0,0)
  for (dir in directions){
    location <- move_dir(location, dir)
  }
  location <- red_diag(location)
  input[i,3] <- location[[1]]
  input[i,4] <- location[[2]]
  input[i,5] <- location[[3]]
}


#library(collections)
existing_coordinates <- c()
for (i in 1:nrow(input)){
  location <- paste(input[i,3], input[i,4], input[i,5])
  if (location %in% existing_coordinates){
    existing_coordinates <- existing_coordinates[existing_coordinates != location]
  } else {
    existing_coordinates <- c(existing_coordinates, location) # in set is black
  }
}

# initialize all other possible hexes around, theoretically can propagate

get_value <- function(x,y,z,active){
  location <- list(x,y,z)
  location <- red_diag(location)
  location <- paste(location[[1]],location[[2]],location[[3]])
  if (location %in% active){
    return(1)
  } else {
    return(0)
  }
}

#(1,0,1) neighbors hsould check (0,0,2) and (2,0,0)
check_neighbors <- function(x,y,z,active){
  # neighbors are: 0/1/0, 0/0/1, 1/0/0, -1/0/0, -1/1/0, -1/0/1
  return(get_value(x,y+1,z,active) + get_value(x,y,z+1,active) + 
           get_value(x+1,y,z,active) + get_value(x-1,y,z,active) + 
           get_value(x-1,y+1,z,active) + get_value(x-1,y,z+1,active))
}

iterate_all <- function(currently_active){
  # check for current maximums in x,y,z
  coords <- strsplit(currently_active, split = " ")
  x <- 0
  y <- 0
  z <- 0
  for (idx in 1:length(coords)){
    x <- c(x, as.numeric(coords[[idx]][1]))
    y <- c(y, as.numeric(coords[[idx]][2]))
    z <- c(z, as.numeric(coords[[idx]][3]))
  }
  x_min <- min(x) - 1
  x_max <- max(x) + 1
  y_max <- max(y) + 1
  z_max <- max(z) + 1
  
  output_active <- currently_active
  for (i in x_min:x_max){
    for (j in 0:y_max){
      k <- 0
      location <- paste(i,j,k)
      
      if (location %in% currently_active){
        if (check_neighbors(i,j,k,currently_active) == 0){
          output_active <- output_active[output_active != location]
        } else if (check_neighbors(i,j,k,currently_active) > 2){
          output_active <- output_active[output_active != location]
        } else {#do nothing
        }
      } else {
        if (check_neighbors(i,j,k,currently_active) == 2){ #if off, and 2 black neighbors
          output_active <- c(output_active, location) 
        }
      }
    }
    
    for (k in 1:z_max){
      j <- 0
      location <- paste(i,j,k)
      
      if (location %in% currently_active){
        if (check_neighbors(i,j,k,currently_active) == 0){
          output_active <- output_active[output_active != location]
        } else if (check_neighbors(i,j,k,currently_active) > 2){
          output_active <- output_active[output_active != location]
        } else {#do nothing
        }
      } else {
        if (check_neighbors(i,j,k,currently_active) == 2){ #if off, and 2 black neighbors
          output_active <- c(output_active, location) 
        }
      }
    }
  }
  
  return(output_active)
}

days_left <- 100

while(days_left > 0){
  print(days_left)
  existing_coordinates <- iterate_all(existing_coordinates)
  
  days_left <- days_left - 1
}

print(length(existing_coordinates))