### ADVENT OF CODE, DAY 12
# PART 1

input <- read.csv("input/day12.txt", header = FALSE, sep = "")
input$move <- gsub("[0-9]", "", input$V1)
input$value <- gsub("[a-zA-Z]", "", input$V1)

x <- 0
y <- 0
face <- pi/2

move <- function(list, direction, value){
  x <- list[1]
  y <- list[2]
  face <- list[3]
  if (direction == "N"){
    y <- y + value
  } else if (direction == "S"){
    y <- y - value
  } else if (direction == "W"){
    x <- x - value
  } else if (direction == "E"){
    x <- x + value
  } else if (direction == "L"){
    face <- face + value*pi/180
  } else if (direction == "R"){
    face <- face - value*pi/180
  } else if (direction == "F"){ #rotation matrix
    x <- x + value*round(cos(face))
    y <- y + value*round(sin(face))
  }
  return(c(x,y,face))
}

pos <- c(0,0,0)
for (i in 1:nrow(input)){
  pos <- move(pos, input[i,2], as.numeric(input[i,3]))
  print(pos)
}

print(abs(pos[1]) + abs(pos[2]))

# PART 2

move <- function(list, direction, value){
  x <- list[1]
  y <- list[2]
  way_x <- list[3]
  way_y <- list[4]
#(1,0)
  if (direction == "N"){
    way_y <- way_y + value
  } else if (direction == "S"){
    way_y <- way_y - value
  } else if (direction == "W"){
    way_x <- way_x - value
  } else if (direction == "E"){
    way_x <- way_x + value
  } else if (direction == "L"){ #rotation matrix
    way_x2 <- way_x*round(cos(value*pi/180)) - way_y*round(sin(value*pi/180))
    way_y2 <- way_x*round(sin(value*pi/180)) + way_y*round(cos(value*pi/180))
    way_x <- way_x2
    way_y <- way_y2
  } else if (direction == "R"){
    way_x2 <- way_x*round(cos(-1*value*pi/180)) - way_y*round(sin(-1*value*pi/180))
    way_y2 <- way_x*round(sin(-1*value*pi/180)) + way_y*round(cos(-1*value*pi/180))
    way_x <- way_x2
    way_y <- way_y2
  } else if (direction == "F"){
    x <- x + value*way_x
    y <- y + value*way_y
  }
  return(c(x,y,way_x,way_y))
}

pos
move(pos, input[4,2], as.numeric(input[4,3]))

pos <- c(0,0,10,1)
for (i in 1:nrow(input)){
  pos <- move(pos, input[i,2], as.numeric(input[i,3]))
  print(pos)
}

print(abs(pos[1]) + abs(pos[2]))
