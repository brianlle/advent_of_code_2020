### ADVENT OF CODE, DAY 22
# PART 1

input <- read.delim("input/day22.txt", header = FALSE,
                    blank.lines.skip = TRUE)

library(collections)


p1 <- as.numeric(as.character(input$V1[2:26]))
p2 <- as.numeric(as.character(input$V1[28:52]))

while ((length(p1) > 0) & (length(p2) > 0)){
  
  if (p1[1] > p2[1]){
    high_card <- max(p1[1], p2[1])
    low_card <- min(p1[1], p2[1])
    
    p1 <- c(p1[-1], high_card, low_card)
    p2 <- p2[-1]
  } else if (p2[1] > p1[1]){
    high_card <- max(p1[1], p2[1])
    low_card <- min(p1[1], p2[1])
    
    p1 <- p1[-1]
    p2 <- c(p2[-1], high_card, low_card)
  }
  
}

sum <- 0
for (i in 1:length(p1)){
  sum <- sum + (length(p1)+1-i)*p1[i]
}

sum

test <- c(3,2,10,6,8,5,9,4,7,1)


### PART 2

p1 <- as.numeric(as.character(input$V1[2:26]))
p2 <- as.numeric(as.character(input$V1[28:52]))

decks <- list(p1,p2)

recursive_combat <- function(decks,i,j){
  p1 <- decks[[1]]
  p2 <- decks[[2]]
  not_first_time <- FALSE
  p1_init <- decks[[1]]
  p2_init <- decks[[2]]
  
  while ((length(p1) > 0) & (length(p2) > 0)){
    print(paste("Game", i, "Round", j))
    
    if(not_first_time){
      # print("not first time")
      # print(paste("p1:", p1))
      # print(paste("p1i:", p1_init))
      # print(paste("p2:", p2))
      # print(paste("p2i:", p2_init))
      if (identical(p1, p1_init) & identical(p2, p2_init)){
        p1 <- c(0)
        p2 <- c()
        next
      }
    } else {
      not_first_time <- TRUE
    }

    if (p1[1] > (length(p1)-1) | p2[1]+1 > (length(p2)-1)){
      if (p1[1] > p2[1]){
        high_card <- max(p1[1], p2[1])
        low_card <- min(p1[1], p2[1])
        p1 <- c(p1[-1], high_card, low_card)
        p2 <- p2[-1]
        #decks <- list(p1, p2)
        #decks <- recursive_combat(decks)
      } else if (p2[1] > p1[1]){
        high_card <- max(p1[1], p2[1])
        low_card <- min(p1[1], p2[1])
        p1 <- p1[-1]
        p2 <- c(p2[-1], high_card, low_card)
        #decks <- list(p1, p2)
        #decks <- recursive_combat(decks)
      }
    } else {
      p1_sub <- p1[2:(1+p1[1])]
      p2_sub <- p2[2:(1+p2[1])]
      decks_sub <- list(p1_sub, p2_sub)
      decks_sub <- recursive_combat(decks_sub, i + 1, 1)
      
      p1_sub <- decks_sub[[1]]
      p2_sub <- decks_sub[[2]]
      
      if (length(p1_sub) == 0){
        winner <- p2[1]
        loser <- p1[1]
        p1 <- p1[-1]
        p2 <- c(p2[-1], winner, loser)
      } else if (length(p2_sub) == 0){
        winner <- p1[1]
        loser <- p2[1]
        p1 <- c(p1[-1], winner, loser)
        p2 <- p2[-1]
      } else {
        print("fuck")
      }
    }
    j <- j + 1
  }
  
  decks <- list(p1, p2)
  return(decks)
}

### test cases
t1 <- c(12,14,16)
t2 <- c(10,8,9)
p1 <- c(24,1,16,7,40,19,47,31,46,30,50,2,28,21,42,36,34,15,48,10,45,6,22,9)
p2 <- c(5,27,12,23,14)

recursive_combat(list(t1,t2))

recursive_combat(decks)

test1 <- c(9, 2, 6, 3, 1)
test2 <- c(5, 8, 4, 7, 10)
recursive_combat(list(test1, test2))

test3 <- c(43,19)
test4 <- c(2,29,14)
recursive_combat(list(test3, test4), 1, 1)

outcome <- recursive_combat(list(p1, p2), 1, 1)
