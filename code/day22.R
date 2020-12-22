### ADVENT OF CODE, DAY 22
# PART 1

input <- read.delim("input/day22.txt", header = FALSE,
                    blank.lines.skip = TRUE, stringsAsFactors = FALSE)

library(collections)
library(sets)

p1 <- as.numeric(input$V1[2:26])
p2 <- as.numeric(input$V1[28:52])

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

print(sum)


### PART 2

recursive_combat <- function(decks,sub,existing_solutions){ # sub for printing subgame depth
  p1 <- decks[[1]]
  p2 <- decks[[2]]
  
  p1_init <- set() # sets of prior arrangements
  p2_init <- set()
  
  while ((length(p1) > 0) & (length(p2) > 0)){
    if ((paste(p1, collapse = ",") %in% p1_init) & (paste(p2, collapse = ",") %in% p2_init)){
      p1 <- c(0)
      p2 <- c()
      decks <- list(p1,p2)
      return(decks)
    }

    p1_init <- c(p1_init, paste(p1, collapse = ","))
    p2_init <- c(p2_init, paste(p2, collapse = ","))
    
    if (p1[1] > (length(p1)-1) | p2[1] > (length(p2)-1)){ # if no sub-game, high card wins
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
    } else { # initiate sub-game if a sub-game winner isn't already memoized
      #print(paste("Going into game", i+1))
      p1_sub <- p1[2:(1+p1[1])]
      p2_sub <- p2[2:(1+p2[1])]
      decks_sub <- list(p1_sub, p2_sub)
      
      # memoizing by storing winners in a dict based on hands
      hands_string <- paste(paste(p1, collapse = ","), paste(p2, collapse = ","), collapse = "")
      
      if (hands_string %in% existing_solutions$keys()){
        winner <- existing_solutions$get(hands_string)
        if (winner == 1){
          p1_sub <- c(0)
          p2_sub <- c()
        } else if (winner == 2){
          p1_sub <- c()
          p2_sub <- c(0)
        } else {
          print("something's wrong")
        }
      } else {
        decks_sub <- recursive_combat(decks_sub, i + 1, existing_solutions)
        p1_sub <- decks_sub[[1]]
        p2_sub <- decks_sub[[2]]
      }

      if (length(p1_sub) == 0){ #sub game winner is whoever's deck has 1+ cards
        existing_solutions$set(hands_string, 2)
        winner <- p2[1]
        loser <- p1[1]
        p1 <- p1[-1]
        p2 <- c(p2[-1], winner, loser)
      } else if (length(p2_sub) == 0){
        existing_solutions$set(hands_string, 1)
        winner <- p1[1]
        loser <- p2[1]
        p1 <- c(p1[-1], winner, loser)
        p2 <- p2[-1]
      } else {
        print("something else is wrong")
      }
    }
  }
  
  decks <- list(p1, p2)
  return(decks)
}

p1 <- as.numeric(input$V1[2:26])
p2 <- as.numeric(input$V1[28:52])
decks <- list(p1,p2)
existing_solutions <- dict()

outcome <- recursive_combat(decks, 1, existing_solutions)

outcome1 <- outcome[[1]]

sum <- 0
for (i in 1:length(outcome1)){
  sum <- sum + (length(outcome1)+1-i)*outcome1[i]
}

print(sum)