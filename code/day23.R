### ADVENT OF CODE, DAY 23
# PART 1

input <- "952316487"
input <- strsplit(input, split = "")[[1]]

plays_left <- 10
current_cup <- 9

while (plays_left > 0){
  
  if (current_cup == 9){
    current_cup <- 1
  } else {
    current_cup <- current_cup + 1
  }
  
  cup_value <- input[current_cup]
  
  next1 <- (current_cup %% 9) + 1
  next2 <- ((current_cup+1) %% 9) + 1
  next3 <- ((current_cup+2) %% 9) + 1
  
  removed_cups <- c(input[next1], input[next2], input[next3])
  
  destination_cup <- as.numeric(cup_value)-1
  
  if (destination_cup == 0){
    destination_cup <- 9
  }
  
  destination_cup <- as.character(destination_cup)
  
  while (destination_cup %in% removed_cups){
    if (destination_cup == "1"){
      destination_cup <- "9"
    } else {
      destination_cup <- as.character(as.numeric(destination_cup) - 1)
    }
  }
  
  input <- input[!input %in% removed_cups]
  destination_cup_loc <- which(input == destination_cup)
  if (destination_cup_loc < 6){
    input <- c(input[1:destination_cup_loc], removed_cups,
             input[(destination_cup_loc+1):(9-3)])  
    } else {
    input <- c(input, removed_cups)
  }
  
  move_value <- which(input == cup_value)
  move_array <- c(1:9)
  
  diff <- move_value - current_cup
  if (diff > 0){
    move_array <- move_array[c((diff+1):9,1:diff)]
  } else if (diff < 0){
    diff <- abs(diff)
    move_array <- move_array[c((9-diff+1):9,1:(9-diff))]
  }
  input <- input[move_array]
  
  print(input)
  plays_left <- plays_left - 1
}

#rotate so that 1 is the first cup
while (input[1] != "1"){
  input <- input[c(2:9,1)]
}

print(paste(input[2:9], collapse = ""))


#### PART 2
# generalizing original part 1 approach would take way too long (in terms of
# runtime), so need to implement cups smarter
# Strategy: implement a dict() where for each cup value (as the key),
# we store the previous cup and the next cup values in order to create
# a linked chain

input <- "952316487"
input <- as.numeric(strsplit(input, split = "")[[1]])
input <- c(input, 10:1000000) # 1 million cups
input_length <- length(input)

#ten million plays
plays_left <- 10000000
current_cup <- 0

# key: cup value (under cup, not position)
# value: list(previous_cup_value, next_cup_value)

cups <- dict()
for (i in 1:input_length){
  if (i == 1){
    cups$set(input[i], list(input[input_length], input[i+1]))
  } else if (i == input_length){
    cups$set(input[i], list(input[i-1], input[1]))
  } else {
    cups$set(input[i], list(input[i-1], input[i+1]))
  }
}

current_cup <- input[1]

print(Sys.time())
# repeat 10,000,000 times
while (plays_left > 0){
  #print(plays_left)
  
  destination_cup <- as.numeric(current_cup)-1
  
  if (destination_cup == 0){ # loop around to end
    destination_cup <- as.numeric(input_length)
  }
  
  # identify next 3 cups to be moved; if the destination cup is any of these,
  # update per rules by subtracting 1 as many times as needed
  next1 <- cups$get(current_cup)[[2]]
  next2 <- cups$get(next1)[[2]]
  next3 <- cups$get(next2)[[2]]
  
  while (destination_cup %in% c(next1, next2, next3)){
    if (destination_cup == 1){
      destination_cup <- as.numeric(input_length)
    } else {
      destination_cup <- destination_cup - 1
    }
  }
  
  #update original cup and new cup after
  #effectively removing next1,next2,next3 from the chain
  new_after_orig <- cups$get(next3)[[2]]
  cups$set(current_cup, list(cups$get(current_cup)[[1]],
                             new_after_orig))
  cups$set(new_after_orig, list(current_cup,
                                cups$get(new_after_orig)[[2]]))
  
  # next1,next2,next3 goes between destination_cup and destination_cup2
  # update destination cup
  destination_cup2 <- cups$get(destination_cup)[[2]]
  
  #check for case if destination_cup comes after next3
  if (cups$get(destination_cup)[[1]] == next3){
    cups$set(destination_cup, list(cups$get(next1)[[1]],
                                   next1))
  } else { #otherwise, keep current previous cup
    cups$set(destination_cup, list(cups$get(destination_cup)[[1]],
                                   next1))
  }
  
  #similarly, check if destination_cup2 comes before next1
  if (cups$get(destination_cup2)[[2]] == next1){
    cups$set(destination_cup2, list(next3,
                                   cups$get(next3)[[2]]))
  } else { #otherwise, keep current previous cup
    cups$set(destination_cup2, list(next3,
                                   cups$get(destination_cup2)[[2]]))
  }
  
  #finally, update next1, next2, next3 cups
  cups$set(next1, list(destination_cup, next2))
  #next2 untouched, same linkings in chain
  cups$set(next3, list(next2, destination_cup2))
  
  ## next cup to start from is 1 after current up
  current_cup <- cups$get(current_cup)[[2]]
  plays_left <- plays_left - 1
}
print(Sys.time())
print(cups$get(1)[[2]] * cups$get(cups$get(1)[[2]])[[2]])

# 363807398885


#### BONUS: part 2 optimization
# keeping track of the *previous* cup value is unnessary, can
# cut down operations by almost half by only keeping track of
# the next cup value


input <- "952316487"
input <- as.numeric(strsplit(input, split = "")[[1]])
input <- c(input, 10:1000000) # 1 million cups
input_length <- length(input)

#ten million plays
plays_left <- 10000000
current_cup <- 0

# key: cup value (under cup, not position)
# value: next_cup_value

cups <- dict()
for (i in 1:input_length){
  if (i == input_length){
    cups$set(input[i], input[1])
  } else {
    cups$set(input[i], input[i+1])
  }
}

current_cup <- input[1]

print(Sys.time())
# repeat 10,000,000 times
while (plays_left > 0){
  #print(plays_left)
  
  destination_cup <- as.numeric(current_cup)-1
  
  if (destination_cup == 0){ # loop around to end
    destination_cup <- as.numeric(input_length)
  }
  
  # identify next 3 cups to be moved; if the destination cup is any of these,
  # update per rules by subtracting 1 as many times as needed
  next1 <- cups$get(current_cup)
  next2 <- cups$get(next1)
  next3 <- cups$get(next2)
  
  while (destination_cup %in% c(next1, next2, next3)){
    if (destination_cup == 1){
      destination_cup <- as.numeric(input_length)
    } else {
      destination_cup <- destination_cup - 1
    }
  }
  
  #update original cup and new cup after
  #effectively removing next1,next2,next3 from the chain
  cups$set(current_cup, cups$get(next3))
  
  # next1,next2,next3 goes between destination_cup and destination_cup2
  # update destination cup
  destination_cup2 <- cups$get(destination_cup)
  
  cups$set(destination_cup, next1)
  
  #check if destination_cup2 comes before next1
  if (cups$get(destination_cup2) == next1){
    cups$set(destination_cup2, cups$get(next3))
  }
  
  #finally, update next1, next2, next3 cups
  #next1 untouched, same next linking
  #next2 untouched, same linkings in chain
  cups$set(next3, destination_cup2)
  
  ## next cup to start from is 1 after current up
  current_cup <- cups$get(current_cup)
  plays_left <- plays_left - 1
}
print(Sys.time())
print(cups$get(1) * cups$get(cups$get(1)))

# 363807398885


#### PART 2 even more bonus optimization
# since we're only tracking a single value, we don't even need
# to use a fake dict() that isn't native to R, we can use
# arrays to track the next value!

#### BONUS: part 2 optimization
# keeping track of the *previous* cup value is unnessary, can
# cut down operations by almost half by only keeping track of
# the next cup value


input <- "952316487"
input <- as.numeric(strsplit(input, split = "")[[1]])
input <- c(input, 10:1000000) # 1 million cups
input_length <- length(input)

#ten million plays
plays_left <- 10000000
current_cup <- 0

# create an array that effectively is:
# i is the cup value, array[i] stores the value of the *next* cup
cups <- rep(0, 1000000)
for (i in 1:9){
  loc <- which(input == i)
  cups[i] <- input[loc+1]
}
for (i in 10:(input_length-1)){
  cups[i] <- input[i+1]
}
cups[1000000] <- input[1]

current_cup <- input[1]

print(Sys.time())
# repeat 10,000,000 times
while (plays_left > 0){
  #print(plays_left)
  
  destination_cup <- as.numeric(current_cup)-1
  
  if (destination_cup == 0){ # loop around to end
    destination_cup <- as.numeric(input_length)
  }
  
  # identify next 3 cups to be moved; if the destination cup is any of these,
  # update per rules by subtracting 1 as many times as needed
  next1 <- cups[current_cup]
  next2 <- cups[cups[current_cup]]
  next3 <- cups[cups[cups[current_cup]]]
  
  while (destination_cup %in% c(next1, next2, next3)){
    if (destination_cup == 1){
      destination_cup <- as.numeric(input_length)
    } else {
      destination_cup <- destination_cup - 1
    }
  }
  
  #update original cup and new cup after
  #effectively removing next1,next2,next3 from the chain
  
  cups[current_cup] <- cups[next3]
  
  
  # next1,next2,next3 goes between destination_cup and destination_cup2
  # update destination cup
  destination_cup2 <- cups[destination_cup]
  
  cups[destination_cup] <- next1
  
  #check if destination_cup2 comes before next1
  if (cups[destination_cup2] == next1){
    cups[destination_cup2] <- cups[next3]
  }
  
  #finally, update next1, next2, next3 cups
  #next1 untouched, same next linking
  #next2 untouched, same linkings in chain
  cups[next3] <- destination_cup2
  
  ## next cup to start from is 1 after current up
  broke_cup <- current_cup
  current_cup <- cups[current_cup]
  plays_left <- plays_left - 1
}
print(Sys.time())

cups[1]*cups[cups[1]]

# 363807398885