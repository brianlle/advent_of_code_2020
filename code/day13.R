### ADVENT OF CODE, DAY 13
# PART 1

input <- read.csv("input/day13.txt", header = FALSE, sep = "")

current_time <- as.numeric(as.character(input[1,1]))
times <- strsplit(gsub("x,", "", input[2,1]), split = ",")
times <- as.numeric(times[[1]])

times_2 <- times
for (i in 1:length(times_2)){
  times_2[i] <- times_2[i] - (current_time %% times_2[i])
}



# PART 2
# initial, incredibly janky solution:

times <- strsplit(as.character(input[2,1]), split = ",")[[1]]

for (i in 1:length(times)){
  if (times[i] != "x"){
    print(paste(times[i],-(i-1)))
  }
}

# plugged into wolfram alpha system of equations: 23*a = 41*b - 13 = 37*c - 17 = ...
# general formula: bus_id * dummy variable - offset from position 1
# take integer solution value with k = 0 for smallest solution

# updated solution using CRT (Chinese remainder theorem)
library(numbers)

times_keep <- c()
position <- c()
for (i in 1:length(times)){
  if (times[i] != "x"){
    times_keep <- c(times_keep, times[i])
    position <- c(position, -(i-1)) # starts at 0, positions are time *until*, so negative
  }
}

min_time <- chinese(as.integer(position), as.integer(times_keep))
print(min_time, digits = 20)

