### ADVENT OF CODE, DAY 10

input <- read.csv("input/day10.txt", header = FALSE)

input <- sort(input$V1)

input2 <- input[2:102] - input[1:101] #joltage jumps throughout sequence
input2 <- c(input[1], input2, 3) #initial jump from 0 to input[1], and final jump to our device

table(input2)
print(table(input2)['1'] * table(input2)['3'])


#PART 2
#pattern can be broken up by delta-3s, e.g. a delta-3 is a forced jump
#and for sets of delta-1s, each set will have a number of perms
#then we can multiply all those possibilities together for total permutations
#what if jumps are 3,1,3? 3,1,1,3? 3,1,1,1,3? 3,1,1,1,1,3?
#possible valid sets of deltas:
# length of 3,1,1,1,1,1 -> 2+4choose2+4choose1+1 = 13 possibilities
# length of 3,1,1,1,1 -> 7 possibilities
# length of 3,1,1,1 -> 4 possibilities
# length of 3,1,1 -> 2 possibilties
# length of 1,1 -> 1 possibility
# poss(n) = poss(n-1) + poss(n-2) + poss(n-3)

# initial fast, hacky solution, doesn't account for possibility of larger strings of 1s,
# e.g. 3,1,1,1,1,1,1,1,1,1,1,3
input3 <- paste(input2, collapse = "")
input4 <- gsub("3", " ", input3)
input5 <- strsplit(input4, split = " ")
input6 <- table(input5) #initial 1111 chain counts as 31111 for combinatoric purposes
num_7s <- 7^(input6['1111'])
num_4s <- 4^(input6['111'])
num_2s <- 2^(input6['11'])
print(num_7s*num_4s*num_2s, digits = 20)


# more future proofed solution, accounting for all possible lengths of delta-1s:
input7 <- data.frame(input6)
input7$length <- nchar(as.character(input7$input5))
test <- c(1,2,3,4,5,6,7)
input7$poss <- test[input7$length]

#construct vector of possibilities
possibilities <- c(1,1,2,4,7) #initial values pre-calculated
for (i in 1:max(input7$length)){
  possibilities[i+5] <- possibilities[i+4] + possibilities[i+3] + possibilities[i+2]
}
possibilities <- data.frame(length = 0:(length(possibilities)-1), perms = possibilities)
input8 <- merge(input7, possibilities,
                by = "length",
                all.x = TRUE, all.y = FALSE)
total <- prod(input8$perms^input8$Freq)
print(total, digits = 20)
