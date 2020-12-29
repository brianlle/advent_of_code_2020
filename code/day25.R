### ADVENT OF CODE, DAY 25
# PART 1

input <- read.delim("input/day25.txt", header = FALSE,
                    blank.lines.skip = FALSE)

card_value <- as.numeric(input[1,1])
door_value <- as.numeric(input[2,1])

value <- 1
subj_num <- 7
loops_1 <- 0

while(value != card_value){
  value <- value*subj_num
  value <- value %% 20201227
  loops_1 <- loops_1 + 1
  #print(value)
}

value <- 1
subj_num <- 7
loops_2 <- 0

while(value != door_value){
  value <- value*subj_num
  value <- value %% 20201227
  loops_2 <- loops_2 + 1
  #print(value)
}

value_1 <- 1
subj_num <- door_value
for (i in 1:loops_1){
  value_1 <- value_1*subj_num
  value_1 <- value_1 %% 20201227
}

value_2 <- 1
subj_num <- card_value
for (i in 1:(loops_2)){
  value_2 <- value_2*subj_num
  value_2 <- value_2 %% 20201227
}

if (value_1 == value_2){
  print(value_1)
}
  
  
### PART 2
# completed all other puzzles, hit submit!