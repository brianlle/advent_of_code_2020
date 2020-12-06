### ADVENT OF CODE, DAY 6
# 

input <- read.delim("input/day6.txt", header = FALSE,
                    blank.lines.skip = FALSE)

list <- c()
temp <- ""

for (i in 1:nrow(input)){
  print(i)
  if (input[i,1] != ""){
    temp <- paste0(temp, input[i,1], "")
    if (i == nrow(input)){
      list <- c(list, temp)
    }
  } else {
    temp <- substr(temp, 1, nchar(temp))
    list <- c(list, temp)
    temp <- ""
  }
}

#run one last time to include last group that wasn't followed by blank line
list <- c(list, temp) 


identify_yes <- function(pattern){
  pattern <- strsplit(pattern, "")[[1]]
  unique_yes <- names(table(pattern))
  return(length(unique_yes))
}

sum <- 0
for (i in 1:455){
  sum <- sum + identify_yes(list[i])
}

### part 2, everyone answered yes to questions

list <- c()
num_person <- c()
temp <- ""
num <- 0

for (i in 1:nrow(input)){
  print(i)
  if (input[i,1] != ""){
    temp <- paste0(temp, input[i,1], "")
    num <- num + 1
    if (i == nrow(input)){
      list <- c(list, temp)
      num_person <- c(num_person, num)
    }
  } else {
    temp <- substr(temp, 1, nchar(temp))
    list <- c(list, temp)
    num_person <- c(num_person, num)
    num <- 0
    temp <- ""
  }
}

#run one last time to include last group that wasn't followed by blank line

identify_yes <- function(pattern, num_person){
  pattern <- strsplit(pattern, "")[[1]]
  unique_yes <- names(table(pattern)[table(pattern) == num_person])
  return(length(unique_yes))
}

sum <- 0
for (i in 1:455){
  sum <- sum + identify_yes(list[i], num_person[i])
}
