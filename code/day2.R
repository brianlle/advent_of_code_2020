### ADVENT OF CODE, DAY 2
# Task: given a list of inputs, identify which lines are "valid"
# e.g.
# For example, suppose you have the following list:
#   
# 1-3 a: abcde
# 1-3 b: cdefg
# 2-9 c: ccccccccc
#
# Lines 6 and 8 are valid, line 7 is not.

input <- read.table("input/day2.txt")

# Step 1: create new columns
input$min <- as.numeric(gsub("-.*", "", input$V1))
input$max <- as.numeric(gsub(".*-", "", input$V1))
input$char <- gsub(":", "", input$V2)
colnames(input)[3] <- "pattern"

# Step 2: create function to identify if a password is valid

is_valid <- function(pattern, min, max, char){
  num_matches = lengths(regmatches(pattern, gregexpr(char, pattern)))
  if ((num_matches >= min) & (num_matches <= max)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Step 3: apply function over dataframe.
library(dplyr)
input <- input %>% rowwise() %>% mutate(valid = is_valid(pattern, min, max, char))

# Step 4: sum valid column
sum(input$valid)

### DAY 2, PART 2
# Modified rules: numbers designate position, e.g.
# 1-3 a: abcde is valid: position 1 contains a and position 3 does not.
# 1-3 b: cdefg is invalid: neither position 1 nor position 3 contains b.
# 2-9 c: ccccccccc is invalid: both position 2 and position 9 contain c.

# update is_valid() function, checking for 2 valid cases
is_valid2 <- function(pattern, pos1, pos2, char){
  if (substr(pattern, pos1, pos1) == char & substr(pattern, pos2, pos2) != char){
    return(TRUE)
  } else if (substr(pattern, pos1, pos1) != char & substr(pattern, pos2, pos2) == char){
    return(TRUE)
  } else {
    return(FALSE)
    }
}
library(purrr)
input <- input %>% rowwise() %>% mutate(valid = is_valid2(pattern, min, max, char))
sum(input$valid)
