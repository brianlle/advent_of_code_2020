### ADVENT OF CODE, DAY 18
# PART 1

input <- read.delim("input/day18.txt", header = FALSE,
                    blank.lines.skip = FALSE)
input <- read.csv("input/day18.txt", header = FALSE)
input$V1 <- gsub(" ", "", input$V1)
input$value <- NA

library(stringr)

### eval(parse(text = "2+3")) will evaluate to 5
### evaluating from left to right is basically saying add hidden parentheses to everything

# assuming no parantheses, this function evaluates an expression using the new math rules
# rules: evaluating occurs left-to-right, e.g. a+b*c is equivalent to (a+b)*c
do_new_math <- function(pattern){
  
  pattern <- gsub(" ", "", pattern)
  oper_pos <- str_locate(pattern, "[+*]")[1]
  value <- substring(pattern, 1, oper_pos-1)
  pattern <- substring(pattern, oper_pos, nchar(pattern))
  
  while (nchar(pattern) > 1){
    if(str_detect(substring(pattern, 2, nchar(pattern)), "[*+]")){
      oper_pos_next <- str_locate(substring(pattern, 2, nchar(pattern)), "[+*]")[1] + 1
      value <- eval(parse(text = paste0(as.character(value), substring(pattern, 1, oper_pos_next-1), collapse = "")))
      pattern <- substring(pattern, oper_pos_next, nchar(pattern))
    } else {
      value <- eval(parse(text = paste0(as.character(value), pattern, collapse = "")))
      pattern <- ""
    }
  }
  return(value)
}

# helper function to find positions of parentheses
# returns positions of matched left and right parens, but adds values such that
# the left-most values will always be the most inner nested parentheses
find_paren <- function(pattern){
  pattern <- strsplit(pattern, split = "")[[1]]
  unmatched_paren <- c()
  left_paren <- c()
  right_paren <- c()
  for (i in 1:length(pattern)){
    if (pattern[i] == "("){
      unmatched_paren <- c(unmatched_paren, i)
    } else if (pattern[i] == ")"){
      left_paren <- c(left_paren, unmatched_paren[length(unmatched_paren)])
      right_paren <- c(right_paren, i)
      unmatched_paren <- unmatched_paren[-length(unmatched_paren)] # pop out value used
    }
  }
  return(list("left" = left_paren, "right" = right_paren))
}

# parse through an exporession, first evaluating all parenthesis pairs using new math
# and then evaluating the remaining expression with no parentheses using new math
handle_paren <- function(pattern){
  number_paren <- str_count(pattern, pattern = "\\(")
  
  while(number_paren > 0){
    paren_list <- find_paren(pattern)
    left <- paren_list[["left"]][1]
    right <- paren_list[["right"]][1]
    
    paren_pattern <- do_new_math(substr(pattern, left+1, right-1))
    pattern <- paste0(substring(pattern, 1, left-1),
                      as.character(paren_pattern),
                      substring(pattern, right+1, nchar(pattern)))
    number_paren <- number_paren - 1
  }
  
  return(do_new_math(pattern)) # do math oen last time on pattern
}

for (i in 1:nrow(input)){
  input[i,2] <- handle_paren(input[i,1])
}

print(sum(input$value), digits = 20)


#### PART 2
# ... new math has different rules, addition occurs before multiplication instead of left-to-right order
# so the only thing we need to do is update the do_new_math() rules, since parentheses behave the same

# assuming no parantheses, this function evaluates an expression using the new math rules
# rules: + are all evaluated before *
do_new_math <- function(pattern){
  
  pattern <- gsub(" ", "", pattern)
  
  # do additions
  while(str_detect(pattern, "[+]")){
    oper_pos_all <- str_locate_all(pattern, "[+*]")[[1]][,1] #find position of all operators
    oper_pos <- str_locate(pattern, "[+]")[[1]] #find position of first +
    
    if (sum(oper_pos_all < oper_pos) == 0){ # if this is 0, + is 1st operator
      left_bound <- 1
    } else {
      left_bound <- oper_pos_all[match(oper_pos, oper_pos_all) - 1] + 1
    }
    
    if (sum(oper_pos_all > oper_pos) == 0){ # if this is 0, + is last operator
      right_bound <- nchar(pattern)
    } else {
      right_bound <- oper_pos_all[match(oper_pos, oper_pos_all) + 1] - 1
    }
    
    temp_value <- eval(parse(text = substring(pattern, left_bound, right_bound)))
    pattern <- paste0(substring(pattern, 0, left_bound - 1),
                      as.character(temp_value),
                      substring(pattern, right_bound + 1, nchar(pattern)))
    
  }
  
  # no more +, so just parse regularly
  return(eval(parse(text = pattern)))
}

for (i in 1:nrow(input)){
  input[i,2] <- handle_paren(input[i,1])
}

print(sum(input$value), digits = 20)
