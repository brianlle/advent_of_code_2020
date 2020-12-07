### ADVENT OF CODE, DAY 7
# 

input <- read.delim("input/day7.txt", header = FALSE,
                    blank.lines.skip = FALSE)

input$object <- gsub("s contain.*", "", input$V1)
input$contents <- gsub(".*s contain ", "", input$V1)
input <- input[order(input$object),]
row.names(input) <- NULL

has_gold_bag <- function(pattern){
  
  if (str_detect(pattern, "shiny gold bag")){
    return(TRUE)
  }
  if (pattern == "no other bags."){
    return(FALSE)
  }
  
  pattern <- gsub("[0-9] ", "", pattern)
  pattern <- gsub("bags", "bag", pattern)
  pattern <- gsub("\\.", "", pattern)
  bags_to_search <- strsplit(pattern, ", ")[[1]]
  
  while (length(bags_to_search) > 0){
    bag <- bags_to_search[1]
    
    if (str_detect(input[input$object == bag, 3], "shiny gold bag")){
      return(TRUE)
    } else {
      pattern <- input[input$object == bag, 3]
      
      if (pattern != "no other bags."){
        pattern <- gsub("[0-9] ", "", pattern)
        pattern <- gsub("bags", "bag", pattern)
        pattern <- gsub("\\.", "", pattern)
        bags_to_search <- c(bags_to_search, strsplit(pattern, ", ")[[1]])
      }
      
      bags_to_search <- bags_to_search[-1]
    }
  }
  return(FALSE)
}

bags_have <- c()
for (i in 1:nrow(input)){
  if (has_gold_bag(input[i, 3])){
    bags_have <- c(bags_have, 1)
  } else {
    bags_have <- c(bags_have, 0)
  }
}

sum(bags_have)

### PART 2, shiny gold is row 474

count_bags <- function(row){
  total_bags <- 0
  
  #pattern <- gsub("[0-9] ", "", pattern)
  pattern <- input[row, 3]
  pattern <- gsub("bags", "bag", pattern)
  pattern <- gsub("\\.", "", pattern)
  bags_to_search <- strsplit(pattern, ", ")[[1]]
  
  while (length(bags_to_search) > 0){
    bag <- gsub("[0-9][0-9]* ", "", bags_to_search[1])
    num <- as.numeric(gsub(" .*", "", bags_to_search[1]))
    total_bags <- total_bags + num
    
    pattern <- input[input$object == bag, 3]
      
    if (pattern != "no other bags."){
      #pattern <- gsub("[0-9] ", "", pattern)
      pattern <- gsub("bags", "bag", pattern)
      pattern <- gsub("\\.", "", pattern)
      pattern <- strsplit(pattern, ", ")[[1]]
      for (i in 1:length(pattern)){
        temp_num <- as.numeric(gsub(" .*", "", pattern[i]))
        pattern[i] <- gsub(as.character(temp_num), as.character(num*temp_num), pattern[i])
      }
      bags_to_search <- c(bags_to_search, pattern)
    }
    
    bags_to_search <- bags_to_search[-1]
    }
    
  return(total_bags)
}

count_bags(474)
