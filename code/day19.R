### ADVENT OF CODE, DAY 19
# PART 1

input <- read.delim("input/day19.txt", header = FALSE,
                    blank.lines.skip = FALSE)
input$V2 <- NA

library(collections)
library(stringr)
library(sets)

rules <- input[1:132,]
patterns <- input[134:nrow(input),1]

rules$rule_num <- gsub(":.*", "", rules$V1)
rules$rule <- gsub(".*: ", "", rules$V1)
rules <- rules[,c("rule_num", "rule")]

rules_dict <- dict(items = rules$rule, keys = rules$rule_num)
for (key in rules_dict$keys()){
  rules_dict$set(key, strsplit(rules_dict$get(key), split = " \\| ")[[1]])
}

rules_converted <- set()

# initial parse
for (key in rules_dict$keys()){
  if (prod(!str_detect(rules_dict$get(key), "[0-9]"))){
    rules_converted <- c(rules_converted, key)
  }
}

while (sum(str_detect(rules_dict$get("0"), "[0-9]")) > 0){ # check if any rule 0 poss. still references other rules
  for (key in rules_dict$keys()){
    if (!key %in% rules_converted){ #check if already in dict, if not in dict check rule for numbers
      
      # need to check if all referenced rules are in rules_converted
      
      rules_check <- unlist(strsplit(rules_dict$get(key), split = " "))
      
      if (prod(rules_check %in% rules_converted)){ #if all rules are in rules_converted, proceed
        possibilities <- rules_dict$get(key)
        poss_out <- c()
        
        for (possibility in possibilities){
          rules_to_add <- strsplit(possibility, split = " ")[[1]]
          
          temp_dict <- dict()
          
          for (i in 1:length(rules_to_add)){ #create separate poss for each number
            temp_dict$set(as.numeric(i), rules_dict$get(rules_to_add[i]))
          }
          
          curr <- 1
          poss_temp <- c("")
          
          while (curr <= length(rules_to_add)){ #combine possibilities together
            poss_temp_out <- c()
            
            for (j in 1:length(poss_temp)){
              poss_temp_i <- poss_temp[j]
              temp_dict_curr <- temp_dict$get(curr)
              temp_dict_len <- length(temp_dict_curr)
              for (i in 1:temp_dict_len){
                poss_temp_out[i+(j-1)*temp_dict_len] <- paste0(poss_temp_i, temp_dict_curr[i]) #c(poss_temp_out, paste0(poss_temp_i, temp_dict_curr[i]))
              }
              print(paste(curr, j))
            }
            poss_temp <- poss_temp_out
            curr <- curr + 1
          }
          poss_out <- c(poss_out, poss_temp)
        }
        
        rules_dict$set(key, poss_out) # set new rule
        rules_converted <- c(rules_converted, key) # note key has been converted
      }
    }
  }
}

valid_patterns <- rules_dict$get("0")

sum(patterns %in% valid_patterns)



### PART 2

### NEW RULES:
# 8: 42 | 42 8
# 11: 42 31 | 42 11 31
# practicality: for 8, 42 has 128 possilities, so 8 is 128 + 128*128 + 128*128*128...
#               for 11, it's 16384 + 16384*16384 + .......

### Part 1 approach will definitely not work, let's try reworking the part 1 solution using regex

### same starting block
rules <- input[1:132,]
patterns <- input[134:nrow(input),1]
rules$rule_num <- gsub(":.*", "", rules$V1)
rules$rule <- gsub(".*: ", "", rules$V1)
rules <- rules[,c("rule_num", "rule")]
rules_dict <- dict(items = rules$rule, keys = rules$rule_num)
for (key in rules_dict$keys()){
  rules_dict$set(key, strsplit(rules_dict$get(key), split = " \\| ")[[1]])
}

rules_converted <- set()

for (key in rules_dict$keys()){
  if (prod(!str_detect(rules_dict$get(key), "[0-9]"))){
    rules_converted <- c(rules_converted, key)
  }
}


while (sum(str_detect(rules_dict$get("0"), "[0-9]")) > 0){ # check if any rule 0 poss. still references other rules
  for (key in rules_dict$keys()){
    if (!key %in% rules_converted){ #check if already in dict, if not in dict check rule for numbers
      
      # need to check if all referenced rules are in rules_converted
      
      rules_check <- unlist(strsplit(rules_dict$get(key), split = " "))
      
      if (prod(rules_check %in% rules_converted)){ #if all rules are in rules_converted, proceed
        possibilities <- rules_dict$get(key)
        poss_out <- c()
        
        for (possibility in possibilities){
          rules_to_add <- strsplit(possibility, split = " ")[[1]]
          
          temp_dict <- dict()
          
          for (i in 1:length(rules_to_add)){ #create separate poss for each number
            temp_dict$set(as.numeric(i), rules_dict$get(rules_to_add[i]))
          }
          
          curr <- 1
          poss_temp <- c("")
          
          while (curr <= length(rules_to_add)){ #combine possibilities together
            poss_temp_out <- c()
            
            for (j in 1:length(poss_temp)){
              poss_temp_i <- poss_temp[j]
              temp_dict_curr <- temp_dict$get(curr)
              temp_dict_len <- length(temp_dict_curr)
              for (i in 1:temp_dict_len){
                poss_temp_out[i+(j-1)*temp_dict_len] <- paste0(poss_temp_i, temp_dict_curr[i])
              }
              print(paste(curr, j))
            }
            poss_temp <- poss_temp_out
            curr <- curr + 1
          }
          poss_out <- c(poss_out, poss_temp)
        }
        
        poss_out <- paste(poss_out, collapse = "|")
        poss_out <- paste0("(", poss_out, ")")
        rules_dict$set(key, poss_out) # set new rule
        
        rules_converted <- c(rules_converted, key) # note key has been converted
      }
    }
  }
}

valid_patterns <- rules_dict$get("0")
valid_patterns <- paste0("^", valid_patterns, "$") #start and end of line checks

sum(str_detect(patterns, valid_patterns)) #this was the part 1 answer, in a much faster time!

### alright, sweet, this works! now, for new 8 and new 11, the patterns look like:
# 8: 42 | 42 8 --> 42 | 42 42 | 42 42 42 | 42 42 42 42 etc.....
# 11: 42 31 | 42 11 31 --> 42 31 | 42 42 31 31 | 42 42 42 31 31 31 | 42 42 42 42 31 31 31 31 etc......
# 0 is still: 8 11, so using regex we can construct this as:
# 0 --> 42*(4231)*, where 8 and 11 can occur any number of times

p42 <- rules_dict$get("42")
p31 <- rules_dict$get("31")
new_pattern <- paste0("^",p42,"(",p42,")*", #p42 at least once, then any number of p42
                      p42,"(",p42,p31,")*",p31,"$") #p42  once, N times p42, N times p31, p31

# the unlimited p42 for 8 is simple enough, but the 42*31* for 11, where the * has to be the same exact number
# not sure how to implement that precisely, so hacking it using a for loop
total <- 0
for (i in 0:10){
  new_pattern1 <- paste0("^",p42,"(",p42,")*", #p42 once, then any number of p42
                        p42, #p42 once
                        paste0(rep(p42, i), collapse = ""), #specific number of p42
                        paste0(rep(p31, i), collapse = ""),
                        p31,"$") #specific number of p31, then one p31
  
  total <- total + sum(str_detect(patterns, new_pattern1))
  print(total)
}

