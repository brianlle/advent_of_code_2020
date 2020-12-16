### ADVENT OF CODE, DAY 15
# PART 1

input <- c(0,1,5,10,3,12,19)

spoken <- data.frame(key = numeric(), prior = numeric(), recent = numeric())


next_num <- function(num, current_time, dataframe){
  if (num %in% dataframe$key){
    dataframe[dataframe$key == num, "prior"] <- dataframe[dataframe$key == num, "recent"]
    dataframe[dataframe$key == num, "recent"] <- current_time
  } else {
    dataframe <- rbind(dataframe, data.frame(key = num, prior = 0, recent = current_time))
  }
  return(dataframe)
}

# next_num(5, 1, spoken)
spoken <- data.frame(key = numeric(), prior = numeric(), recent = numeric())
#spoken_values <- c(3,1,2)
spoken_values <- c(0,1,5,10,3,12,19)

for (i in 1:7){
  spoken <- next_num(spoken_values[i], i, spoken)
}

for (i in 8:2020){
  recent <- spoken_values[i-1]
  if (recent %in% spoken$key){
    if (spoken[spoken$key == recent,"prior"] == 0){
      recent_time <- 0
    } else {
      recent_time <- spoken[spoken$key == recent,"recent"] - spoken[spoken$key == recent,"prior"]
    }
  } else {
    recent_time <- 0
  }

  spoken <- next_num(recent_time, i, spoken)
  spoken_values <- c(spoken_values, recent_time)
}

spoken_values[2020]


# PART 2

# library(hash)
# 
# next_num <- function(num, current_time, spokenset){
#   num <- as.character(num)
#   if (has.key(num, spokenset)){
#     rec <- spokenset[[num]][2]
#     spokenset[[num]] <- c(rec, current_time)
#     #dataframe[num, 1:2] <- c(rec, current_time)
#   } else {
#     spokenset[[num]] <- c(0, current_time)
#     # dataframe <- rbind(dataframe, data.frame(prior = 0, recent = current_time,
#     #                                          row.names = c(as.character(num))))
#   }
#   return(spokenset)
# }
# 
# spoken <- hash() # key : list (2 elements; 1 = prior, 2 = recent)
# #spoken <- data.frame(prior = numeric(), recent = numeric())
# #spoken_values <- c(3,1,2)
# spoken_values <- c(0,1,5,10,3,12,19)
# spoken_set <- set()
# 
# for (i in 1:7){
#   num <- as.character(spoken_values[i])
#   spoken[[num]] <- c(0,i)
#   #spoken <- next_num(spoken_values[i], spoken_set, i, spoken)
#   spoken_set <- c(spoken_set, spoken_values[i])
# }
# 
# recent <- spoken_values[7]
# 
# for (i in 8:30000000){
#   print(i)
#   if (i %% 100000 == 0){
#     print(i)
#   }
# 
#   #if (recent %in% spoken_set){
#   if (has.key(as.character(recent), spoken)){  
#     # prior1 <- spoken[as.character(recent),"prior"]
#     # recent1 <- spoken[as.character(recent),"recent"]
#     prior1 <- spoken[[as.character(recent)]][1]
#     recent1 <- spoken[[as.character(recent)]][2]
#     
#     if (prior1 == 0){
#       recent <- 0
#     } else {
#       recent <- recent1 - prior1
#     }
#   } else {
#     recent <- 0
#     #print(i)
#   }
#   
#   spoken <- next_num(recent, i, spoken)
#   #spoken_set <- c(spoken_set, recent)
# }
# 
# recent
# 
# 
# ### try 2, , try 1 would've taken 24+ hours, attempting to use fewer lookups to make it run faster
# 
# spoken <- hash() # key : list (2 elements; 1 = prior, 2 = recent)
# spoken_values <- c(0,1,5,10,3,12,19)
# 
# 
# for (i in 1:7){
#   num <- as.character(spoken_values[i])
#   spoken[[num]] <- c(0,i)
# }
# 
# recent <- spoken_values[7]
# 
# for (i in 8:30000000){
#   if (i %% 100000 == 0){
#     print(i)
#   }
#   
#   prior1 <- spoken[[as.character(recent)]][1]
#   recent1 <- spoken[[as.character(recent)]][2] 
#   
#   if (prior1 == 0){
#     recent <- 0
#   } else {
#     recent <- recent1 - prior1
#   }
#   
#   if (has.key(as.character(recent), spoken)){  
#     spoken[[as.character(recent)]] <- c(spoken[[as.character(recent)]][2], i)
#   } else {
#     spoken[[as.character(recent)]] <- c(0, i)
#     #print(i)
#   }
# }

### try 2 would've been overnight, try 3 using library(collections), allegedly faster than library(hash)
## finished in under 3 minutes!

library(collections)

#spoken <- hash() # key : list (2 elements; 1 = prior, 2 = recent)
spoken <- dict(items = NULL, keys = NULL)
spoken_values <- c(0,1,5,10,3,12,19)

for (i in 1:7){
  spoken$set(spoken_values[i], c(0,i))
}

num <- spoken_values[7]

print(Sys.time())
for (i in 8:30000000){
  
  prior_recent <- spoken$get(num)
  prior <- prior_recent[1]
  recent <- prior_recent[2]
  
  if (prior == 0){
    num <- 0
  } else {
    num <- recent - prior
  }
  
  if (spoken$has(num)){  
    spoken$set(num, c(spoken$get(num)[2], i))
  } else {
    spoken$set(num, c(0, i))
  }
}

print(num)
print(Sys.time()) #2:30 to run!
