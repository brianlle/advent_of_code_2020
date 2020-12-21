### ADVENT OF CODE, DAY 21
# PART 1

input <- read.delim("input/day21.txt", header = FALSE,
                    blank.lines.skip = TRUE)

library(collections)

input$lang <- gsub("\\(.*", "", input$V1)
input$eng <- gsub(".*contains ", "", input$V1)
input$eng <- gsub("[,)]", "", input$eng)

allergies <- dict()
ing_count <- dict()
all_ingredients <- c()

for (i in 1:nrow(input)){
  ingredients <- strsplit(input[i,2], split = " ")[[1]]
  all_ingredients <- c(all_ingredients, ingredients)
  contains <- strsplit(input[i,3], split = " ")[[1]]
  
  for (ing in ingredients){
    if (ing %in% ing_count$keys()){
      ing_count$set(ing, 1 + ing_count$get(ing))
    } else {
      ing_count$set(ing, 1)
    }
  }
  
  for (ing in contains){
    if (ing %in% allergies$keys()){
      allergies$set(ing, intersect(allergies$get(ing), ingredients))
      ing_count$set(ing, 1 + ing_count$get(ing))
    } else {
      allergies$set(ing, ingredients)
      ing_count$set(ing, 1)
    }
  }
}

all_ingredients <- unique(all_ingredients)
maybe_allergies <- c()

for (ing in allergies$keys()){
  maybe_allergies <- c(maybe_allergies, allergies$get(ing))
}

no_allergies <- all_ingredients[!all_ingredients %in% maybe_allergies]

no_allergy_count <- 0
test <- c()

for (ing in no_allergies){
  test <- c(test, ing_count$get(ing))
  no_allergy_count <- no_allergy_count + ing_count$get(ing)
}

print(no_allergy_count)


### PART 2

combos <- c()
for (key in allergies$keys()){
  combos <- c(combos, paste(key, allergies$get(key)))
}

c_ing <- gsub(".* ", "", combos)
c_all <- gsub(" .*", "", combos)
combos <- data.frame(allergy = c_all, ingred = c_ing)
combos <- as.data.frame.matrix(table(combos$allergy, combos$ingred))
combos <- cbind(combos, 0)
combos <- rbind(combos, 0) # buffers

confirmed_allergy <- c()
confirmed_ingred <- c()

while(length(confirmed_allergy) < 8){
  for (i in 1:ncol(combos)){
    break_out <- FALSE
    if (sum(combos[,i]) == 1){
      for (j in 1:nrow(combos)){
        if (combos[j,i] == 1){
          confirmed_allergy <- c(confirmed_allergy, rownames(combos)[j])
          confirmed_ingred <- c(confirmed_ingred, colnames(combos)[i])
          combos <- combos[-j,-i]
          break_out <- TRUE
          break
        }
      }
    }
    if (break_out == TRUE){
      break
    }
  }
}

matches <- data.frame(allergy = confirmed_allergy, ingred = confirmed_ingred)
matches <- matches[order(matches$allergy),]
print(paste0(matches$ingred, collapse = ","))
# answer: hkflr,ctmcqjf,bfrq,srxphcm,snmxl,zvx,bd,mqvk