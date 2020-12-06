#DAY 1

expenses <- read.csv("input/day1.csv", header = TRUE)
expenses <- expenses$values

#PART 1: find pair of values that add to 2020 and multiply together
#Brute force solution: nested for loop. Could improve speed by sorting arrays
#e.g. lowest -> highest, then search by outside pairs, iterating inwards

values_found <- FALSE
for (i in 1:(length(expenses)-1)){
  for (j in (i+1):length(expenses)){
    if ((expenses[i] + expenses[j]) == 2020){
      print(paste(i,j))
      value_1 <- expenses[i]
      value_2 <- expenses[j]
      values_found <- TRUE
      break
    }
  }
  if (values_found){break} #very janky way to break out of double loop
}

print(value_1 * value_2)

#PART 2: find triplet of values that add to 2020 and multiply together
#Brute force solution: ... more nested for loops!

values_found <- FALSE
for (i in 1:(length(expenses)-2)){
  for (j in (i+1):(length(expenses)-1)){
    for (k in (j+1):length(expenses)){
      if ((expenses[i] + expenses[j] + expenses[k]) == 2020){
        print(paste(i,j,k))
        value_1 <- expenses[i]
        value_2 <- expenses[j]
        value_3 <- expenses[k]
        values_found <- TRUE
        break
      }
    }
    if (values_found){break}
  }
  if (values_found){break}
}

print(value_1 * value_2 * value_3)
