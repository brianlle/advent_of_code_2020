### ADVENT OF CODE, DAY 4
# 

input <- read.delim("input/day4.txt", header = FALSE,
                    blank.lines.skip = FALSE)

list <- c()
temp <- ""

for (i in 1:957){
  print(i)
  if (input[i,1] != ""){
    temp <- paste0(temp, input[i,1], " ")
  } else {
    temp <- substr(temp, 1, nchar(temp)-1)
    list <- c(list, temp)
    temp <- ""
  }
}

#"ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm"
valid_set <- sort(c("ecl", "pid", "eyr", "hcl", "byr", "iyr", "cid", "hgt"))
valid_set7 <- sort(c("ecl", "pid", "eyr", "hcl", "byr", "iyr", "hgt"))

is_valid <- function(pattern){
  pattern2 <- strsplit(pattern, " ")
  pattern_length <- length(pattern2[[1]])
  if (pattern_length < 7){
    return(FALSE)
  } else if (pattern_length > 8){
    return(FALSE)
  } else if (pattern_length == 8){
    pattern <- sort(gsub(":.*", "", pattern2[[1]]))
    if (identical(pattern, valid_set)){return(TRUE)} else {return(FALSE)}
  } else if (pattern_length == 7){
    pattern <- sort(gsub(":.*", "", pattern2[[1]]))
    if (identical(pattern, valid_set7)){return(TRUE)} else {return(FALSE)}
  }
}

input2 <- data.frame(data = list, valid = NA)
for (i in 1:253){
  input2[i,2] <- is_valid(as.character(input2[i,1]))
}

sum(input2$valid)

#part 2

# byr (Birth Year) - four digits; at least 1920 and at most 2002.
# cid (Country ID) - ignored, missing or not.
# ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
# eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
# hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
# hgt (Height) - a number followed by either cm or in:
#   If cm, the number must be at least 150 and at most 193.
#   If in, the number must be at least 59 and at most 76.
# iyr (Issue Year) - four digits; at least 2010 and at most 2020.
# pid (Passport ID) - a nine-digit number, including leading zeroes.

library(stringr)

is_valid_height <- function(pattern){
  units <- gsub("[0-9]*", "", pattern)
  value <- gsub("[a-zA-Z]*", "", pattern)
  if (units == "cm"){
    if (value >= 150 & value <= 193){
      return(TRUE)
    }
  } else if (units == "in"){
    if (value >= 59 & value <= 76){
      return(TRUE)
    }
  }
  return(FALSE)
}

is_valid_pid <- function(pattern){
  if (nchar(pattern) == 9){
    if (as.numeric(pattern) < 1000000000){
      return(TRUE)
    }
  }
  return(FALSE)
}

is_valid2 <- function(pattern){
  pattern2 <- strsplit(as.character(pattern), " ")
  pattern2 <- sort(pattern2[[1]])
  pattern2 <- gsub(".*:", "", pattern2)
  
  if (length(pattern2) == 7){
    pattern2 <- c(pattern2[1], "null", pattern2[2:7])
  }
  
  if ((pattern2[1] >= 1920) & (pattern2[1] <= 2002) &
      pattern2[3] %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth") &
      (pattern2[4] >= 2020) & (pattern2[4] <= 2030) &
      str_detect(pattern2[5], "#[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]") & 
      is_valid_height(pattern2[6]) &
      (pattern2[7] >= 2010) & (pattern2[7] <= 2020) &
      is_valid_pid(pattern2[8])
      ){
    return(TRUE)
  } else { return(FALSE) }
}

input2$valid2 <- NA
for (i in 1:253){
  print(i)
  if (input2[i,2]){
    input2[i,3] <- is_valid2(as.character(input2[i,1]))
  }
}
  
table(input2$valid2)
