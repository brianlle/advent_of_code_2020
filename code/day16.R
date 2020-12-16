### ADVENT OF CODE, DAY 16
# PART 1

input <- read.delim("input/day16.txt", header = FALSE,
                    blank.lines.skip = FALSE)
input$V2 <- NA

rules <- input[1:20,]
my_ticket <- strsplit(as.character(input[23,1]), split = ",")[[1]]
other_tix <- input[26:263,]

#process the rules with dirty regex
rules$field <- gsub(": .*", "", rules$V1)
rules$nums <- gsub(".*: ", "", rules$V1)
rules$range1 <- gsub(" or .*", "", rules$nums)
rules$range2 <- gsub(".* or ", "", rules$nums)
rules$num1 <- gsub("-.*", "", rules$range1)
rules$num2 <- gsub(".*-", "", rules$range1)
rules$num3 <- gsub("-.*", "", rules$range2)
rules$num4 <- gsub(".*-", "", rules$range2)

#extract ranges of valid numbers for each rule
range <- list()
all_rules <- c()
for (i in 1:nrow(rules)){
  range[[i]] <- c(as.numeric(rules$num1[i]):as.numeric(rules$num2[i]),
                      as.numeric(rules$num3[i]):as.numeric(rules$num4[i]))
  all_rules <- c(all_rules, range[[i]])
}

#combine all rule ranges together for part 1 check, should prolly use an actual set
all_rules <- unique(all_rules)

#process tickets
tix <- list()
for (i in 1:nrow(other_tix)){
  tix[[i]] <- strsplit(as.character(other_tix[i,1]), split = ",")[[1]]
}

invalid_value <- c()

for (i in 1:length(tix)){
  ticket <- tix[[i]]
  for (j in 1:length(ticket)){
    if (ticket[j] %in% all_rules){
      # if valid match, do nothing and keep checking fields
    } else {
      invalid_value <- c(invalid_value, as.numeric(ticket[j]))
    }
  }
}

print(sum(invalid_value))

### PART 2
# start by finding tickets with valid values

valid_tix <- c()

for (i in 1:length(tix)){
  ticket <- tix[[i]]
  valid = 0
  for (j in 1:length(ticket)){
    if (ticket[j] %in% all_rules){
      valid = valid + 1
    } 
  }
  if (valid == length(ticket)){ # if all 20 ticket values are validm, ticket is valid
    valid_tix <- c(valid_tix, i)
  }
}

# using the valid tickets, for each rule, find ticket fields that would satisfy the rule
possible_fields <- list()

for (i in 1:20){
  temp_range <- range[[i]]
  possible_fields[[i]] <- c(0) # initialize with 0
  for (field in 1:20){
    valid <- 0
    for (ticket_idx in valid_tix){
      ticket_field <- tix[[ticket_idx]][[field]]
      if (!ticket_field %in% temp_range){
        print(paste("not", field))
        break
      } else {
        valid <- valid + 1
      }
    }
    if (valid == 190){possible_fields[[i]] <- c(possible_fields[[i]], field)}
  }
}

for (i in 1:20){ # remove initailization value
  possible_fields[[i]] <- possible_fields[[i]][-1]
}

# rules can have multiple fields that match, but appears to be a simple process of elim puzzle
field_match <- c() #rule fields: e.g. rule 1, rule 2, rule 3
ticket_match <- c() #ticdket fields: e.g. 1st number, 2nd number, 3rd number

for (k in 1:20){ # repeat 20 times total
  for (i in 1:20){ #i is rule row; possible_fields is what ticket fields are possible for that row
    if (length(possible_fields[[i]]) == 1){
      field_found <- possible_fields[[i]]
      field_match <- c(field_match, i)
      ticket_match <- c(ticket_match, field_found)
      possible_fields[[i]] <- 1:50
      for (j in 1:20){
        possible_fields[[j]] <- possible_fields[[j]][possible_fields[[j]] != field_found]
      }
      break
    }
  }
}

rule_names <- rules[1:20,3]
rules_df <- data.frame(rule_name = rule_names, rule_num = 1:20)
match_df <- data.frame(rule_num = field_match, ticket_field = ticket_match)
match_df <- merge(rules_df, match_df, by = "rule_num")

dep_fields <- match_df[1:6, 3] # departure fields are 1st 6 rows, so get matching ticket fields

my_ticket_fields <- my_ticket[dep_fields]
print(prod(as.numeric(my_ticket_fields)), digits = 20)
