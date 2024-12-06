# Day 5: Print Queue

# load packages
library(stringr)
library(tidyr)
library(dplyr)

### Part 1

# function to convert rules from a character string to a data.frame
convert_rules <- function(rules_string){
  rules_vec <- str_split_1(rules_string, pattern = "\n")
  rules_df <- data.frame(rule = rules_vec) %>%
    separate_wider_delim(rule, delim = "|", names = c("page1", "page2")) %>%
    mutate(page1 = as.numeric(page1),
           page2 = as.numeric(page2))

  return(rules_df)

}

# rules strings
test_rules <- "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13"

input_rules <- source("2024/R/day_5_input.R")$value

# convert rules into two column data frame
rules_test <- convert_rules(test_rules)
rules_input <- convert_rules(input_rules)

# page updates
test_updates <- list(u1 = c(75,47,61,53,29),
                     u2 = c(97,61,53,29,13),
                     u3 = c(75,29,13),
                     u4 = c(75,97,47,61,53),
                     u5 = c(61,13,29),
                     u6 = c(97,13,75,29,47)
                    )

input_updates_raw <- readLines("2024/data/day_5_input.txt")
# Split the strings into lists of character vectors
split_character <- strsplit(input_updates_raw, split = ",")

# Convert each character vector to a numeric vector
input_updates <- lapply(split_character, as.numeric)

# function to check rules
check_rules <- function(updates, rules_df){
# loop through updates and check the page ordering against the rules
correct_updates <- 0 # counter for correct updates
middle_values <- numeric(length(updates))

for (l in 1: length(updates)){

  current_update <- updates[[l]]
  counter <- 0
  # loop over rules
  for (r in 1:nrow(rules_df)){
    get_index1 <- match(as.numeric(rules_df[r, 1]), current_update)
                  #which(current_update == as.numeric(rules_df[r,1]))
    get_index2 <- match(as.numeric(rules_df[r, 2]), current_update)
                  #which(current_update == as.numeric(rules_df[r,2]))

    if (is.na(get_index1) || is.na(get_index2)) {
      next
    }

    # Check rule
    if (get_index1 < get_index2) {
      counter <- counter + 1
    } else {
      counter <- -999

    }
  }

  if(counter > 0){
    correct_updates <- correct_updates + 1
    middle_values[l] <- current_update[ceiling(length(current_update)/2)]
  } else{
    middle_values[l] <- 0
  }
}

return(list(num_correct = correct_updates,
            sum_middle_values = sum(middle_values)))
}

# check rules
check_rules(test_updates, rules_test)
check_rules(input_updates, rules_input)

### Part 2

# update check_rules() to spit out the incorrect update sequences
check_rules2 <- function(updates, rules_df){
  # loop through updates and check the page ordering against the rules
  correct_updates <- 0 # counter for correct updates
  middle_values <- c()
  incorrect_updates <- list()
  rules_broken <- list()

  for (l in 1: length(updates)){

    current_update <- updates[[l]]
    counter <- 0
    rules_broken_counter <- c()
    # loop over rules
    for (r in 1:nrow(rules_df)){
      get_index1 <- which(current_update == as.numeric(rules_df[r,1]))
      get_index2 <- which(current_update == as.numeric(rules_df[r,2]))

      if(identical(get_index1, integer(0)) || identical(get_index2, integer(0))){
        next
      } else if(get_index1 < get_index2){
        counter <- counter + 1
      } else{
        counter <- -999
        rules_broken_counter <- c(rules_broken_counter, r)
      }
    }

    if(counter > 0){
      correct_updates <- correct_updates + 1
      middle_values[l] <- current_update[ceiling(length(current_update)/2)]
    } else{
      middle_values[l] <- 0
      incorrect_updates[[l]] <- current_update
      rules_broken[[l]] <- rules_df[rules_broken_counter,]
    }
  }

  return(list(num_correct = correct_updates,
              sum_middle_values = sum(middle_values),
              incorrect_updates = incorrect_updates,
              rules_broken = rules_broken))
}

incorrect_test <- check_rules2(test_updates, rules_test)$incorrect_updates
incorrect_input <- check_rules2(input_updates, rules_input)$incorrect_updates

# create function to fix the incorrect updates
fix_incorrect <- function(updates, rules_df, max_iterations = 100) {
  corrected_updates <- list()
  middle_values <- c()

  # Create progress bar
  pb <- txtProgressBar(min = 0, max = length(updates), initial = 0)

  for (l in 1:length(updates)) {
    setTxtProgressBar(pb, l) # Update progress bar

    if (is.null(updates[[l]])) {
      corrected_updates[[l]] <- NULL
      middle_values[l] <- 0
      next
    } else {
      tmp <- updates[[l]]
      iterations <- 0

      # Keep reordering until all rules are satisfied
      while (TRUE) {
        rules_check <- check_rules(list(tmp), rules_df)
        if (rules_check$num_correct == 1) {
          break
        }

        # Fix broken rules one by one
        for (r in 1:nrow(rules_df)) {
          page1 <- rules_df$page1[r]
          page2 <- rules_df$page2[r]

          # Find indices of page1 and page2 in tmp
          idx1 <- match(page1, tmp)
          idx2 <- match(page2, tmp)

          # Skip if either page1 or page2 is missing
          if (is.na(idx1) || is.na(idx2)) {
            next
          }

          # If page1 appears after page2, swap their positions
          if (idx1 > idx2) {
            tmp[c(idx1, idx2)] <- tmp[c(idx2, idx1)]
          }
        }

        # Prevent infinite loops
        iterations <- iterations + 1
        if (iterations >= max_iterations) {
          warning(paste("Max iterations reached for index", l))
          break
        }
      }

      # Store the corrected update
      corrected_updates[[l]] <- tmp
      middle_values[l] <- tmp[ceiling(length(tmp) / 2)]
    }
  }

  close(pb) # Close progress bar

  return(sum(middle_values))
}

fix_incorrect(incorrect_test, rules_test)
fix_incorrect(incorrect_input, rules_input) # this is very inefficient!






