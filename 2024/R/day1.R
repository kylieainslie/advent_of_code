# Day 1: Dec 1, 2024

library(readr)
library(dplyr)

day_1_data <- read_delim("~/Documents/advent_of_code_2024/data/day_1_data.csv",
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

### Part 1 - total distance between lists

# arange the two columns in ascending order
list1 <- sort(day_1_data$list1)
list2 <- sort(day_1_data$list2)

# find the distance between the two lists and sum
sum(abs(list1 - list2))

### Part 2 - similarity between lists

# determine how many times each entry in list1 appears in list2
test1 <- c(3, 4, 2, 1, 3, 3)
test2 <- c(4, 3, 5, 3, 9, 3)

similarity_vec <- c()

for (i in 1:length(list1)){

  n <- length(which(list2 == list1[i]))
  similarity_vec[i] <- list1[i] * n

}

# get similarity score
sum(similarity_vec)
