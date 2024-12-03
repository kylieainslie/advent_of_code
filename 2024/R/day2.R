# Day 2 - Red-Nosed Reports

library(readr)
library(dplyr)

# Test data
test <- t(matrix(c(7, 6, 4, 2, 1,
                   1, 2, 7, 8, 9,
                   9, 7, 6, 2, 1,
                   1, 3, 2, 4, 5,
                   8, 6, 4, 4, 1,
                   1, 3, 6, 7, 9), nrow = 5))

# Puzzle data
day_2_data <- read_delim("~/Documents/advent_of_code_2024/data/day_2_data.csv",
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

input <- day_2_data

### Part 1
safe <- c()

for (i in 1:nrow(input)){

  r <- input[i,]

  # remove NAs
  r <- r[!is.na(r)]

  # check if row is ascending or descending
  asc <- identical(r, sort(r))
  desc <- identical(r, sort(r, decreasing = TRUE))

  # if neither descending or ascending, row is UNSAFE, so add nothing to the
  # safe counter and move to the next row
  if(asc == FALSE && desc == FALSE){
    safe[i] <- "UNSAFE"
    next
  }

  # if ascending OR descending, then check difference between consecutive entries
  if(asc == TRUE || desc == TRUE){
    d <- abs(diff(r))
    if (any(d > 3) || any(d == 0)){
      safe[i] <- "UNSAFE"
    } else {
      safe[i] <- "SAFE"
    }
  }

}

# so we can visually check
input$safe <- safe

# number of safe reports
length(which(safe == "SAFE"))

### Part 2 - add the Problem Dampener
safe <- c()

for (i in 1:nrow(input)){

  r <- input[i,]

  # remove NAs
  r <- r[!is.na(r)]

  safe_alt <- c()
  # loop through r vector an remove one value then recheck if it's safe
  for(pd in 1:length(r)){
    r_alt <- r[-pd]

    # check if row is ascending or descending
    asc <- identical(r_alt, sort(r_alt))
    desc <- identical(r_alt, sort(r_alt, decreasing = TRUE))

    # if neither descending or ascending, row is UNSAFE, so add nothing to the
    # safe counter and move to the next row
    if(asc == FALSE && desc == FALSE){
      safe_alt[pd] <- "UNSAFE"
      next
    }

    # if ascending OR descending, then check difference between consecutive entries
    if(asc == TRUE || desc == TRUE){
      d <- abs(diff(r_alt))
      if (any(d > 3) || any(d == 0)){
        safe_alt[pd] <- "UNSAFE"
      } else {
        safe_alt[pd] <- "SAFE"
      }
    }
  } # end loop over values of r

  if(any(safe_alt == "SAFE")){
    safe[i] <- "SAFE"
  } else {
    safe[i] <- "UNSAFE"
  }
}

# number of safe reports
length(which(safe == "SAFE"))

