# Day 3 - Mull it over

# load packages
library(readr)
library(stringr)

# example input
test <- "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
input <- read_file("data/day_3_input.txt")

### Part 1

# first define mul() function
mul <- function(x,y){
  if(x > 999 | y > 999){
    rtn <- NA
  } else{
    rtn <- x * y
  }

  return(rtn)
}

# search the input string for mul(digit, digit)
output <- str_extract_all(input, "mul\\([[:digit:]]+,[[:digit:]]+\\)")
output <- unlist(output)

# loop through recognised patterns and apply function mul()
mul_results <- c()
for (i in 1:length(output)){
  mul_results[i] <- eval(parse(text = output[i]))
}

# sum all the answers
sum(mul_results)

### Part 2
# search for do() and don't() as well
test2 <- "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

# search the input string for mul(digit, digit)
patterns <- "mul\\([[:digit:]]+,[[:digit:]]+\\)|do\\(\\)|don't\\(\\)"
output <- str_extract_all(input, patterns)
output <- unlist(output)

# remove the mul() calls after a don't() and before the next do()
success <- TRUE
out_enabled <- c()
for (i in 1:length(output)){

  if(output[i] == "don't()"){
    success <- FALSE
    next
  }
  if (output[i] == "do()"){
    success <- TRUE
  }

  if (!success){ next }

  out_enabled[i] <- output[i]
}

# remove NAs and "do()"
out_enabled <- out_enabled[!is.na(out_enabled)]
out_enabled <- out_enabled[out_enabled != "do()"]

# loop through recognised patterns and apply function mul()
mul_results <- c()
for (i in 1:length(out_enabled)){
  mul_results[i] <- eval(parse(text = out_enabled[i]))
}

sum(mul_results)
