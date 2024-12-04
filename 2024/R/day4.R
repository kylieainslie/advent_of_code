# Day 4 - Ceres Search

### Part 1
library(stringr)
library(readr)

test <- "MMMSXXMASMMSAMXMSMSAAMXSXMAAMMMSAMASMSMXXMASAMXAMMXXAMMXXAMASMSMSASXSSSAXAMASAAAMAMMMXMMMMMXMXAXMASX"

input <- read_file("2024/data/day_4_input.txt")

# split character string into individual letters
test2 <- str_extract_all(test, boundary("character"))
input2 <- str_extract_all(input, boundary("character"))
input3 <- input2[[1]][-which(input2[[1]] == "\n")]

# convert into matrix
test_mat <- t(matrix(test2[[1]], nrow = 10))
input_mat <- t(matrix(input3[-c(1, length(input3))], nrow = 140))

# helper function
reverse_string <- function(string) {
  paste(rev(strsplit(string, NULL)[[1]]), collapse = "")
}

# create function to search within the matrix for a patter (word)
word_search <- function(mat, word){

# combine characters in each row
  row_entries <- apply(mat, 1, paste, collapse = "")
  # combine columns
  col_entries <- apply(mat, 2, paste, collapse = "")
  # combine diagonals
    main_diag <- list() # Create a list to store main diagonals
    anti_diag <- list() # Create a list to store anti diagonals

    # Extract main diagonals (row - col = offset)
    for (offset in -(nrow(mat) - 1):(ncol(mat) - 1)) {
      diagonal_indices <- which(row(mat) - col(mat) == offset, arr.ind = TRUE)
      main_diag[[as.character(offset)]] <- mat[diagonal_indices]
    }

    # Extract anti-diagonals (row + col = constant)
    for (sum_offset in 2:sum(nrow(mat),ncol(mat))) {
      anti_diagonal_indices <- which(row(mat) + col(mat) == sum_offset, arr.ind = TRUE)
      anti_diag[[as.character(sum_offset)]] <- mat[anti_diagonal_indices]
    }

  main_diag1 <- unlist(lapply(main_diag, paste, collapse = ""))
  anti_diag1 <- unlist(lapply(anti_diag, paste, collapse = ""))

  # all entries
  all_entries <- c(row_entries, col_entries, main_diag1, anti_diag1)

  # find word pattern (both forwards and backwards)
  forwards <- str_extract_all(all_entries, word)
  backwards <- str_extract_all(all_entries, reverse_string(word))

  results <- c(unlist(forwards), unlist(backwards))

  return(length(results))

}

word_search(test_mat, "XMAS")
word_search(input_mat, "XMAS")

### Part 2: Find X-MAS
library(purrr)
library(tidyr)

# helper functions
ind <- function(k, n, o) Map(`:`, seq(1, k-n+1, by = o), seq(n, k, by = o))

my_partition <- function(mat, n, o) {
  lapply(cross2(ind(nrow(mat),n,o), ind(ncol(mat),n,o)), function(i) mat[i[[1]], i[[2]]])
}

# main function

find_x <- function(mat, word){
  # divide mat into square matrices of w x w, where w is the number of letters in word
  word_letters <- str_extract_all(word, boundary("character"))[[1]]
  w <- length(word_letters)
  my_blocks <- my_partition(mat, w, 1)

  # loop over all block matrices to see if they contain X-MAS
  block_counter <- 0

  for(l in 1:length(my_blocks)){
    diags <- paste(diag(my_blocks[[l]]), collapse = "")
    anti_diags <- paste(my_blocks[[l]][cbind(1:nrow(my_blocks[[l]]), ncol(my_blocks[[l]]):1)],
                        collapse = "")

    find_word_f <- str_extract_all(c(diags, anti_diags), word)
    find_word_b <- str_extract_all(c(diags, anti_diags), reverse_string(word))

    if (length(unlist(find_word_f)) == 2 ||
        length(unlist(find_word_b)) == 2 ||
        (length(unlist(find_word_f)) == 1 && length(unlist(find_word_b)) == 1)
        ){
      block_counter <- block_counter + 1
    }
  }

  return(block_counter)
}

find_x(test_mat, "MAS")
find_x(input_mat, "MAS")
