library(scales)
# Define the numbers to be formatted
salaries <- c(1200000, 1300000, 1300000, 1400000)
# Format the numbers with commas
salaries <- comma(salaries)

format_indian <- function(x) {
  format_single <- function(y) {
    y <- as.character(y)
    n <- nchar(y)
    if (n > 3) {
      last3 <- substr(y, n-2, n)
      other <- substr(y, 1, n-3)
      result <- paste0(gsub("(\\d)(?=(\\d{2})+$)", "\\1,", other, perl=TRUE), ",", last3)
    } else {
      result <- y
    }
    return(result)
  }

  if (is.vector(x)) {
    sapply(x, format_single)
  } else {
    format_single(x)
  }
}
