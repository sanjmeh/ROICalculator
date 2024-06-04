library(scales)
# Define the numbers to be formatted
salaries <- c(1200000, 1300000, 1300000, 1400000)
# Format the numbers with commas
salaries <- comma(salaries)


# Indian style formating on numbers: 1,00,000
format_indian <- function(x) {
  format_single <- function(y) {
    y <- as.character(y)
    if (grepl("\\.", y)) {
      parts <- unlist(strsplit(y, "\\."))
      int_part <- parts[1]
      dec_part <- parts[2]
    } else {
      int_part <- y
      dec_part <- NULL
    }
    n <- nchar(int_part)
    if (n > 3) {
      last3 <- substr(int_part, n-2, n)
      other <- substr(int_part, 1, n-3)
      formatted_int <- paste0(gsub("(\\d)(?=(\\d{2})+$)", "\\1,", other, perl=TRUE), ",", last3)
    } else {
      formatted_int <- int_part
    }
    if (!is.null(dec_part)) {
      result <- paste0(formatted_int, ".", dec_part)
    } else {
      result <- formatted_int
    }
    return(result)
  }

  if (is.vector(x)) {
    sapply(x, format_single)
  } else {
    format_single(x)
  }
}


# Function for dynamically updating text outputs
updateTextOutput <- function(outputId, newText) {
  output[[outputId]] <- renderText({
    newText
  })
}








