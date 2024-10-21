count_occurrences <- function(vector) {
  # Convert the vector into a data frame
  df <- as.data.frame(vector)
  
  # Create a frequency table of the vector
  freq_table <- table(df$vector)
  
  # Match the original vector to get the frequencies in the same order
  out <- freq_table[as.character(df$vector)]
  
  # Return the frequencies
  return(as.numeric(out))
}
