##Transform movie data
Transform_m <- function(data){
  rows <- sort(unique(data$User))
  columns <- sort(unique(data$Movie))
  table <- matrix(NA, nrow = length(rows), ncol = length(columns))
  for(index in 1:length(rows)){
    row.name <- rows[index]
    i <- which(data$User == row.name)
    movies <- data[i,2]
    scores <- data[i,4]
    j <- which(columns %in% movies)
    table[index,j] <- scores
  }
  rownames(table) <- rows
  colnames(table) <- columns
  return(table)
}
