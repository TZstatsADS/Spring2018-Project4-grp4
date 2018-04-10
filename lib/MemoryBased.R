##Transform Data MS
Transform_ms <- function(data){
  case_line <- which(data$V1 == 'C')
  user_id <- data$V3[case_line]
  vote_id <- sort(unique(data$V2[which(data$V1 == 'V')]))
  table <- matrix(0, nrow = length(user_id), ncol = length(vote_id))
  for(i in 1:length(case_line)){
    start_num <- case_line[i]
    end_num <- ifelse(i < length(case_line), case_line[i+1], nrow(data)+1)
    for(index in (start_num+1):(end_num-1)){
      j <- which(vote_id == data[index,3])
      table[i,j] <- 1
    }
  }
  rownames(table) <- as.character(user_id)
  colnames(table) <- as.character(vote_id)
  return(table)
}

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


