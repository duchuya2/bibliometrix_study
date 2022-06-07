# The function of combining the data

merge.wos.scopus <- function (..., remove.duplicated = TRUE){
  L <- list(...)
  n = length(L)
  Tags = names(L[[1]])
  for (j in 2:n) {
    Tags = intersect(Tags, names(L[[j]]))
  }
  M = data.frame(matrix(NA, 1, length(Tags)))
  names(M) = Tags
  for (j in 1:n) {
    L[[j]] = L[[j]][, Tags]
    M = rbind(M, L[[j]])
  }
  M = M[-1, ]
  M$DB = "ISI"
  remove.duplicated <- "T"
  if (remove.duplicated=="T") {
    M$TI = gsub("[^[:alnum:] ]", "", M$TI)
    M$TI = gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", M$TI, 
                perl = TRUE)
    d = duplicated(M$TI)
    cat("\n", sum(d), "duplicated documents have been removed\n")
    M = M[!d, ]
  }
}