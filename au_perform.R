au_perform <- function (M) 
{
  M$TC <- as.numeric(M$TC)
  M$PY <- as.numeric(M$PY)
  M <- M[!is.na(M$PY), ]
  AU <- names(tableTag(M, "AU"))
  k <- length(AU)
  AU <- AU[1:k]
  df <- data.frame(Author = "NA", year = NA, TI = "NA", SO = "NA", 
                   DOI = "NA", TC = NA, TCpY = NA, stringsAsFactors = FALSE)
  Y <- as.numeric(substr(Sys.time(), 1, 4))
  if (!("DI" %in% names(M))) {
    M$DI = "NA"
  }
  for (i in 1:length(AU)) {
    ind <- which(regexpr(AU[i], M$AU) > -1)
    TCpY <- M$TC[ind]/(Y - M$PY[ind] + 1)
    dfAU <- data.frame(Author = rep(AU[i], length(ind)), 
                       year = M$PY[ind], TI = M$TI[ind], SO = M$SO[ind], 
                       DOI = M$DI[ind], TC = M$TC[ind], TCpY = TCpY, stringsAsFactors = TRUE)
    df <- rbind(df, dfAU)
  }
  df <- df[-1, ]
  df2 <- dplyr::group_by(df, .data$Author, .data$year) %>% 
    dplyr::summarise(freq = length(.data$year), TC = sum(.data$TC), 
                     TCpY = sum(.data$TCpY))
  df2 <- as.data.frame(df2)
  df2$Author <- factor(df2$Author, levels = AU[1:k])
  df2
}