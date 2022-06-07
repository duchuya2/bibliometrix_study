citation_n <- function (M, field = "article", sep = ";") 
{
  CR = NULL
  Year = NULL
  SO = NULL
  listCR = strsplit(M$CR, sep)
  if (sum(nchar(listCR) > 3, na.rm = TRUE) == 0) {
    cat("\nReference metadata field 'CR' is empty!!\n\n")
    return(NA)
  }
  if (field == "author") {
    if (M$DB[1] == "ISI") {
      listCR = lapply(listCR, function(l) {
        ListL = lapply(strsplit(unlist(l), ","), function(x) x[1])
        l = trimws(trimES(gsub("[[:punct:]]", " ", unlist(ListL))))
      })
    }
    if (M$DB[1] == "SCOPUS") {
      listCR = lapply(listCR, function(l) {
        ListL = lapply(l, function(x) {
          a = strsplit(x, "\\., ")
          ind = which(grepl("[[:digit:]]", a[[1]]))
          if (length(ind) == 0) 
            ind = 1
          x = unlist(a[[1]][1:(ind[1] - 1)])
        })
        l = trimws(trimES(gsub("[[:punct:]]", " ", unlist(ListL))))
      })
    }
  }
  if (field == "article") {
    listCR = lapply(listCR, function(l) {
      l = l[grep(",", l)]
    })
  }
  CR = unlist(listCR)
  CR = CR[nchar(CR) >= 3]
  CR = trim.leading(CR)
  CR = sort(table(CR), decreasing = TRUE)
  if (field == "article") {
    switch(M$DB[1], ISI = {
      listCR = strsplit(rownames(CR), ",")
      Year = unlist(lapply(listCR, function(l) {
        if (length(l) > 1) {
          l = suppressWarnings(as.numeric(l[2]))
        } else {
          l = NA
        }
      }))
      SO = unlist(lapply(listCR, function(l) {
        if (length(l) > 2) {
          l = l[3]
        } else {
          l = NA
        }
      }))
      SO = trim.leading(SO)
    }, SCOPUS = {
      REF = names(CR)
      y = yearSoExtract(REF)
      Year = as.numeric(y$Year)
      SO = y$SO
    })
  }
  CR = list(Cited = CR, Year = Year, Source = SO)
  return(CR)
}
