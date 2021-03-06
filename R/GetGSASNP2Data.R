#'
#' @title Transform GSA-SNP2 result to use GScluster
#'
#' @param filename directory of GSA-SNP2 result file
#'
#' @usage GetGSASNP2Data(filename)
#'
#' @export
#'

GetGSASNP2Data <- function(filename) {
  tab <- read.delim(filename, header = T, stringsAsFactors = FALSE)
  GS <- tab[, 1]
  Qvalues <- as.numeric(tab[, 7])

  Genes <- sort(unique(strsplit(paste(tab[, 8], collapse = "; "), "; ")[[1]]))
  Genes <- Genes[-1] # remove " "

  GeneScores <- matrix(0, length(Genes), 2)

  for (i in 1:length(Genes)) {
    temp <- strsplit(Genes[i], "\\(")[[1]]
    GeneScores[i, 1] <- temp[1]
    v <- as.numeric(strsplit(temp[2], "\\)")[[1]][1])
    GeneScores[i, 2] <- 2^(-max(c(v, 0)))
  }

  Genelist <- c()
  for (i in 1:nrow(tab)) {
    Genelist[i] <- paste(sort(unname(sapply(strsplit(tab[i, 8], "; ")[[1]], function(i) {
      strsplit(i, "\\(")[[1]][1]
    }))), collapse = " ")
  }

  GSAresult <- data.frame(cbind(GS, Genelist, Qvalues), stringsAsFactors = FALSE)
  GSAresult[, 3] <- as.numeric(GSAresult[, 3])
  GeneScores <- data.frame(GeneScores, stringsAsFactors = FALSE)
  GeneScores[, 2] <- as.numeric(GeneScores[, 2])
  return(list(GSAresult = GSAresult, GeneScores = GeneScores))
}
