#'
#' @title Transform GSA-Seq result to use GScluster
#'
#' @param GSA directory of GSA-seq result file ( GSA result )
#' @param Score directory of GSA-seq result file ( Gene score )
#'
#' @usage GetGSASeqData(GSA, Score)
#'
#' @export
#'
#'

GetGSASeqData = function(GSA, Score){
  tab = read.delim(GSA, header = T, stringsAsFactors = FALSE)
  GS = tab[,1]
  Qvalues = as.numeric(tab[,8])
  Direction = tab[,10]
  Direction[which(Direction=='DOWN')] = 'DN'
  Genes = sort(unique(strsplit(paste(tab[,11],collapse = ' '),' ')[[1]]))

  Genelist = c()
  for(i in 1:nrow(tab)){
    Genelist[i] = paste(sort(unname(sapply(strsplit(tab[i,11],' ')[[1]], function(i){strsplit(i,'\\(')[[1]][1]}))),collapse = ' ')
  }

  GSAresult = data.frame(cbind(GS, Genelist, Qvalues, Direction), stringsAsFactors = FALSE)
  GSAresult[,3] = as.numeric(GSAresult[,3])

  tab = read.delim(score, stringsAsFactors = FALSE)
  GeneScores = data.frame(tab[,c(1,4)], stringsAsFactors = FALSE)

  return(list(GSAresult = GSAresult, GeneScores = GeneScores))
}


