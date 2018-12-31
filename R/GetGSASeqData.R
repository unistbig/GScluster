#'
#' @title Transform GSA-Seq result to use GScluster
#'
#' @param filename directory of GSA-SNP2 result file
#'
#' @usage GetGSASeqData(filename)
#'
#' @export
#'
#'

GetGSASeqData = function(filename){
  tab = read.delim(filename, header = T, stringsAsFactors = FALSE)
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
  return(GSAresult)
}


