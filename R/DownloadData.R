#'
#' @title Download additional GScluster Data
#'
#' @param species text value for which species PPI data will downloaded.
#' possible value is : arabidopsis, celegans, eColi, fly, human, mouse, rat, yeast, zebrafish
#'
#' @usage DownloadData('human')
#'
#'
#' @export

DownloadData = function(species){
  NowDir = getwd()
  dataDir = file.path(paste0(system.file("mypkg", package = 'mypkg'),'/',species))
  if(!dir.exists(dataDir)) { dir.create(dataDir,showWarnings = FALSE) }

  setwd(dataDir)
  filelist = paste0('l',1:6,'.RData')
  filelist = c(filelist,'string.RData')
  if(species == 'human') { filelist = c(filelist,'hippie.RData') }

  filelist = setdiff(filelist, dir())

  urls = paste0('https://github.com/unistbig/GScluster-Data/raw/master/', species,'/',filelist)
  for(i in 1:length(urls)){
    download.file(urls[i], filelist[i])
  }

  setwd(NowDir)
}
