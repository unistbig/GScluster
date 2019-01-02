#'
#' @title Run GScluster
#'
#' @param GSAresult R object contain GSAresult , 3 (name,member,qvalue) or 4 (+direction) column matrix
#' @param GeneScores R object contain DE analysis result, 2 (genesymbol, score) column matrix
#' @param PPI Matrix with rowname, colname : genesymbol and value should 0 ~ 1. if null, STRING will be used.
#' @param Species A character for species, default is Human.
#' possible value is : (A)rabidopsis, (C).elegans, (E).Coli, (F)ly, (H)uman, (M)ouse, (R)at, (Y)east, (Z)ebrafish
#' @param alpha A numeric value between 0(No network) ~ 1 (Full network). weight for network in pMM Distance. default is 1
#' @param GsQCutoff A numeric value, threshold to use geneset. default is 0.25
#' @param GQCutoff A numeric value, threshold to use gene. default is 0.25
#'
#'
#'
#' @usage GScluster() # will run demo data
#'
#'
#' @export

GScluster = function(
  GSAresult = NULL, GeneScores = NULL, PPI = NULL,
  Species ='H', alpha = 1, GsQCutoff = 0.25, GQCutoff = 0.25){
  # arg1 -> inst/FOLDERNAME ( GScluster )
  # arg2 -> ...?
  appDir = system.file("GScluster", package = 'GScluster')
  if(appDir ==''){
    stop(
      "Could not find GScluster Directory, Try re-installing 'GScluster'.",
      call. = FALSE
    )
  }
  .GlobalEnv$.GeneScores = GeneScores
  .GlobalEnv$.GSAresult = GSAresult
  .GlobalEnv$.PPI = PPI
  .GlobalEnv$.Species = Species
  .GlobalEnv$.alpha = alpha
  .GlobalEnv$.GsQCutoff = GsQCutoff
  .GlobalEnv$.GQCutoff = GQCutoff
  on.exit(rm(list=c('.GeneScores', '.GSAresult', '.PPI',
                    '.Species','.alpha', '.GsQCutoff', '.GQCutoff'),
             envir=.GlobalEnv))

  shiny::runApp(
    appDir,
    display.mode ='normal'
  )
}
