# load library
library(shinydashboard)
library(shinyjs)
library(shinyCyJS)
library(shiny)

# Functions
Ovl <- function(a, b) {
  length(intersect(a, b)) / min(length(a), length(b))
}
DO <- function(a, b) {
  1 - Ovl(a, b)
}

DK <- function(a, b, l) {
  A <- length(intersect(a, b))
  B <- length(setdiff(a, b))
  C <- length(setdiff(b, a))
  D <- l - (A + B + C)

  O <- (A + D) / l
  E <- ((A + C) * (A + B) + (B + D) * (C + D)) / (l * l)
  k <- (O - E) / (1 - E)
  return(1 - k)
}

GetDO <- function(GsM) {
  d <- length(GsM)
  v <- matrix(0.001, d, d)
  for (i in 1:(d - 1)) {
    for (j in (i + 1):d) {
      v[i, j] <- v[j, i] <- DO(GsM[[i]], GsM[[j]])
    }
  }
  return(v)
}

GetDK <- function(GsM) {
  l <- length(unique(unlist(GsM)))
  d <- length(GsM)
  v <- matrix(0.001, d, d)
  for (i in 1:(d - 1)) {
    for (j in (i + 1):d) {
      v[i, j] <- v[j, i] <- DK(GsM[[i]], GsM[[j]], l)
    }
  }
  return(v)
}

GetDOP <- function(GsM, PPI, Alpha = 1) {
  d <- length(GsM)
  v <- matrix(0.001, d, d)

  DOP <- function(gs1, gs2, gsi1, gsi2, PPI, Alpha) {
    interact <- function(idx1, idx2, PPI, k = 1) {
      if (length(idx1) == 0 | length(idx2) == 0) {
        return(0)
      }
      return(sum(PPI[idx1, idx2]^(1 / k)))
    }

    score <- function(A, B, IA, IB, PPI, Alpha) {
      w <- min(length(A), length(B)) / (length(A) + length(B))
      a <- length(A)
      i <- length(intersect(A, B))
      uniqA <- setdiff(IA, IB)
      uniqB <- setdiff(IB, IA)
      common <- intersect(IA, IB)
      nom <- w * interact(uniqA, common, PPI) + interact(uniqA, uniqB, PPI)
      denom <- w * i + length(setdiff(B, A))
      1 - (i + Alpha * nom / denom) / a
    }
    s1 <- score(gs1, gs2, gsi1, gsi2, PPI, Alpha)
    s2 <- score(gs2, gs1, gsi2, gsi1, PPI, Alpha)

    return(max(min(s1, s2), 0.001))
  }

  GsMI <- IndexingGsM(GsM, rownames(PPI))

  for (i in 1:(d - 1)) {
    for (j in (i + 1):d) {
      v[i, j] <- v[j, i] <- DOP(GsM[[i]], GsM[[j]], GsMI[[i]], GsMI[[j]], PPI, Alpha)
    }
  }
  return(v)
}

GetLinked <- function(v, DistCutoff) {
  return(sort(which(v <= DistCutoff)))
}

GetLinkedRatio <- function(v, DistCutoff) {
  L <- nrow(v)
  if (L == 1) {
    return(0)
  }
  return((length(which(v < DistCutoff)) - L) / (L^2 - L)) # except Diag part ( -L )
}

GetUniq <- function(v) {
  return(v[which(!duplicated(v))])
}

RemoveIncluded <- function(v) {
  L <- length(v)
  LV <- sapply(1:L, function(i) {
    length(v[[i]])
  })
  r <- c() # remove
  for (i in 1:(L - 1)) {
    for (j in (i + 1):L) {
      if (Ovl(v[[i]], v[[j]]) == 1) {
        ifelse(LV[i] > LV[j],
          {
            r <- c(r, j)
          },
          {
            r <- c(r, i)
          }
        )
      }
    }
  }
  if (length(r)) {
    v <- v[-r]
  }
  return(v)
}

GetGenes <- function(Track, GM) {
  lapply(1:length(Track), FUN = function(i) {
    unique(unlist(GM[Track[[i]]], use.names = FALSE))
  })
}

RemoveSmallSeed <- function(v, SizeCutoff) {
  return(v[which(sapply(v, length) >= SizeCutoff)])
}

RemoveWeakRatio <- function(v, VarW, Dist, DistCutoff) {
  return(v[which(sapply(1:length(v), function(i) {
    GetLinkedRatio(Dist[v[[i]], v[[i]]], DistCutoff)
  }) >= VarW)])
}

MergeTrack <- function(v, Dist, VarM) {
  lapply(1:length(v), FUN = function(i) {
    sort(unique(unlist(v[which(Dist[i, ] < VarM)])))
  })
}

GetMin <- function(Dist) {
  min(sapply(1:nrow(Dist), FUN = function(i) {
    min(Dist[i, -i])
  }))
}

GetClust <- function(DistCutoff, MinSize, Dist, DistType, GM, Fuzzy = TRUE) {
  Dist2 <- Dist
  VarM <- DistCutoff
  VarX <- MinSize
  VarW <- 0.5

  Track <- list()

  for (i in 1:nrow(Dist)) {
    Track[[i]] <- GetLinked(Dist[i, ], DistCutoff)
  }

  Track <- RemoveSmallSeed(Track, VarX)
  if (length(Track) == 1) {
    return(Track)
  }
  Track <- RemoveWeakRatio(Track, VarW, Dist, DistCutoff)
  if (length(Track) == 1) {
    return(Track)
  }
  names(Track) <- NULL
  MinDist <- -9999

  while (MinDist <= VarM) {
    Track <- GetUniq(Track)
    if (length(Track) == 1) {
      return(Track)
    }
    Track <- RemoveIncluded(Track)
    if (length(Track) == 1) {
      return(Track)
    }
    TrackGenes <- GetGenes(Track, GM)

    if (DistType == 1) {
      Dist <- GetDO(TrackGenes)
    }
    if (DistType == 2) {
      Dist <- GetDOP(TrackGenes, PPI)
    }
    if (DistType == 3) {
      Dist <- GetDK(TrackGenes)
    }

    Track <- MergeTrack(Track, Dist, VarM)
    if (length(Track) == 1) {
      break
    }
    Track <- GetUniq(Track)
    if (length(Track) == 1) {
      break
    }
    Track <- RemoveIncluded(Track)
    if (length(Track) == 1) {
      break
    }
    TrackGenes <- GetGenes(Track, GM)

    if (DistType == 1) {
      Dist <- GetDO(TrackGenes)
    }
    if (DistType == 2) {
      Dist <- GetDOP(TrackGenes, PPI)
    }
    if (DistType == 3) {
      Dist <- GetDK(TrackGenes)
    }

    MinDist <- GetMin(Dist)
  }

  if (!Fuzzy) {
    ut <- unique(unlist(Track))
    DD <- Dist2[ut, ut]
    for (i in 1:nrow(DD)) {
      idxa <- which(DD[i, ] <= DistCutoff)
      idxb <- which(DD[i, ] > DistCutoff)
      DD[i, idxa] <- 1
      DD[i, idxb] <- 0
    }
    ct <- spectralClustering(DD, k = length(Track))

    Track2 <- list()
    for (i in 1:length(Track)) {
      Track2[[i]] <- ut[which(ct == i)]
    }
    return(Track2)
  }

  return(Track) # return index form
}

ToGsN <- function(i, GsN) {
  GsN[as.numeric(i)]
}

GetColors <- function(i) {
  sample(
    c(
      "#e6194B", "#3cb44b", "#ffe119", "#4363d8", "#f58231",
      "#911eb4", "#42d4f4", "#f032e6", "#bfef45", "#fabebe",
      "#469990", "#e6beff", "#9A6324", "#fffac8", "#800000",
      "#aaffc3", "#808000", "#ffd8b1", "#000075"
    ), i
  )
}

BuildBasicNodes <- function(ClustObj, Col) {
  v <- c()
  for (i in 1:length(ClustObj)) {
    v <- rbind(v, cbind(ClustObj[[i]], Col[i]))
  }
  as.data.frame(v, stringsAsFactors = FALSE)
}

GrayDuplicateNodes <- function(nodeData) {
  dIdx <- which(duplicated(nodeData[, 1]))
  dObj <- nodeData[dIdx, 1]
  nodeData <- nodeData[-dIdx, ]
  for (i in 1:length(dObj)) {
    nodeData[which(nodeData[, 1] == dObj[i]), 2] <- "#ffdd59"
  }
  nodeData
}

AddNameCol <- function(nodeData, GsN) {
  nodeData[, 1] <- sapply(nodeData[, 1], function(i) {
    ToGsN(i, GsN)
  })
  nodeData <- cbind(nodeData[, 1], nodeData)
  colnames(nodeData) <- c("id", "name", "color")
  nodeData
}

GetEdgeWidth <- function(i) {
  return(round(i / 0.111 + (10 - 1 / 0.111), 3))
}

GetClustDist <- function(ClustObj, Dist) {
  v <- sort(unique(unlist(ClustObj)))
  Dist <- Dist[v, v]
  rownames(Dist) <- colnames(Dist) <- ToGsN(v, GsN)
  Dist
}


StringSep <- function(x, Cv) {
  x <- strsplit(x, "")[[1]]
  v <- ""
  # Cv : Cut threshold : 20 for genemembers, 30 for genesetNames
  L <- length(x)
  for (i in 1:floor(L / Cv)) {
    v <- paste(v, paste(x[(1:Cv + Cv * (i - 1))], collapse = ""), "\n", sep = "")
  }

  if (i * Cv + 1 <= L) {
    v <- paste(v, paste(x[(i * Cv + 1):L], collapse = ""), collapse = "", sep = "")
  }
  return(v)
}

BuildDT <- function(cl, GsN, GsM, GsQ) {
  DF <- data.frame(matrix(ncol = 4, nrow = 0), stringsAsFactors = FALSE)
  colnames(DF) <- c("Cluster", "Geneset Name", "Geneset Qvalue", "Geneset member")
  for (i in 1:length(cl)) {
    ThisClust <- cl[[i]]
    for (j in 1:length(ThisClust)) {
      v <- ThisClust[j]
      ThisGsN <- GsN[v]
      if (nchar(ThisGsN) > 30) {
        ThisGsN <- StringSep(ThisGsN, 30)
      }
      ThisGsM <- paste(GsM[[v]], collapse = ", ")
      if (length(ThisGsM) > 30) {
        ThisGsM <- StringSep(ThisGsM, 30)
      }
      ThisGsN <- as.character(ThisGsN)
      ThisGsM <- as.character(ThisGsM)
      ThisGsQ <- as.numeric(GsQ[v])
      ThisClustNo <- i
      DF <- rbind(DF, cbind(ThisClustNo, ThisGsN, round(ThisGsQ, 5), ThisGsM))
    }
  }

  DF[, 3] <- as.numeric(as.character(DF[, 3])) # geneset qvalue
  DT::datatable(
    DF,
    extensions = "Buttons",
    filter = "top",
    rownames = FALSE,
    options = list(
      dom = "Bltipr",
      lengthChange = TRUE,
      pageLength = 50,
      autoWidth = TRUE,
      scrollX = TRUE,
      scrollY = TRUE,
      buttons = list(list(extend = "csv", text = "Download")),
      order = list(2, "asc")
    ),
    colnames = c("Cluster", "Name", "Qvalue", "Member"),
    selection = "none"
  )
}

UpdateNodeSearch <- function(session, ids) {
  updateSelectizeInput(session, inputId = "sel1", choices = ids, server = TRUE)
}

UpdateClusters <- function(session, ids) {
  updateSelectizeInput(session, inputId = "menuI", choices = c("Unselect", ids), server = TRUE)
}

GetHubGenes <- function(genes, PPI, PPICutoff, ScoreCutoff) {
  genes <- intersect(genes, rownames(PPI))
  genes <- intersect(genes, names(which(GS < ScoreCutoff)))
  tab <- PPI[genes, genes]

  d <- sapply(1:nrow(tab), function(i) {
    length(which(tab[i, ] >= PPICutoff / 1000))
  })
  names(d) <- rownames(tab)
  sort(d[which(rank(-d, ties.method = "min") <= 5)], decreasing = T)
}

BuildLegend <- function(colors) {
  res <- c()
  colors <- setdiff(unique(colors), "#666666")
  for (i in 1:length(colors)) {
    b <- HTML(
      paste0(
        '<div
        style="background:', colors[i],
        ';margin-bottom:0.3em;
        width:20px;
        height:20px;
        border-radius:50%;
        display:inline-block">
        </div>',
        '<div style="
        float:right;
        text-align:center;
        height:20px;
        line-height:20px;
        width:70px;">
        Cluster ',
        i, "</div><br>"
      )
    )
    res <- paste0(res, b)
  }
  return(HTML(res))
}

BuildEviLegend <- function() {
  res <- c()
  colors <- c("red", "orange", "yellow", "green", "blue", "navy")
  Evi <- c("Neighborhood", "Gene-fusion", "Cooccurence", "Coexpression", "Experiment", "Database")
  for (i in 1:length(colors)) {
    b <- HTML(
      paste0(
        '<div
        style="background:', colors[i],
        ';margin-bottom:0.3em;
        width:20px;
        height:20px;
        border-radius:50%;
        margin-right:0.5em;
        display:inline-block">
        </div>',
        '<div style="
        float:right;
        text-align:center;
        height:20px;
        line-height:20px;
        width:7em;">
        ',
        Evi[i], "</div><br>"
      )
    )
    res <- paste0(res, b)
  }
  return(HTML(res))
}

BuildQvLegend <- function() {
  res <- c()
  colors <- c("#30cc72", "#59d68e", "#82e0aa", "#FFFFFF") # LIGHT GREEN
  exp <- c("0", "0.05", "0.1", ">= 0.25")
  for (i in 1:length(colors)) {
    b <- HTML(
      paste0(
        '<div
        style="background:', colors[i], ';
        margin-bottom:0.3em;
        border:solid 1px gray;
        width:20px;
        height:20px;
        border-radius:50%;
        display:inline-block">
        </div>',
        '<div style="
        float:right;
        text-align:center;
        height:20px;
        line-height:20px;
        width:7em;">
        Q-value  ',
        exp[i], "</div><br>"
      )
    )
    res <- paste0(res, b)
  }
  return(HTML(res))
}

BuildGeneNetwork <- function(genes, PPICutoff = 0.7, PPI, ScoreCutoff) {
  source <- target <- c()

  # build edges
  genes <- intersect(genes, rownames(PPI))
  if (!is.null(GS)) {
    genes <- intersect(genes, names(which(GS < ScoreCutoff)))
  }

  if (length(genes) == 0) {
    showNotification("NO Genes with Cutoff", type = "error")
    return()
  }

  tab <- PPI[genes, genes]

  # EdgeData
  for (i in 1:nrow(tab)) {
    v <- names(which(tab[i, ] >= PPICutoff))
    if (length(v) > 0) {
      v <- v[which(v < genes[i])]
    } # alphabetically smaller
    if (length(v) > 0) {
      source <- c(source, rep(genes[i], length(v)))
      target <- c(target, v)
    }
  }

  color <- rep("#001c54", length(source)) # Edge color : gray , #666666, / pink , #fc7bee
  edgeData <- data.frame(source, target, color, stringsAsFactors = FALSE)

  if (length(source) == 0) {
    showNotification("NO Interactions with Cutoff", type = "error")
    return()
  }

  # build nodes
  name <- id <- unique(union(edgeData$source, edgeData$target))

  color <- rep("#F8EFBA", length(id)) # not genescore : yellow color
  if (!is.null(GS)) {
    color <- sapply(GS[id], function(i) {
      i <- as.numeric(i)
      rgb(860 * (0.25 / ScoreCutoff) * i, 400 * (0.25 / ScoreCutoff) * i + 155, 720 * (0.25 / ScoreCutoff) * i + 47, max = 255)
    })
  }

  href <- sapply(1:length(id), function(i) {
    GetGenecardURL(id[i])
  })
  nodeData <- data.frame(id, name, color, href, stringsAsFactors = FALSE)
  return(list(nodeData = nodeData, edgeData = edgeData))
}

ClearCy <- function() {
  shinyjs::delay( # DELAYED FUNCTION FOR AFTER CY DECLARED
    ms = 2000,
    expr = {
      js$SetClickNode()
      js$SetSoftZoom()
      js$defineColorMap()
    }
  )
}

BuildMultiHub <- function(cl, GsM, PPI, PPICutoff, ScoreCutoff) {
  res <- data.frame(matrix(ncol = 3, nrow = 0), stringsAsFactors = FALSE)
  for (i in 1:length(cl)) {
    hubs <- GetHubGenes(unique(unlist(GsM[cl[[i]]])), PPI, PPICutoff, ScoreCutoff)
    res <- rbind(
      res,
      data.frame(
        Gene = names(hubs),
        Degree = unname(hubs),
        Cluster = i,
        stringsAsFactors = FALSE
      )
    )
  }
  k <- sort(unique(res[which(duplicated(res["Gene"])), "Gene"]))
  kk <- c()
  for (i in 1:length(k)) {
    kk <- c(kk, which(res["Gene"] == k[i]))
  }
  rownames(res) <- NULL

  DT::datatable(
    res[kk, ],
    colnames = c("Gene", "Degree", "Cluster"),
    options = list(dom = "t"),
    rownames = FALSE,
    selection = "single"
  )
}

RenderGeneNetwork <- function(genes, output, PPICutoff, PPI, ScoreCutoff, session) {
  nobj <- BuildGeneNetwork(genes, PPICutoff, PPI, ScoreCutoff)
  if (is.null(nobj)) {
    return(0)
  }

  elem <- list()

  nodes <- nobj$nodeData

  for (i in 1:nrow(nodes)) {
    elem[[length(elem) + 1]] <-
      buildNode(
        id = nodes[i, 1], bgColor = nodes[i, 3], labelColor = "black",
        height = 50, width = 50, tooltip = nodes[i, 4], textOutlineColor = "white", textOutlineWidth = "1"
      )
  }

  edges <- nobj$edgeData
  for (i in 1:nrow(edges)) {
    elem[[length(elem) + 1]] <-
      buildEdge(source = edges[i, 1], target = edges[i, 2], lineColor = "#fc7bee")
  }

  UpdateNodeSearch(session, sort(nobj$nodeData[, "id"]))

  output$cy <- renderShinyCyJS(shinyCyJS(elem, layout = list(name = "cola", nodeSpacing = 3, edgeLength = 250, animate = TRUE, randomize = TRUE, maxSimulationTime = 3000)))

  return(1)
}

GetColorEdge <- function(genes, l, color) {
  res <- l[genes]
  for (i in 1:length(res)) {
    res[[i]] <- intersect(res[[i]], genes)
  }

  res <- res[which(sapply(res, length) != 0)]
  v <- data.frame(matrix(ncol = 3, nrow = 0), stringsAsFactors = FALSE)

  if (length(res)) {
    for (i in 1:length(res)) {
      A <- res[[i]]
      for (j in 1:length(A)) {
        v <- rbind(v, cbind(names(res)[i], A[j], color))
      }
    }

    colnames(v) <- c("source", "target", "color")

    return(v)
  }
  return(0)
}

GetDisease <- function(g) {
  res <- as.data.frame(table(unlist(D[g])))
  res <- res[which(res[, 2] > 3), ]
  res
}

SendError <- function(session, title, text) {
  sendSweetAlert(session = session, title = , text = )
}

GetGenecardURL <- function(gene) {
  as.character(shiny::a(
    target = "_blank",
    href = paste0("http://www.genecards.org/cgi-bin/carddisp.pl?gene=", gene), gene
  ))
}

percentRank <- function(arr, n) {
  L <- S <- 0
  for (i in 1:length(arr)) {
    if (arr[i] < n) {
      L <- L + 1
    }
    else {
      if (arr[i] == n) {
        S <- S + 1
      }
    }
  }
  (L + 0.5 * S) / length(arr)
}
# object handling Functions

GetGenesetName <- function(tab) {
  toupper(gsub(" ", "_", tab[, 1]))
}
GetGenesetQvalue <- function(tab) {
  tab[, 3]
}
GetGenesetMember <- function(tab) {
  lapply(1:nrow(tab), function(i) {
    strsplit(tab[i, 2], " ")[[1]]
  })
}
GetGenesetDirection <- function(tab) {
  tab[, 4]
}
GetGeneScore <- function(tab) {
  res <- tab[, 2]
  names(res) <- tab[, 1]
  res
}
ReadDiseaseFile <- function() {
  load("DisGeNet.RData")
  return(D)
}

#### DEMO DATAS

# if sample : both genescore and gsaresult not exist
if (is.null(.GeneScores) & is.null(.GSAresult)) {
  print("run example data")
  .GSAresult <- GSAresult <- read.delim("sample_geneset.txt", header = TRUE, stringsAsFactors = FALSE) # Name, Genes, Qvalue
  .GeneScores <- GeneScores <- read.delim("sample_genescore.txt", header = TRUE, stringsAsFactors = FALSE) # Gene, Score
}

# if gsaresult only exist
if (!is.null(.GSAresult) & is.null(.GeneScores)) {
  GSAresult <- .GSAresult
  GeneScores <- NULL
} else if (!is.null(.GSAresult) & !is.null(.GeneScores)) { # if both file exist
  GSAresult <- .GSAresult
  GeneScores <- .GeneScores
}

IndexingGsM <- function(GsM, rp) {
  sapply(1:length(GsM), function(i) {
    unname(unlist(sapply(
      GsM[[i]],
      function(j) {
        a <- which(j == rownames(PPI))
        if (length(a)) {
          a
        }
      }
    ), use.names = FALSE))
  })
}



if (!is.null(.PPI)) { # not string PPI, no use btn4;
  UseString <- FALSE
  PPI <- .PPI
} else {
  print("PPI table is not given, STRING data is used.")
  if (.Species == "A") {
    DirName <- "arabidopsis/"
  }
  if (.Species == "C") {
    DirName <- "celegans/"
  }
  if (.Species == "E") {
    DirName <- "ecoli/"
  }
  if (.Species == "F") {
    DirName <- "fly/"
  }
  if (.Species == "H") {
    D <- ReadDiseaseFile()
    DirName <- "human/"
  }
  if (.Species == "I") {
    DirName <- "rice/"
  }
  if (.Species == "M") {
    DirName <- "mouse/"
  }
  if (.Species == "R") {
    DirName <- "rat/"
  }
  if (.Species == "Y") {
    DirName <- "yeast/"
  }
  if (.Species == "Z") {
    DirName <- "zebrafish/"
  }

  Files <- paste0(DirName, paste0("l", 1:6), ".RData")

  if (!file.exists(Files[1])) {
    DownloadData(species = strsplit(DirName, "/")[[1]])
  }

  for (i in 1:6) {
    load(Files[i])
  }
  load(paste0(DirName, "string.RData"))

  PPI <- string
  if (.Species == "H") {
    PPI <- as.matrix(PPI)
  }
  rm(string)
  UseString <- TRUE
}



GsN <- GetGenesetName(GSAresult) # Geneset Name
GsQ <- GetGenesetQvalue(GSAresult) # Geneset Qvalue
GsM <- GetGenesetMember(GSAresult) # Geneset Member

GS <- GeneScores # assign null value
if (!is.null(GeneScores)) {
  GS <- GetGeneScore(GeneScores)
}


IsGsD <- FALSE
if (ncol(GSAresult) == 4) { # Direction
  GsD <- GetGenesetDirection(GSAresult)
  IsGsD <- TRUE
}

# Cutoff by GsQCutoff
Idx <- which(GsQ <= .GsQCutoff)

GsN <- GsN[Idx]
GsQ <- GsQ[Idx]
GsM <- GsM[Idx]
if (IsGsD) {
  GsD <- GsD[Idx]
}


ui <- function() {
  dashboardPage(
    title = "GScluster",
    header = dashboardHeader(title = "GScluster"),
    sidebar = dashboardSidebar(
      sidebarMenu(
        menuItem(
          text = "Clustering Results",
          tabName = "menu2"
        ), # Table
        menuItem(
          text = "Network Plot",
          tabName = "menu1",
          selected = TRUE
        ), # Network
        div(
          style = "z-index:999;",
          id = "GENENETWORK_OPTIONS",
          selectizeInput(
            inputId = "menuI",
            label = "Select Gene-set Cluster",
            choices = "NOT BUILT YET",
            selected = NULL,
            width = "auto"
          ),
          div(
            id = "PPICutoffContainer",
            style = "display:none;",
            sliderInput(
              inputId = "PPICutoff",
              label = "PPI Cutoff: ",
              min = 0,
              max = 999,
              value = 700,
              step = 10
            )
          ),
          actionButton(
            inputId = "DRAW_GENENETWORK",
            label = "Show PPI",
            style = "z-index:999;width:7em;float:left;display:none;"
          )
        ),
        actionButton(inputId = "btn2", label = "Go Back", style = "z-index:999;width:7em;display:none;margin-top:-40px;float:right;")
      )
    ),
    body = dashboardBody(
      useShinyjs(),
      tags$head(tags$script(src = "cytoscape-panzoom.js")),
      tags$link(rel = "stylesheet", type = "text/css", href = "cytoscape.js-panzoom.css"),
      tags$head(tags$script(src = "cytoscape-cy-svg-convertor.js")),
      tags$head(tags$script(src = "cytoscape-svg-convertor.js")),
      tags$head(tags$script(src = "svg.min.js")),
      tags$head(tags$script(src = "additional_script.js")),

      extendShinyjs(script = "shinyjs.js"), # change as scripts;

      div(id = "create", display = "none"), # EMPTY DIV FOR DOWNLOAD SVG
      tags$head(
        tags$style(
          HTML("
            /* logo */
            .skin-blue .main-header .logo { background-color: #2f9193; }

            /* logo when hovered */
            .skin-blue .main-header .logo:hover { background-color: #2f9193; }

            /* body */
            .content-wrapper, .right-side { background-color:white; }

            /* Header Bar */
            .skin-blue .main-header .navbar { background-color: #44c1c4; }

            /* active selected tab in the sidebarmenu */
            .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
              background-color: #000a2d;
              color : #ffffff;
            }

            /* other links in the sidebarmenu when hovered */
            .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
              background-color: #000a2d;
              color : #ffffff;
            }

            /* other links in the sidebarmenu */
            .skin-blue .main-sidebar .sidebar .sidebar-menu a{
              background-color: #001b54;
              color: #ffffff;
            }

            /* Active Sidebar Tail*/
            .skin-blue .sidebar-menu>li.active>a, .skin-blue .sidebar-menu>li:hover>a {
            border-left-color : #f1592a;
            }

            /* main sidebar */
            .skin-blue .main-sidebar {
              background-color: #001b54;
              color: #000000;
            }

            /* subMenu opened */
            .skin-blue .sidebar-menu>li>.treeview-menu{ background : #143e41; }

            /* Datatable button Position*/
            .dataTables_wrapper .dt-buttons { margin-right:2em; }

            ")
        )
      ),
      tags$style(type = "text/css", "html, body {min-height: 100%}"),
      tags$style(type = "text/css", "height: 100%; background:'white'"),

      tabItems(
        tabItem(
          tabName = "menu1",
          actionButton(
            inputId = "btn3",
            label = "Download Graph",
            style = "position:absolute; z-index:9999;right:1em;top:0.5em;"
          ),
          actionButton(
            inputId = "btn10",
            label = "Graph Layout",
            style = "position:absolute; z-index:9999;right:11em;top:0.5em;"
          ),
          actionButton( # 1em to 3em --> 5 ~ 15
            inputId = "btn11",
            label = "Graph Size",
            style = "position:absolute; z-index:9999;right:19.5em;top:0.5em;"
          ),
          actionButton(
            inputId = "btn12",
            label = "Clustering Method",
            style = "position:absolute; z-index:9999;right:26.5em;top:0.5em;"
          ),
          actionButton(
            inputId = "btn13",
            label = "Find",
            style = "position:absolute; z-index:9999;left:43em;top:0.5em;"
          ),
          div(
            style = "position:absolute; z-index:9999;left:21em;top:0.5em;",
            selectInput(
              inputId = "sel1",
              label = NULL,
              choices = NULL,
              selected = NULL
            )
          ),
          fluidRow(
            width = 12,
            id = "DivContainOpt3",
            style = "border: 2px solid rgb(0, 27, 84);
              padding: 1em 1em 1em;
              z-index: 9999;
              position: absolute;
              background: white;
              top: 3em;
              right: 20em;
              display: none;",
            selectInput(
              inputId = "sel2",
              label = "Distance",
              choices = c("pMM", "MM", "Kappa")
            ),
            numericInput(
              inputId = "alpha",
              label = "Network weight α (for pMM only)",
              value = 1, min = 0, max = 1, step = 0.05
            ),
            sliderInput(inputId = "num1", label = "Minimum Seed Size", value = 3, min = 3, max = 8, step = 1),
            numericInput(inputId = "num2", label = "Merging Threshold", value = 0.5, step = 0.05, min = 0.1, max = 0.9),
            textOutput("txt1"),
            actionButton(inputId = "btn8", label = "Apply", style = "margin-left:17em")
          ),
          fluidRow(
            width = 12,
            id = "DivContainOpt2",
            style =
              "border: 2px solid rgb(0, 27, 84);
            padding: 1em 1em 1em;
            z-index: 9999;
            position: absolute;
            background: white;
            top: 3em;
            right: 11em;
            display: none;",
            sliderInput(inputId = "sld1", label = "Node Label Size (px)", min = 16, max = 32, value = 16, step = 4),
            actionButton(inputId = "btn6", label = "Apply"),
            sliderInput(inputId = "sld2", label = "Node Size (px)", min = 30, max = 120, value = 60, step = 30),
            actionButton(inputId = "btn18", label = "Apply")
          ),
          fluidRow(
            width = 12,
            id = "DivContainOpt1",
            style =
              "border: 2px solid rgb(0, 27, 84);
            padding: 1em 1em 1em;
            z-index: 9999;
            position: absolute;
            background: white;
            top: 3em;
            right: 11em;
            display: none;",
            radioButtons(
              inputId = "rad1",
              label = "",
              choices = c("Circle", "Cola", "Dagre", "Klay", "Spread"),
              selected = "Cola",
              inline = TRUE
            ),
            actionButton(
              inputId = "btn5",
              label = "Apply"
            )
          ),
          actionButton(
            inputId = "btn4",
            label = "PPI Evidence",
            style = "position:absolute; z-index:9999;right:26.5em;top:0.5em;display:none;"
          ),
          fluidRow(
            width = 12,
            id = "DivContainOpt4",
            style =
              "border: 2px solid rgb(0, 27, 84);
            padding: 1em 1em 1em;
            z-index: 9999;
            position: absolute;
            background: white;
            top: 3em;
            right: 26em;
            width :10em;
            display: none;",
            shinyWidgets::checkboxGroupButtons(
              inputId = "che1",
              label = "",
              direction = "vertical",
              size = "sm",
              choices = c(
                `<i class='fas fa-circle' style='color:red'></i>&nbsp;Neighborhood&nbsp;` = "Neighborhood",
                `<i class='fas fa-circle' style='color:orange'></i>&nbsp;Gene-fusion&nbsp;` = "Gene-fusion",
                `<i class='fas fa-circle' style='color:yellow'></i>&nbsp;Cooccurence&nbsp;` = "Cooccurence",
                `<i class='fas fa-circle' style='color:green'></i>&nbsp;Coexpression&nbsp;` = "Coexpression",
                `<i class='fas fa-circle' style='color:blue'></i>&nbsp;Experiment&nbsp;` = "Experiment",
                `<i class='fas fa-circle' style='color:navy'></i>&nbsp;Database&nbsp;` = "Database"
              )
            ),

            actionButton(inputId = "btn14", label = "Apply", style = "margin-left:auto")
          ),

          actionButton( # HUB IN GENESETS
            inputId = "btn15",
            label = "Hubs",
            style = "position:absolute; z-index:9999;left:1em;margin-top:37em;width:14em;"
          ),
          fluidRow(
            width = 12,
            id = "DivContainOpt5",
            style = "border : 2px solid rgb(0, 27, 84);
            padding : 1em;
            z-index : 9999;
            position : absolute;
            background : white;
            top : 41em;
            left : 19em;
            width : 12em;
            display: none;",
            actionButton(inputId = "btn16", label = "Gene-set Hub", style = "width:10em;"), # GENESET Hub
            actionButton(inputId = "btn17", label = "Gene Hub", style = "width:10em;"), # GENE Hub
            actionButton(inputId = "RenderTab2", label = "Multi-cluster Hub", style = "width:10em;") # MULTI Hub
          ),

          actionButton(
            inputId = "RenderPlot1",
            label = "WordCloud",
            style = "position:absolute; z-index:9999;right:1em;margin-top:1em;display:none;"
          ), # Wordcloud
          fluidRow(
            width = 12,
            id = "DivContainPlot1",
            style = "padding-right:1em;
            z-index:-1;
            position:absolute;
            margin-top:10em;
            right:0.5em;",
            wordcloud2::wordcloud2Output(
              outputId = "plot1",
              width = "400px",
              height = "400px"
            ),
            actionButton(inputId = "ClearPlot1", label = "Clear", style = "float:right;display:none;")
          ),

          fluidRow(
            width = 4,
            id = "DivContainTab2",
            style = "padding-right:1em;
            z-index:999;
            text-align:center;
            position:absolute;
            margin-top:6em;
            right:0.5em;",
            DT::dataTableOutput(outputId = "tab2"),
            actionButton(inputId = "ClearTab2", label = "Hide Tab", style = "float:right;display:none;margin-top:1em;")
            # actionButton(inputId='ClearTabNode', label = 'Unselect Nodes', style='float:right;display:none;margin-top:1em;')
          ),
          uiOutput(
            style = {
              "position:absolute;margin-top:17em;z-index:999"
            },
            outputId = "legend"
          ),
          p(
            style = {
              "position:absolute;bottom:5em;z-index:999;"
            },
            imageOutput(outputId = "img1", inline = TRUE, width = "300px")
          ),
          uiOutput(
            style = {
              "position:absolute;bottom : 5em; margin-left:7em;z-index:999;"
            },
            outputId = "legend2"
          ),
          div(
            id = "CYCONTAINER",
            shinycssloaders::withSpinner(
              ShinyCyJSOutput(outputId = "cy", height = "100%"),
              proxy.height = "600px"
            )
          )
        ),
        tabItem(
          tabName = "menu2",
          DT::dataTableOutput(outputId = "tab1")
        ),
        tabItem(
          tabName = "menu3"
        )
      )
    )
  )
}

server <- function(input, output, session) {
  if (!UseString) {
    shinyjs::disable("btn4")
  }
  if (.Species != "H") {
    shinyjs::disable("RenderPlot1")
  }
  js$SetHeight()
  shinyjs::disable("btn17")

  BuildNetworkObj <- function(ClustObj, GsN, Dist, MinEdge, Fuzzy = TRUE) {
    Col <- rep("#053190", length(ClustObj))
    nodeData <- BuildBasicNodes(ClustObj, Col)
    nodeData <- AddNameCol(nodeData, GsN)
    Color <- sapply(
      GsQ[sapply(nodeData[, "id"], function(i) {
        which(i == GsN)
      })],
      function(i) {
        rgb(
          red = 880 * (0.25 / .GsQCutoff) * i,
          green = 880 * (0.25 / .GsQCutoff) * i,
          blue = 228 * (0.25 / .GsQCutoff) * i + 198, max = 255
        )
      }
    )
    nodeData[, "color"] <- Color
    if (IsGsD) {
      DN <- GsN[which(GsD == "DN")]
      Idx <- unlist(sapply(DN, function(i) {
        which(i == nodeData[, 1])
      }))
      Color <- sapply(
        GsQ[sapply(nodeData[Idx, "id"], function(i) {
          which(i == GsN)
        })],
        function(i) {
          rgb(800 * (0.25 / .GsQCutoff) * i + 22, 388 * (0.25 / .GsQCutoff) * i + 158, 744 * (0.25 / .GsQCutoff) * i + 48, max = 255)
        }
      )
      nodeData[Idx, "color"] <- Color # '#218E3D' # Sora GREEN COLOR
      UP <- GsN[which(GsD == "UP")]
      Idx <- unlist(sapply(UP, function(i) {
        which(i == nodeData[, 1])
      }))
      Color <- sapply(
        GsQ[sapply(nodeData[Idx, "id"], function(i) {
          which(i == GsN)
        })],
        function(i) {
          rgb(-16 * (0.25 / .GsQCutoff) * i + 255, 920 * (0.25 / .GsQCutoff) * i, 936 * (0.25 / .GsQCutoff) * i, max = 255)
        }
      )
      nodeData[Idx, "color"] <- Color # '#E81548' # Sora Red COLOR
    }

    Dist <- GetClustDist(ClustObj, Dist)
    source <- target <- w <- c()
    if (Fuzzy) {
      for (i in 1:nrow(Dist)) {
        v <- which(Dist[i, -i] <= MinEdge) # Strong Edges only
        if (length(v) > 0) {
          v <- v[which(names(v) < rownames(Dist)[i])]
        } # One-side Edge : Name Ordered
        if (length(v) > 0) {
          nv <- names(v)
          source <- c(source, rep(rownames(Dist)[i], length(v)))
          target <- c(target, nv)
          w <- c(w, sapply(Dist[i, v], GetEdgeWidth))
        }
      }
    }
    else {
      for (i in 1:length(ClustObj)) {
        nv <- GsN[ClustObj[[i]]]

        if (length(nv) < 1) {
          next
        }

        for (j in 1:length(nv)) {
          for (k in 1:length(nv)) {
            if (nv[j] > nv[k] & Dist[nv[j], nv[k]] <= MinEdge) {
              source <- c(source, nv[j])
              target <- c(target, nv[k])
              w <- c(w, sapply(Dist[nv[j], nv[k]], GetEdgeWidth))
            }
          }
        }
      }
    }
    edgeData <- data.frame(source, target, stringsAsFactors = FALSE)
    return(list(nodeData = nodeData, edgeData = edgeData, color = Col, width = w))
  }

  RenderGeneSetNetwork <- function(cl, GsN, v, output, session, DC, Fuzzy = TRUE) {
    nobj <- BuildNetworkObj(cl, GsN, v, DC, Fuzzy)

    elem <- list()
    nodes <- nobj$nodeData
    for (i in 1:nrow(nodes)) {
      elem[[length(elem) + 1]] <-
        buildNode(id = nodes[i, 1], bgColor = nodes[i, 3], labelColor = "black", height = 30, width = 30)
    }
    edges <- nobj$edgeData
    for (i in 1:nrow(edges)) {
      elem[[length(elem) + 1]] <-
        buildEdge(source = edges[i, 1], target = edges[i, 2], lineColor = "#001c54", width = 1)
    }

    output$cy <- renderShinyCyJS(shinyCyJS(elem, layout = list(name = "cola", nodeSpacing = 3, edgeLength = 250, animate = TRUE, randomize = TRUE, maxSimulationTime = 3000)))
    UpdateNodeSearch(session, sort(nobj$nodeData[, "id"]))
    UpdateClusters(session, 1:length(cl))
    ClearCy()
  }

  ScoreCutoff <- .GQCutoff
  # DEFAULT CLUSTER = > pMM, 0.5 of MM, 3, alpha = 1


  v <- GetDOP(GsM, PPI, .alpha)
  DC <- unname(quantile(v, percentRank(as.numeric(GetDO(GsM)), 0.5)))
  DC2 <- unname(quantile(GetDK(GsM), percentRank(as.numeric(GetDO(GsM)), 0.5)))

  output$txt1 <- renderText(paste("pMM : ", round(DC, 4), "= MM : ", 0.5, "= Kappa : ", round(DC2, 4), collapse = ""))

  updateNumericInput(session, "num2", label = "Maximum gene-set distance", value = as.numeric(DC), step = 0.05, min = 0.1, max = 0.9)
  cl <- GetClust(DistCutoff = DC, MinSize = 3, Dist = v, DistType = 2, GM = GsM, Fuzzy = .Fuzzy)
  RenderGeneSetNetwork(cl, GsN, v, output, session, DC, Fuzzy = .Fuzzy)
  tab <- BuildDT(cl, GsN, GsM, GsQ)
  output$tab1 <- DT::renderDataTable(tab, server = FALSE)
  ClearCy()
  if (IsGsD) {
    output$img1 <- renderImage(
      {
        list(src = "grscale.png", contentType = "image/png")
      },
      deleteFile = FALSE
    )
  }
  else {
    output$img1 <- renderImage(
      {
        list(src = "bscale.png", contentType = "image/png")
      },
      deleteFile = FALSE
    )
  }

  # GENE NETWORK
  observeEvent(input$DRAW_GENENETWORK, {
    if (input$menuI != "Unselected") {
      i <- as.numeric(input$menuI)
      genes <- unique(unlist(GsM[cl[[i]]]))
      v <- RenderGeneNetwork(genes, output, input$PPICutoff / 1000, PPI, ScoreCutoff, session)

      if (v) { # Success

        ClearCy()
        shinyjs::hide("img1")
        if (!is.null(GS)) {
          output$img1 <- renderImage(
            {
              list(src = "gscale.png", contentType = "image/png")
            },
            deleteFile = FALSE
          )
          shinyjs::show("img1")
        }

        shinyjs::enable("btn17")
        shinyjs::showElement("btn4")
        shinyjs::showElement("btn2")
        shinyjs::showElement("RenderPlot1")
        shinyjs::hideElement("RenderTab2")
        shinyjs::hideElement("btn12")
        shinyjs::hideElement("btn16")

        shinyjs::hide("DivContainOpt1")
        shinyjs::hide("DivContainOpt2")
        shinyjs::hide("DivContainOpt3")
        shinyjs::hide("DivContainOpt4")
        shinyjs::hide("DivContainOpt5")
      }
    }
  })

  # BACKTO BUILT GENESET NETWORK
  observeEvent(input$btn2, {
    nobj <- BuildNetworkObj(cl, GsN, v, input$num2)

    elem <- list()
    nodes <- nobj$nodeData
    for (i in 1:nrow(nodes)) {
      elem[[length(elem) + 1]] <-
        buildNode(id = nodes[i, 1], bgColor = nodes[i, 3], labelColor = "black", height = input$sld2, width = input$sld2, fontSize = input$sld1)
    }
    edges <- nobj$edgeData
    for (i in 1:nrow(edges)) {
      elem[[length(elem) + 1]] <-
        buildEdge(source = edges[i, 1], target = edges[i, 2], lineColor = "#001c54")
    }

    output$cy <- renderShinyCyJS(shinyCyJS(elem, layout = list(name = "cola", nodeSpacing = 3, edgeLength = 250, animate = TRUE, randomize = TRUE, maxSimulationTime = 3000)))

    UpdateNodeSearch(session, sort(nobj$nodeData[, "id"]))
    UpdateClusters(session, 1:length(cl))
    ClearCy()

    if (IsGsD) {
      output$img1 <- renderImage(
        {
          list(src = "grscale.png", contentType = "image/png")
        },
        deleteFile = FALSE
      )
    }
    else {
      output$img1 <- renderImage(
        {
          list(src = "bscale.png", contentType = "image/png")
        },
        deleteFile = FALSE
      )
    }

    tab <- BuildDT(cl, GsN, GsM, GsQ)
    output$tab1 <- DT::renderDataTable(tab, server = FALSE)
    shinyjs::hideElement("btn4")
    shinyjs::hideElement("RenderPlot1")
    shinyjs::showElement("RenderTab2")
    shinyjs::showElement("btn12")
    shinyjs::hideElement("legend2")
    shinyjs::disable("btn17")
    shinyjs::hideElement("btn2")
    shinyjs::showElement("btn16")
  })

  # Multi-Hub : find high ranked genes around all cluster, and return their intersected

  # Render Multihub
  observeEvent(input$RenderTab2, {
    shinyjs::showElement("DivContainTab2")
    shinyjs::showElement("ClearTab2", anim = "slide", time = 0.5)
    shinyjs::hideElement("DivContainOpt5")

    output$tab2 <- DT::renderDataTable(BuildMultiHub(cl, GsM, PPI, input$PPICutoff, ScoreCutoff))
  })

  observeEvent(input$tab2_rows_selected, {
    js$HighlightTab()
  })

  # Render Wordcloud
  observeEvent(input$RenderPlot1, {
    shinyjs::showElement("DivContainPlot1", anim = "slide", time = 0.5)
    shinyjs::showElement("ClearPlot1", anim = "slide", time = 0.5)
    shinyjs::hideElement("RenderPlot1", anim = "slide", time = 0.5)
    shinyjs::runjs("$('#DivContainPlot1').css('z-index',1)")
    i <- as.numeric(input$menuI)
    res <- GetDisease(unique(unlist(GsM[cl[[i]]])))
    wc <- wordcloud2(res)
    output$plot1 <- renderWordcloud2(wc)
  })

  # Layout Sidebar
  observeEvent(input$btn5, {
    if (input$rad1 == "Cola") {
      js$ColaLayout()
    }
    if (input$rad1 == "Circle") {
      js$CircleLayout()
    }
    if (input$rad1 == "Dagre") {
      js$DagreLayout()
    }
    if (input$rad1 == "Klay") {
      js$KlayLayout()
    }
    if (input$rad1 == "Spread") {
      js$SpreadLayout()
    }
    shinyjs::hideElement("DivContainOpt1")
  })

  # Hide Div After Option selected
  observeEvent(input$btn10, {
    shinyjs::toggleElement("DivContainOpt1", anim = "slide", time = 0.5)
    shinyjs::hide("DivContainOpt2")
    shinyjs::hide("DivContainOpt3")
    shinyjs::hide("DivContainOpt4")
    shinyjs::hide("DivContainOpt5")
  })
  observeEvent(input$btn11, {
    shinyjs::toggleElement("DivContainOpt2", anim = "slide", time = 0.5)
    shinyjs::hide("DivContainOpt1")
    shinyjs::hide("DivContainOpt3")
    shinyjs::hide("DivContainOpt4")
    shinyjs::hide("DivContainOpt5")
  })
  observeEvent(input$btn12, {
    shinyjs::toggleElement("DivContainOpt3", anim = "slide", time = 0.5)
    shinyjs::hide("DivContainOpt1")
    shinyjs::hide("DivContainOpt2")
    shinyjs::hide("DivContainOpt4")
    shinyjs::hide("DivContainOpt5")
  })

  # FontSize
  observeEvent(input$btn6, {
    js$SetFontSize(input$sld1)
    shinyjs::hideElement("DivContainOpt2")
    js$CyFit()
  })

  # Clear Multihub
  observeEvent(input$ClearTab2, {
    shinyjs::hideElement("DivContainTab2")
  })

  # Clear Wordcloud
  observeEvent(input$ClearPlot1, {
    shinyjs::hideElement("DivContainPlot1")
    shinyjs::showElement("RenderPlot1")
    shinyjs::runjs("$('#DivContainPlot1').css('z-index',-1)")
  })

  # NodeSearch
  observeEvent(input$btn13, {
    v <- paste0("#", input$sel1)
    js$SetNode(v)
    idx <- which(input$sel1 == GsN)
    res <- c()
    for (i in 1:length(cl)) {
      if (idx %in% cl[[i]]) {
        res <- c(res, i)
      }
    }
    if (length(res) > 1) {
      res <- paste0(res, collapse = ",")
    }

    js$IndicateCluster(c(v, res))
    js$CheckNodeHigh(v)
    shinyjs::delay(ms = 2000, expr = {
      js$CheckNodeHigh(v)
    })
    shinyjs::delay(ms = 4000, expr = {
      js$CheckNodeHigh(v)
    })
    shinyjs::delay(ms = 6000, expr = {
      js$CheckNodeHigh(v)
    })
    shinyjs::delay(ms = 8000, expr = {
      js$CheckNodeHigh(v)
      js$removeCluster()
    })
  })

  # download network svg form
  observeEvent(input$btn3, {
    js$download()
  })

  # ClusterOption changed
  observeEvent(input$btn8, {
    if (input$sel2 == "MM") {
      v <- GetDO(GsM)
      cl <<- GetClust(input$num2, input$num1, v, 1, GsM, Fuzzy = .Fuzzy)
    }
    if (input$sel2 == "pMM") {
      v <- GetDOP(GsM, PPI, input$alpha)
      cl <<- GetClust(input$num2, input$num1, v, 2, GsM, Fuzzy = .Fuzzy)
    }
    if (input$sel2 == "Kappa") {
      v <- GetDK(GsM)
      cl <<- GetClust(input$num2, input$num1, v, 3, GsM, Fuzzy = .Fuzzy)
    }

    nobj <- BuildNetworkObj(cl, GsN, v, input$num2)

    elem <- list()
    nodes <- nobj$nodeData
    for (i in 1:nrow(nodes)) {
      elem[[length(elem) + 1]] <-
        buildNode(id = nodes[i, 1], bgColor = nodes[i, 3], labelColor = "black", height = input$sld2, width = input$sld2, fontSize = input$sld1)
    }
    edges <- nobj$edgeData
    for (i in 1:nrow(edges)) {
      elem[[length(elem) + 1]] <-
        buildEdge(source = edges[i, 1], target = edges[i, 2], lineColor = "#001c54")
    }

    output$cy <- renderShinyCyJS(shinyCyJS(elem, layout = list(name = "cola", nodeSpacing = 3, edgeLength = 250, animate = TRUE, randomize = TRUE, maxSimulationTime = 3000)))


    UpdateNodeSearch(session, sort(nobj$nodeData[, "id"]))
    UpdateClusters(session, 1:length(cl))
    ClearCy()

    tab <- BuildDT(cl, GsN, GsM, GsQ)
    output$tab1 <- DT::renderDataTable(tab, server = FALSE)

    shinyjs::hideElement("DivContainOpt3")
  })

  observeEvent(input$btn14, { # Coloring Gene network Edge
    shinyjs::hideElement("DivContainOpt4")

    i <- as.numeric(input$menuI)
    genes <- unique(unlist(GsM[cl[[i]]]))
    genes <- intersect(genes, names(which(GS < ScoreCutoff)))
    E1 <- E2 <- E3 <- E4 <- E5 <- E6 <- 0
    if ("Neighborhood" %in% input$che1) E1 <- GetColorEdge(genes, l1, "red")
    if ("Gene-fusion" %in% input$che1) E2 <- GetColorEdge(genes, l2, "orange")
    if ("Cooccurence" %in% input$che1) E3 <- GetColorEdge(genes, l3, "yellow")
    if ("Coexpression" %in% input$che1) E4 <- GetColorEdge(genes, l4, "green")
    if ("Experiment" %in% input$che1) E5 <- GetColorEdge(genes, l5, "blue")
    if ("Database" %in% input$che1) E6 <- GetColorEdge(genes, l6, "navy")

    nobj <- BuildGeneNetwork(genes, input$PPICutoff / 1000, PPI, ScoreCutoff)

    suppressWarnings({
      if (E1 != 0) {
        nobj$edgeData <- rbind(nobj$edgeData, E1)
      }
      if (E2 != 0) {
        nobj$edgeData <- rbind(nobj$edgeData, E2)
      }
      if (E3 != 0) {
        nobj$edgeData <- rbind(nobj$edgeData, E3)
      }
      if (E4 != 0) {
        nobj$edgeData <- rbind(nobj$edgeData, E4)
      }
      if (E5 != 0) {
        nobj$edgeData <- rbind(nobj$edgeData, E5)
      }
      if (E6 != 0) {
        nobj$edgeData <- rbind(nobj$edgeData, E6)
      }
    })

    elem <- list()

    nodes <- nobj$nodeData

    for (i in 1:nrow(nodes)) {
      elem[[length(elem) + 1]] <-
        buildNode(
          id = nodes[i, 1], bgColor = nodes[i, 3], labelColor = "black",
          height = 50, width = 50, textOutlineColor = "white", textOutlineWidth = "1px",
          tooltip = nodes[i, 4]
        )
    }

    edges <- nobj$edgeData

    for (i in 1:nrow(edges)) {
      if (edges[i, 1] %in% nodes[, 1] && edges[i, 2] %in% nodes[, 1]) {
        elem[[length(elem) + 1]] <-
          buildEdge(
            source = as.character(edges[i, 1]),
            target = as.character(edges[i, 2]),
            lineColor = as.character(edges[i, 3])
          )
      }
    }

    output$cy <- renderShinyCyJS(shinyCyJS(elem, layout = list(name = "cola", nodeSpacing = 3, edgeLength = 250, animate = TRUE, randomize = TRUE, maxSimulationTime = 3000)))

    UpdateNodeSearch(session, sort(nobj$nodeData[, "id"]))
    shinyjs::show("legend2")
    output$legend2 <- renderUI(BuildEviLegend())
    ClearCy()
  })

  # ColorEdges
  observeEvent(input$btn4, {
    shinyjs::hide("DivContainOpt1")
    shinyjs::hide("DivContainOpt2")
    shinyjs::hide("DivContainOpt3")
    shinyjs::hide("DivContainOpt5")
    shinyjs::toggleElement("DivContainOpt4", anim = "slide", time = 0.5)
  })

  observeEvent(input$menuI, {
    if (input$menuI != "Unselect") {
      shinyjs::show("DRAW_GENENETWORK")
      shinyjs::show("PPICutoffContainer")

      i <- as.numeric(input$menuI)
      Nodes <- GsN[unique(unlist(cl[[i]]))]
      Nodes <- paste0("#", Nodes, collapse = ",")
      js$SetNode(Nodes)
      js$CheckNodeHigh(Nodes)
      shinyjs::delay(ms = 2000, expr = {
        js$CheckNodeHigh(Nodes)
      })
      shinyjs::delay(ms = 4000, expr = {
        js$CheckNodeHigh(Nodes)
      })
      shinyjs::delay(ms = 6000, expr = {
        js$CheckNodeHigh(Nodes)
      })
      shinyjs::delay(ms = 8000, expr = {
        js$CheckNodeHigh(Nodes)
      })
    }
    else {
      js$SetNode("")
      shinyjs::hide("DRAW_GENENETWORK")
      shinyjs::hide("PPICutoffContainer")
    }
  })

  observeEvent(input$btn15, {
    shinyjs::hide("DivContainOpt1")
    shinyjs::hide("DivContainOpt2")
    shinyjs::hide("DivContainOpt3")
    shinyjs::hide("DivContainOpt4")
    toggleElement("DivContainOpt5", anim = "slide", time = 0.5)
  })

  observeEvent(input$btn16, {
    shinyjs::showElement("ClearTab2")
    shinyjs::show("DivContainTab2")
    shinyjs::show("tab2")
    tab <- matrix(0, 0, 2)

    for (i in 1:length(cl)) {
      m <- cl[[i]]
      V <- v[m, m]
      r <- c()
      for (j in 1:length(m)) {
        r[j] <- length(which(V[j, ] <= DC))
        tab <- rbind(tab, c(GsN[m[j]], r[j]))
      }
    }
    colnames(tab) <- c("Gene-set", "Degree")
    tab <- tab[order(as.numeric(tab[, 2]), decreasing = TRUE), ]

    output$tab2 <- DT::renderDataTable(DT::datatable(tab[1:5, ],
      colnames = c("Cluster", "Degree"),
      options = list(dom = "t"),
      selection = "single",
      rownames = FALSE
    ))
    shinyjs::hideElement("DivContainOpt5")
  })

  observeEvent(input$btn17, {
    shinyjs::showElement("ClearTab2")
    shinyjs::hideElement("DivContainOpt5")
    shinyjs::show("DivContainTab2")
    i <- as.numeric(input$menuI)
    genes <- unique(unlist(GsM[cl[[i]]]))
    nobj <- BuildGeneNetwork(genes, input$PPICutoff / 1000, PPI, ScoreCutoff)
    v <- table(c(nobj$edgeData[, "source"], nobj$edgeData[, "target"]))
    v <- cbind(names(v), unname(v))
    v <- v[order(as.numeric(v[, 2]), decreasing = TRUE), ]
    output$tab2 <- DT::renderDataTable(DT::datatable(v[1:5, ],
      colnames = c("Gene", "Degree"),
      options = list(dom = "t"),
      rownames = FALSE
    ))
  })

  observeEvent(input$btn1, {
    # shinyjs::toggleElement('GENENETWORK_OPTIONS', anim = TRUE, time = 0.5)
    shinyjs::hide("DivContainOpt1")
    shinyjs::hide("DivContainOpt2")
    shinyjs::hide("DivContainOpt3")
    shinyjs::hide("DivContainOpt4")
    shinyjs::hide("DivContainOpt5")
  })

  observeEvent(input$btn18, {
    js$SetNodeSize(input$sld2)
    shinyjs::hideElement("DivContainOpt2")
    js$CyFit()
  })
}

shinyApp(ui = ui(), server = server, options = list(launch.browser = TRUE))
