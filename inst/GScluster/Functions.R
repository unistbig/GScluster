Ovl = function(a,b){length(intersect(a,b))/min(length(a), length(b))}
DO = function(a,b){ 1-Ovl(a,b) }

DK = function(a,b,l){
  A = length(intersect(a, b))
  B = length(setdiff(a,b))
  C = length(setdiff(b,a))
  D = l - (A+B+C)

  O = (A+D)/l;
  E = ((A+C)*(A+B)+(B+D)*(C+D))/(l*l);
  k = (O-E)/(1-E);
  return(1-k)
}

GetDO = function(GsM){
  d = length(GsM)
  v = matrix(0.001,d,d)
  for(i in 1:(d-1)){
    for(j in (i+1):d){
      v[i,j] = v[j,i] = DO(GsM[[i]],GsM[[j]])
    }
  }
  return(v)
}

GetDK = function(GsM){
  l = length(unique(unlist(GsM)))
  d = length(GsM)
  v = matrix(0.001,d,d)
  for(i in 1:(d-1)){
    for(j in (i+1):d){
      v[i,j] = v[j,i] = DK(GsM[[i]],GsM[[j]], l)
    }
  }
  return(v)
}

GetDOP = function(GsM, PPI, Alpha = 1){
  d = length(GsM)
  v = matrix(0.001,d,d)

  IndexingGsM = function(GsM, rp){
    sapply(1:length(GsM), function(i){
      unname(unlist(sapply(GsM[[i]],
                           function(j){
                             a = which(j==rownames(PPI))
                             if(length(a)){a}
                           }), use.names = FALSE))})

  }
  GsMI = IndexingGsM(GsM, rownames(PPI))

  DOP = function(gs1, gs2, gsi1, gsi2, PPI, Alpha){

    interact = function(idx1, idx2, PPI, k = 1){
      if(length(idx1)==0 | length(idx2)==0){return(0)}
      return(sum(PPI[idx1,idx2]^(1/k)) )
    }

    score = function(A, B, IA, IB, PPI, Alpha){ #
      w = min(length(A), length(B))/ (length(A)+length(B)) # 0.5633803
      a = length(A)
      i = length(intersect(A,B))
      uniqA = setdiff(IA,IB)
      uniqB = setdiff(IB,IA)
      common = intersect(IA,IB)
      nom = w*interact(uniqA,common,PPI) + interact(uniqA,uniqB,PPI)
      denom = w*i+length(setdiff(B,A))
      1-(i+Alpha*nom/denom)/a
    }
    s1 = score(gs1,gs2,gsi1,gsi2, PPI, Alpha)
    s2 = score(gs2,gs1,gsi2,gsi1, PPI, Alpha)

    return(max(min(s1, s2),0.001))

  }

  for(i in 1:(d-1)){
    for(j in (i+1):d){
      v[i,j] = v[j,i] = DOP(GsM[[i]], GsM[[j]], GsMI[[i]], GsMI[[j]], PPI, Alpha)
    }
  }
  return(v)
}

GetLinked = function(v, DistCutoff){ return(sort(which(v<=DistCutoff))) }

GetLinkedRatio = function(v, DistCutoff){
  L = nrow(v)
  if(L==1){return(0)}
  return( ( length(which(v<DistCutoff)) - L )/(L^2-L) ) # except Diag part ( -L )
}

GetUniq = function(v){ return(v[which(!duplicated(v))]) } # what's different with unique?

RemoveIncluded = function(v){
  L = length(v)
  LV = sapply(1:L, function(i){length(v[[i]])})
  r = c() # remove
  for(i in 1:(L-1)){
    for(j in (i+1):L){
      if(Ovl(v[[i]],v[[j]])==1){ ifelse(LV[i]>LV[j], { r = c(r,j) }, { r = c(r,i) } ) }
    }
  }
  if(length(r)){v = v[-r]}
  return(v)

}

GetGenes = function(Track, GM){ lapply(1:length(Track), FUN = function(i){ unique(unlist(GM[Track[[i]]], use.names = FALSE)) })}

RemoveSmallSeed = function(v, SizeCutoff){ return(v[which(sapply(v, length)>=SizeCutoff)])  }

RemoveWeakRatio = function(v, VarW, Dist, DistCutoff){
  return(v[which(sapply(1:length(v),function(i){
    GetLinkedRatio(Dist[v[[i]],v[[i]]], DistCutoff)})>=VarW)]) }

MergeTrack = function(v, Dist, VarM){ lapply(1:length(v), FUN = function(i){sort(unique(unlist(v[which(Dist[i,]<VarM)]))) }) }

GetMin = function(Dist){min(sapply(1:nrow(Dist), FUN = function(i){min(Dist[i, -i])}))}

GetClust = function(DistCutoff, MinSize, Dist, DistType, GM, Fuzzy = TRUE){

  Dist2 = Dist
  VarM = DistCutoff
  VarX = MinSize
  VarW = 0.5

  Track = list()

  for(i in 1:nrow(Dist)){ Track[[i]] = GetLinked(Dist[i,], DistCutoff) }

  Track = RemoveSmallSeed(Track, VarX)
  if(length(Track)==1){return(Track)}
  Track = RemoveWeakRatio(Track, VarW, Dist, DistCutoff)
  if(length(Track)==1){return(Track)}
  names(Track) = NULL
  MinDist = -9999

  while(MinDist <= VarM){
    Track = GetUniq(Track)
    if(length(Track)==1){return(Track)}
    Track = RemoveIncluded(Track)
    if(length(Track)==1){return(Track)}
    TrackGenes = GetGenes(Track, GM)

    if(DistType==1){ Dist = GetDO(TrackGenes) }
    if(DistType==2){ Dist = GetDOP(TrackGenes,PPI)}
    if(DistType==3){ Dist = GetDK(TrackGenes) }

    Track = MergeTrack(Track, Dist, VarM)
    if(length(Track)==1){break}
    Track = GetUniq(Track)
    if(length(Track)==1){break}
    Track = RemoveIncluded(Track)
    if(length(Track)==1){break}
    TrackGenes = GetGenes(Track, GM)

    if(DistType==1){Dist = GetDO(TrackGenes)}
    if(DistType==2){Dist = GetDOP(TrackGenes, PPI)}
    if(DistType==3){Dist = GetDK(TrackGenes)}

    MinDist = GetMin(Dist)
  }

  if(!Fuzzy){
    #rownames(Dist2) = colnames(Dist2) = paste0("GS",1:nrow(Dist2))
    ut = unique(unlist(Track))
    DD = Dist2[ut,ut]
    for(i in 1:nrow(DD)){
      idxa = which(DD[i,]<=DistCutoff)
      idxb = which(DD[i,]>DistCutoff)
      DD[i,idxa] = 1
      DD[i,idxb] = 0
    }
    ct = spectralClustering(DD, k = length(Track))
    #ct = cutree(hc,k = length(Track))
    Track2 = list()
    for(i in 1:length(Track)){ Track2[[i]] = ut[which(ct==i)] }
    return(Track2)
  }

  return(Track) # return index form

}

ToGsN = function(i, GsN){GsN[as.numeric(i)]}

GetColors = function(i){
  sample(
    c("#e6194B", '#3cb44b', '#ffe119', '#4363d8', '#f58231',
      '#911eb4', '#42d4f4', '#f032e6', '#bfef45', '#fabebe',
      '#469990', '#e6beff', '#9A6324', '#fffac8', '#800000',
      '#aaffc3', '#808000', '#ffd8b1', '#000075'), i)
}

BuildBasicNodes = function(ClustObj, Col){
  v = c()
  for(i in 1:length(ClustObj)){
    v = rbind(v, cbind(ClustObj[[i]], Col[i]))
  }
  as.data.frame(v, stringsAsFactors = FALSE)
}

GrayDuplicateNodes = function(nodeData){
  dIdx = which(duplicated(nodeData[,1]))
  dObj = nodeData[dIdx,1]
  nodeData = nodeData[-dIdx,]
  for(i in 1:length(dObj)){
    nodeData[which(nodeData[,1]==dObj[i]),2] = '#ffdd59'
  }
  nodeData
}

AddNameCol = function(nodeData, GsN){
  nodeData[,1] = sapply(nodeData[,1], function(i){ToGsN(i,GsN)})
  nodeData = cbind(nodeData[,1],nodeData)
  colnames(nodeData) = c('id','name','color')
  nodeData
}

GetEdgeWidth = function(i){ return(round(i/0.111+(10-1/0.111),3)) }

GetClustDist = function(ClustObj,Dist){
  v = sort(unique(unlist(ClustObj)))
  Dist = Dist[v, v]
  rownames(Dist) = colnames(Dist) = ToGsN(v, GsN)
  Dist
}


StringSep = function(x, Cv){
  x = strsplit(x,'')[[1]]
  v = ""
  # Cv : Cut threshold : 20 for genemembers, 30 for genesetNames
  L = length(x)
  for(i in 1:floor(L/Cv)){
    v = paste(v, paste(x[(1:Cv+Cv*(i-1))], collapse = ''), '\n', sep = '')
  }

  if(i*Cv+1 <= L){
    v = paste(v, paste(x[(i*Cv+1):L], collapse = ""), collapse = '', sep = '')
  }
  return(v)
}

BuildDT = function(cl, GsN, GsM, GsQ){

  DF = data.frame(matrix(ncol=4,nrow=0), stringsAsFactors = FALSE)
  colnames(DF) = c("Cluster","Geneset Name","Geneset Qvalue", "Geneset member")
  for(i in 1:length(cl)){
    ThisClust = cl[[i]]
    for(j in 1:length(ThisClust)){
      v = ThisClust[j]
      ThisGsN = GsN[v]
      if(nchar(ThisGsN)>30){ ThisGsN = StringSep(ThisGsN, 30) }
      ThisGsM = paste(GsM[[v]], collapse = ', ')
      if(length(ThisGsM)>30){ ThisGsM = StringSep(ThisGsM, 30) }
      ThisGsN = as.character(ThisGsN)
      ThisGsM = as.character(ThisGsM)
      ThisGsQ = as.numeric(GsQ[v])
      ThisClustNo = i
      DF = rbind(DF,cbind(ThisClustNo,ThisGsN, round(ThisGsQ,5), ThisGsM))
    }
  }

  DF[,3] = as.numeric(as.character(DF[,3])) # geneset qvalue
  # DF[,2] = as.character(DF[,2]) # geneset name

  DT::datatable(
    DF,
    extensions = 'Buttons',
    filter = 'top',
    rownames = FALSE,
    options = list(
      dom = 'Bltipr',
      #dom = 'Blftipr',
      lengthChange = TRUE,
      pageLength = 50,
      #columnDefs = list(list(targets = c(3), searchable = FALSE)),
      autoWidth = TRUE,
      scrollX = TRUE,
      scrollY = TRUE,
      buttons = list(list(extend='csv', text='Download')),
      order = list(2,'asc')
    ),
    colnames = c("Cluster","Name", 'Qvalue', 'Member'),
    selection = 'none'
  )
}

UpdateNodeSearch = function(session, ids){
  updateSelectizeInput(session, inputId = 'sel1', choices = ids, server = TRUE)
}

UpdateClusters = function(session, ids){
  updateSelectizeInput(session, inputId = 'menuI', choices = c("Unselect",ids), server = TRUE)
}

GetHubGenes = function(genes, PPI, PPICutoff, ScoreCutoff){
  genes = intersect(genes, rownames(PPI))
  genes = intersect(genes, names(which(GS<ScoreCutoff)))
  tab = PPI[genes,genes]

  d = sapply(1:nrow(tab), function(i){ length(which(tab[i,] >= PPICutoff/1000)) })
  names(d) = rownames(tab)
  sort(d[which(rank(-d, ties.method = 'min')<=5)], decreasing = T)
}

BuildLegend = function(colors){
  res = c()
  colors = setdiff(unique(colors), '#666666')
  for(i in 1:length(colors)){
    b = HTML(
      paste0(
        '<div
        style="background:',colors[i],
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
        i,'</div><br>'
      )
    )
    res = paste0(res,b)
  }
  return(HTML(res))
}

BuildEviLegend = function(){
  res = c()
  colors = c('red','orange','yellow','green','blue','navy')
  Evi = c('Neighborhood','Gene-fusion','Cooccurence','Coexpression','Experiment','Database')
  for(i in 1:length(colors)){
    b = HTML(
      paste0(
        '<div
        style="background:',colors[i],
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
        Evi[i],'</div><br>'
      )
    )
    res = paste0(res,b)
  }
  return(HTML(res))
}

BuildQvLegend = function(){
  res = c()

  # colors = c("#00d2d3", "#66e4e4", "#FFFFFF") MINT SKY
  colors = c("#30cc72", "#59d68e", "#82e0aa", "#FFFFFF") # LIGHT GREEN
  exp = c("0", "0.05","0.1", ">= 0.25")
  for(i in 1:length(colors)){
    b = HTML(
      paste0(
        '<div
        style="background:',colors[i],';
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
        exp[i],'</div><br>'
      )
    )
    res = paste0(res,b)
  }
  return(HTML(res))
}

BuildGeneNetwork = function(genes, PPICutoff = 0.7, PPI, ScoreCutoff){

  source = target = c()

  # build edges

  genes = intersect(genes, rownames(PPI))
  if(!is.null(GS)){
    genes = intersect(genes, names(which(GS<ScoreCutoff)))
  }


  if(length(genes) ==0){
    showNotification("NO Genes with Cutoff", type='error')
    return()
  }

  tab = PPI[genes,genes]

  # EdgeData
  for(i in 1:nrow(tab)){
    v = names(which(tab[i,] >= PPICutoff))
    if(length(v) > 0){v = v[which(v<genes[i])]} # alphabetically smaller
    if(length(v) > 0){
      source = c(source, rep(genes[i], length(v)))
      target = c(target, v)
    }
  }

  color = rep('#001c54', length(source)) # Edge color : gray , #666666, / pink , #fc7bee
  edgeData = data.frame(source, target, color, stringsAsFactors=FALSE)

  if(length(source) ==0){
    showNotification("NO Interactions with Cutoff", type='error')
    return()
  }

  #00d2d3 ~ #FFFFFF ( MINT SKY )
  # 0 -> 0,210,211
  # 0.25 -> 255,255,255

  #30cc72 ~#FFFFFF ( LIGHT GREEN )
  # 0 -> 48,204,114
  # 0.25 -> 255,255,255


  # build nodes
  name = id = unique(union(source, target))

  color = rep('#F8EFBA',length(id)) # not genescore : yellow color
  if(!is.null(GS)){
    color = sapply(GS[id], function(i){
      # rgb(1020*i,180*i+210,176*i+211,max = 255)
      rgb(860*(0.25/ScoreCutoff)*i, 400*(0.25/ScoreCutoff)*i+155, 720*(0.25/ScoreCutoff)*i+47,max = 255)
    })
  }

  href = c()
  suppressWarnings(
    for(i in 1:length(id)){
      href[i] = GetGenecardURL(id[i])
    }
  )
  nodeData = data.frame(id, name, color, href, stringsAsFactors=FALSE)
  return( list(nodeData = nodeData, edgeData = edgeData) )

}

ClearCy = function(hover = FALSE, rand = FALSE){
  shinyjs::delay( # DELAYED FUNCTION FOR AFTER CY DECLARED
    ms = 2000,
    expr = {
      #js$SetCxtTap();
      if(!hover){js$ClearMouseOverNode();}
      js$ColaLayout(rand);
      js$SetClickNode();
      js$SetSoftZoom();
      js$defineColorMap();
    }
  )
}

BuildMultiHub = function(cl, GsM, PPI, PPICutoff, ScoreCutoff){
  res = data.frame(matrix(ncol=3,nrow=0), stringsAsFactors = FALSE)
  for(i in 1:length(cl)){
    hubs = GetHubGenes(unique(unlist(GsM[cl[[i]]])), PPI, PPICutoff, ScoreCutoff)
    res = rbind(res,
      data.frame(
        Gene = names(hubs),
        Degree = unname(hubs),
        Cluster = i,
        stringsAsFactors = FALSE
      ))
  }
  k = sort(unique(res[which(duplicated(res['Gene'])),'Gene']))
  kk = c()
  for(i in 1:length(k)){ kk = c(kk, which(res['Gene']==k[i]))  }
  rownames(res) = NULL

  DT::datatable(
    res[kk,],
    colnames = c("Gene", 'Degree', 'Cluster'),
    options = list(dom = 't'),
    rownames = FALSE,
    selection = 'single'
  )
}

RenderGeneNetwork = function(genes, output, PPICutoff, PPI, ScoreCutoff, session){

  nobj = BuildGeneNetwork(genes, PPICutoff, PPI, ScoreCutoff)
  if(is.null(nobj)){return(0)}

  cjn = createCytoscapeJsNetwork(
    nobj$nodeData,
    nobj$edgeData,
    edgeTargetShape = 'none',
    edgeColor = '#fc7bee',
    #edgeColor = '#ec3e50',
    nodeLabelColor = 'black',
    nodeHeight = '50',
    nodeWidth = '50'
  )

  UpdateNodeSearch(session, sort(nobj$nodeData[,"id"]))

  suppressWarnings(
    for(i in 1:length(cjn$nodes)){
      cjn$nodes[[i]]$data$href = GetGenecardURL(cjn$nodes[[i]]$data$id)
    }
  )

  output$CY = renderRcytoscapejs( rcytoscapejs(cjn$nodes, cjn$edges, highlightConnectedNodes = FALSE) )

  shinyjs::delay(
    ms = 2000,
    expr = {
      #js$ClearEdge()
      #js$BorderNode()
      js$ColorLabelNode()
      js$StrongEdge();
    }
  )

  return(1)
}

GetColorEdge = function(genes, l, color){
  res = l[genes]
  for(i in 1:length(res)){
    res[[i]] = intersect(res[[i]], genes)
  }

  res = res[which(sapply(res, length)!=0)]
  v = data.frame(matrix(ncol=3,nrow=0), stringsAsFactors = FALSE)

  if(length(res)){

    for(i in 1:length(res)){
      A = res[[i]]
      for(j in 1:length(A)){
        v = rbind(v,cbind(names(res)[i], A[j], color))
      }
    }

    colnames(v) = c('source','target','color')

    return(v)
  }
  return(0)
}

GetDisease = function(g){
  #res = as.data.frame(table(Disease[unname(unlist(
    #sapply(g, function(i){which(i==Disease[,1])}))),2]))

  # WHY WORK WHY NOT WORK?
   #print(g)
  #print(names(D))
   #print(length(intersect(names(D),g)))
  # save(g,file = 'G.RData')
  # print(length(D[g]))

  res = as.data.frame(table(unlist(D[g])))
  res = res[which(res[,2]> 3),]
  res
}

SendError = function(session,title,text){
  sendSweetAlert(session = session,title = ,text = )
}

GetGenecardURL = function(gene){
  a(href = paste0('http://www.genecards.org/cgi-bin/carddisp.pl?gene=',gene))
}

percentRank = function(arr,n){
  L = 0
  S = 0
  N = length(arr)
  for(i in 1:N){
    if(arr[i]<n){L = L+1}
    else{if(arr[i] ==n){S = S+1}}
  }
  (L+0.5*S)/N
}
