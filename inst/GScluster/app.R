# Read Functions.
source(file = 'Functions.R', local = TRUE)
# Additional function to check rcytoscapejs library
CheckCy = function(){
  if(!require("rcytoscapejs")) {
    #print("To Run this appliaction, rcytoscapejs package required")
    #print("Please Download Package from https://github.com/cytoscape/cyjShiny/releases")
    #print("and Install that, command should looks like : ")
    #print("install.packages('cyjShiny-0.0.7.tar.gz', repos = NULL) ")
    print("install rcytoscapejs package")
    install.packages("https://github.com/cytoscape/cyjShiny/archive/v0.0.7.tar.gz",
                     repos=NULL, method="libcurl")
    library(rcytoscapejs)
  }
  else{
    library(rcytoscapejs)
  }
  library(shinydashboard)
  library(shinyjs)
}

# object handling Functions

GetGsN = function(tab){ tab[,1] }
GetGsQ = function(tab){ tab[,3] }
GetGsM = function(tab){ lapply(1:nrow(tab), function(i){strsplit(tab[i,2],' ')[[1]]}) }
GetGsD = function(tab){ tab[,4] }
GetGS = function(tab){
  res = tab[,2]
  names(res) = tab[,1]
  res
}
ReadDiseaseFile = function(){
  load('DisGeNet.RData')
  return(D)
}

# check cytoscape library has prepared
CheckCy()

#### DEMO DATAS

if(!is.null(.GeneScores)){ GeneScores = .GeneScores } else{
  print("genescore not given, read Demo file")
  GeneScores = read.delim('sample_genescore.txt', header = TRUE, stringsAsFactors = FALSE) # Gene, Score
}

if(!is.null(.GSAresult)){ GSAresult = .GSAresult } else{
  print("GSAresult not given, read Demo file")
  GSAresult = read.delim('sample_geneset.txt',header = TRUE, stringsAsFactors = FALSE) # Name, Genes, Qvalue
}

if(!is.null(.PPI)){ # not string PPI, no use btn4;
  UseString = FALSE
  PPI = .PPI
} else
{
  print("PPI table is not given, STRING data is used.")
  if(.Species=='A'){ DirName="arabidopsis/" }
  if(.Species=='C'){ DirName="celegans/" }
  if(.Species=='E'){ DirName="ecoli/" }
  if(.Species=='F'){ DirName="fly/" }
  if(.Species=='H'){
    D = ReadDiseaseFile()
    DirName='human/'
  }
  if(.Species=='I'){ DirName="rice/" }
  if(.Species=='M'){ DirName="mouse/" }
  if(.Species=='R'){ DirName="rat/" }
  if(.Species=='Y'){ DirName="yeast/" }
  if(.Species=='Z'){ DirName="zebrafish/" }

  Files = paste0(DirName,paste0("l",1:6), '.RData')

  if(!file.exists(Files[1])){
    DownloadData(species=strsplit(DirName,'/')[[1]])
  }

  for(i in 1:6){load(Files[i])}
  load(paste0(DirName,'string.RData'))

  PPI = string
  rm(string)
  UseString = TRUE
}

GsN = GetGsN(GSAresult) # Geneset Name
GsQ = GetGsQ(GSAresult) # Geneset Qvalue
GsM = GetGsM(GSAresult) # Geneset Member
GS = GetGS(GeneScores) # Gene Score
IsGsD = FALSE
if(ncol(GSAresult)==4){ # Direction
  GsD = GetGsD(GSAresult)
  IsGsD = TRUE
}

# Cutoff by GsQCutoff
Idx = which(GsQ <=.GsQCutoff)

GsN = GsN[Idx]
GsQ = GsQ[Idx]
GsM = GsM[Idx]
if(IsGsD){GsD = GsD[Idx]}


ui = function(){
  shinydashboard::dashboardPage(
    title = 'GScluster',
    header = shinydashboard::dashboardHeader(
      title='GScluster'
    ),
    sidebar = shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        menuItem(text = "Clustering Results", tabName = 'menu2'), # Table
        menuItem( # Network
          text = "Network Plot",
          tabName = 'menu1',
          selected = TRUE
        ),
        div(
          style='z-index:999;',
          id='GENENETWORK_OPTIONS',
          verticalLayout(
            selectizeInput(
              inputId='menuI',
              label='Select Gene-set Cluster',
              choices='NOT BUILT YET',
              selected=NULL,
              width='auto'
            ),
            sliderInput(
              inputId='PPICutoff',
              label="PPI Cutoff: ",
              min=0,max=999,value=700,step=10
            ),
            actionButton(inputId = "DRAW_GENENETWORK", label = "Show PPI", style='z-index:999;width:14em;')
          ),
          width = 12
        ),
		    actionButton( inputId = 'btn2',label = "Go Back",style='z-index:999;width:14em;display:none;' )
      )
    ),
    body = shinydashboard::dashboardBody(
      shinyjs::useShinyjs(),
      shinyjs::extendShinyjs(script = 'shinyjs.js'), # change as scripts;
      tags$head(tags$script(src="cytoscape-cy-svg-convertor.js")),
      tags$head(tags$script(src="cytoscape-svg-convertor.js")),
      tags$head(tags$script(src="svg.min.js")),
      tags$head(tags$script(src="additional_script.js")),
      div(id='create', display='none'), # EMPTY DIV FOR DOWNLOAD SVG
      tags$head(
        tags$style(
          HTML(
            '
            /* logo */
            .skin-blue .main-header .logo {
            background-color: #2f9193;
            }

            /* logo when hovered */
            .skin-blue .main-header .logo:hover {
            background-color: #2f9193;
            }

            /* body */
            .content-wrapper, .right-side {
            background-color:white;
            }

            /* Header Bar */
            .skin-blue .main-header .navbar {
            background-color: #44c1c4;
            }

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
            .skin-blue .sidebar-menu>li>.treeview-menu{
            background : #143e41;
            }


            /* Datatable button Position*/
            .dataTables_wrapper .dt-buttons {
              margin-right:2em;
            }

            '
          )
          )
          ),
      tags$style(type = "text/css", "html,body {min-height: 100%}"),
      tags$style(type = "text/css", "height: 100%; background:'white'"),

      tabItems(
        tabItem(
          tabName = 'menu1',
          actionButton(
            inputId = 'btn3',
            label = 'Download Graph',
            style='position:absolute; z-index:9999;right:1em;top:0.5em;'
          ),
          actionButton(
            inputId='btn10',
            label = "Graph Layout",
            style='position:absolute; z-index:9999;right:11em;top:0.5em;'
          ),
          actionButton( # 1em to 3em --> 5 ~ 15
            inputId = "btn11",
            label = 'Label Size',
            style='position:absolute; z-index:9999;right:19.5em;top:0.5em;'
          ),
          actionButton(
            inputId = "btn12",
            label = "Clustering Method",
            style='position:absolute; z-index:9999;right:26.5em;top:0.5em;'
          ),
          actionButton(
            inputId = "btn13",
            label = "Find",
            style='position:absolute; z-index:9999;left:43em;top:0.5em;'
          ),
          div(
            style='position:absolute; z-index:9999;left:21em;top:0.5em;',
            selectInput(
              inputId = 'sel1',
              label = NULL,
              choices = NULL,
              selected = NULL
            )
          ),
          fluidRow(
            width = 12,
            id='DivContainOpt3',
            style=
              'border: 2px solid rgb(0, 27, 84);
            padding: 0px 1em 1em;
            z-index: 9999;
            position: absolute;
            background: white;
            top: 3em;
            right: 20em;
            display: none;',
            selectInput(inputId = 'sel2', label = 'Distance',choices = c("pMM","MM","Kappa")),
            numericInput(inputId = 'alpha', label = 'Network weight α (for pMM only)', value = 1, min = 0, max = 1, step = 0.05),
            sliderInput(inputId = 'num1', label = 'Minimum Seed Size',value = 3, min = 3, max = 8, step=1),
            numericInput(inputId = 'num2', label = 'Merging Threshold', value = 0.5, step = 0.05, min= 0.1, max = 0.9),
            textOutput('txt1'),
            actionButton(inputId = 'btn8', label = 'Apply', style='margin-left:17em')
          ),
          fluidRow(
            width = 12,
            id='DivContainOpt2',
            style=
              'border: 2px solid rgb(0, 27, 84);
            padding: 0px 1em 1em;
            z-index: 9999;
            position: absolute;
            background: white;
            top: 3em;
            right: 11em;
            display: none;',
            sliderInput(inputId = 'sld1',label = 'Node Label Size (px)',min = 1, max = 3, value = 1, step = 0.2),
            actionButton(inputId = 'btn6', label = "Apply")
          ),
          fluidRow(
            width = 12,
            id='DivContainOpt1',
            style=
              'border: 2px solid rgb(0, 27, 84);
            padding: 0px 1em 1em;
            z-index: 9999;
            position: absolute;
            background: white;
            top: 3em;
            right: 11em;
            display: none;',
            radioButtons(
              inputId = 'rad1',
              label='',
              choices = c("Cola","Circle"),
              selected = "Cola",
              inline = TRUE
            ),
            actionButton(
              inputId = 'btn5',
              label = "Apply"
            )
          ),
          actionButton(
            inputId = 'btn4',
            label = 'PPI Evidence',
            style='position:absolute; z-index:9999;right:26.5em;top:0.5em;display:none;'
          ),
          fluidRow(
            width = 12,
            id='DivContainOpt4',
            style=
              'border: 2px solid rgb(0, 27, 84);
            padding: 0px 1em 1em;
            z-index: 9999;
            position: absolute;
            background: white;
            top: 3em;
            right: 26em;
            width :10em;
            display: none;',
            shinyWidgets::checkboxGroupButtons(
              inputId = 'che1',
              label = '',
              direction = 'vertical',
              size = 'sm',
              choices = c(
                `<i class='fas fa-circle' style='color:red'></i>&nbsp;Neighborhood&nbsp;` = "Neighborhood",
                `<i class='fas fa-circle' style='color:orange'></i>&nbsp;Gene-fusion&nbsp;` = 'Gene-fusion',
                `<i class='fas fa-circle' style='color:yellow'></i>&nbsp;Cooccurence&nbsp;` = 'Cooccurence',
                `<i class='fas fa-circle' style='color:green'></i>&nbsp;Coexpression&nbsp;` = 'Coexpression',
                `<i class='fas fa-circle' style='color:blue'></i>&nbsp;Experiment&nbsp;` = 'Experiment',
                `<i class='fas fa-circle' style='color:navy'></i>&nbsp;Database&nbsp;` = 'Database')
            ),
            #checkboxGroupInput(inputId = 'che1',label='',
            #choices = c('Neighborhood','Gene-fusion','Cooccurence','Coexpression','Experiment','Database')
            #)
            actionButton(inputId = 'btn14', label = 'Apply', style='margin-left:auto')
          ),

          actionButton( # HUB IN GENESETS
            inputId = 'btn15',
            label='Hubs',
            style='position:absolute; z-index:9999;left:1em;margin-top:37em;width:14em;'
          ),
		  fluidRow(
		  width=12,
		  id='DivContainOpt5',
		  style='border: 2px solid rgb(0, 27, 84);
            padding: 1em;
            z-index: 9999;
            position: absolute;
            background: white;
            top: 41em;
            left: 19em;
            width :10em;
            display: none;',
			actionButton(inputId='btn16',label='Gene-set Hub', style="width:8em;"), # GENESET Hub
			actionButton(inputId='btn17',label='Gene Hub', style="width:8em;"), # GENE Hub
			actionButton(inputId='RenderTab2', label='Multi-cluster Hub', style="width:8em;") # MULTI Hub
			),

          actionButton(
            inputId = 'RenderPlot1',
            label = 'WordCloud',
            style='position:absolute; z-index:9999;right:1em;margin-top:1em;display:none;'
          ), # Wordcloud
          fluidRow(
            width = 12,
            id='DivContainPlot1',
            style='padding-right:1em;
            z-index:-1;
            position:absolute;
            margin-top:10em;
            right:0.5em;',
            wordcloud2::wordcloud2Output(
              outputId = 'plot1',
              width = '400px',
              height = '400px'
            ),
            actionButton(inputId='ClearPlot1', label = 'Clear', style='float:right;display:none;')
          ),

          fluidRow(
            width = 4,
            id='DivContainTab2',
            style='padding-right:1em;
            z-index:999;
            text-align:center;
            position:absolute;
            margin-top:6em;
            right:0.5em;',
            DT::dataTableOutput(outputId = 'tab2'),
            actionButton(inputId='ClearTab2', label = 'Hide Tab', style='float:right;display:none;margin-top:1em;')
            #actionButton(inputId='ClearTabNode', label = 'Unselect Nodes', style='float:right;display:none;margin-top:1em;')
          ),
          uiOutput(
            style={'position:absolute;margin-top:17em;z-index:999'},
            outputId='legend'
          ),
          p(
            style={'position:absolute;bottom:5em;z-index:999;'},
            imageOutput(outputId = "img1", inline = TRUE, width='300px')
          ),
          uiOutput(
            style={'position:absolute;bottom : 5em; margin-left:7em;z-index:999;'},
            outputId='legend2'
          ),
          div(
            id='CYCONTAINER',
            shinycssloaders::withSpinner(
              rcytoscapejsOutput(outputId = 'CY', height = "100%"),
              proxy.height = '600px')
          )
        ),
        tabItem(
          tabName = 'menu2',
          DT::dataTableOutput(outputId = 'tab1')
        ),
        tabItem(
          tabName='menu3'
        )
		)
      )
    )
  }

server = function(input, output, session) {
  if(!UseString){shinyjs::disable('btn4')}
  if(.Species!='H'){shinyjs::disable('RenderPlot1')}
  js$SetHeight()

  shinyjs::disable('btn17')
  BuildNetworkObj = function(ClustObj, GsN, Dist, MinEdge){

	  #FFFFFF : WHITE
    Col = rep("#053190",length(ClustObj)) #GetColors(length(ClustObj))
    nodeData = BuildBasicNodes(ClustObj, Col)

    #nodeData = GrayDuplicateNodes(nodeData)
    nodeData = AddNameCol(nodeData, GsN)

    Color = sapply(GsQ[sapply(nodeData[,'id'], function(i){which(i==GsN)})],
                   function(i){ rgb(880*i, 880*i, 228*i+198,max = 255) })

    nodeData[,'color'] = Color

    if(IsGsD){
  		DN = GsN[which(GsD=='DN')]
  		Idx = unlist(sapply(DN, function(i){which(i == nodeData[,1])}))

  		Color = sapply(GsQ[sapply(nodeData[Idx,'id'], function(i){which(i==GsN)})],
  		               function(i){ rgb(800*i+22, 388*i+158, 744*i+48,max = 255) })
  		nodeData[Idx,'color'] = Color# '#218E3D' # Sora GREEN COLOR
  		UP = GsN[which(GsD=='UP')]
  		Idx = unlist(sapply(UP, function(i){which(i == nodeData[,1])}))

  		Color = sapply(GsQ[sapply(nodeData[Idx,'id'], function(i){which(i==GsN)})],
		function(i){ rgb(-16*i+255, 920*i, 936*i,max = 255) })

  		nodeData[Idx,'color'] = Color # '#E81548' # Sora Red COLOR
  	}

    Dist = GetClustDist(ClustObj,Dist)
    source = target = w = c()

    for(i in 1:nrow(Dist)){
      v = which(Dist[i, -i] <= MinEdge) # Strong Edges only
      if(length(v)>0){v = v[which(names(v)<rownames(Dist)[i])]} # One-side Edge : Name Ordered
      if(length(v)>0){
        nv = names(v)
        source = c(source, rep(rownames(Dist)[i], length(v)))
        target = c(target, nv)
        w = c(w,sapply(Dist[i,v], GetEdgeWidth))
      }
    }

    edgeData = data.frame(source, target, stringsAsFactors=FALSE)
    return(list (nodeData = nodeData, edgeData = edgeData, color = Col, width = w))
  }

  RenderGeneSetNetwork = function(cl, GsN, v, output, session, DC){
    nobj = BuildNetworkObj(cl, GsN, v, DC)

    cjn = createCytoscapeJsNetwork(
      nobj$nodeData,
      nobj$edgeData,
      edgeTargetShape = 'none',
      edgeColor = '#001c54',
      nodeLabelColor = 'black',
      nodeHeight = '30',
      nodeShape = 'ellipse',
      nodeWidth = '30'
    )

    output$CY = renderRcytoscapejs( rcytoscapejs(cjn$nodes, cjn$edges, highlightConnectedNodes = FALSE))
    UpdateNodeSearch(session, sort(nobj$nodeData[,"id"]))
    UpdateClusters(session, 1:length(cl))
    ClearCy()
  }

  ScoreCutoff = .GQCutoff
  # DEFAULT CLUSTER = > pMM, 0.5 of MM, 3, alpha = 1

  v = GetDOP(GsM, PPI, .alpha)
  DC = unname(quantile(v, percentRank(as.numeric(GetDO(GsM)), 0.5 )))
  DC2 = unname(quantile(GetDK(GsM), percentRank(as.numeric(GetDO(GsM)), 0.5 )))

  output$txt1 = renderText(paste("pMM : ",round(DC,4), "MM : ",0.5,"Kappa : ", round(DC2,4), collapse = ''))

  updateNumericInput(session, 'num2',label='Maximum gene-set distance', value = as.numeric(DC), step = 0.05, min= 0.1, max = 0.9)  
  cl = GetClust(DistCutoff = DC, MinSize = 3, Dist = v, DistType = 2, GM = GsM)
  RenderGeneSetNetwork(cl, GsN, v, output, session, DC)
  tab = BuildDT(cl, GsN, GsM, GsQ)
  output$tab1 = DT::renderDataTable(tab)
  ClearCy()
  if(IsGsD){ output$img1 = renderImage({ list( src = "grscale.png", contentType = "image/png" ) }, deleteFile = FALSE) }
  else{ output$img1 = renderImage({ list( src = "bscale.png", contentType = "image/png") }, deleteFile = FALSE) }

  # GENE NETWORK
  observeEvent(input$DRAW_GENENETWORK, {
    if(input$menuI!='Unselected'){
      i = as.numeric(input$menuI)
      genes = unique(unlist(GsM[cl[[i]]]))
      v = RenderGeneNetwork(genes, output, input$PPICutoff/1000, PPI, ScoreCutoff, session)

      if(v){ # Success
        ClearCy(hover = TRUE)
        output$img1 = renderImage({ list( src = "gscale.png", contentType = "image/png" ) }, deleteFile = FALSE)
        js$ToggleElem("GENENETWORK_OPTIONS")
        shinyjs::enable('btn17')
        shinyjs::showElement("btn4")
        shinyjs::showElement('btn2')
        shinyjs::showElement("RenderPlot1")
        shinyjs::hideElement("RenderTab2")
        shinyjs::hideElement("btn12")
        shinyjs::hideElement('btn16')
        shinyjs::delay(ms = 2000,{js$SetHref()})

        shinyjs::toggleElement('GENENETWORK_OPTIONS', anim = TRUE, time = 0.5)
        shinyjs::hide('DivContainOpt1')
        shinyjs::hide('DivContainOpt2')
        shinyjs::hide('DivContainOpt3')
        shinyjs::hide('DivContainOpt4')
        shinyjs::hide('DivContainOpt5')
      }
    }
  })

  # BACKTO BUILT GENESET NETWORK
  observeEvent(input$btn2,{
    nobj = BuildNetworkObj(cl, GsN, v, input$num2)

    cjn = createCytoscapeJsNetwork(
      nobj$nodeData,
      nobj$edgeData,
      edgeTargetShape = 'none',
      edgeColor = '#001c54',
      nodeLabelColor = 'black',
      nodeShape = 'ellipse',
      nodeHeight = '30',
      nodeWidth = '30'
    )

    output$CY = renderRcytoscapejs( rcytoscapejs(cjn$nodes, cjn$edges, highlightConnectedNodes = FALSE))
    UpdateNodeSearch(session, sort(nobj$nodeData[,"id"]))
    UpdateClusters(session, 1:length(cl))
    ClearCy()

    if(IsGsD){ output$img1 = renderImage({ list( src = "grscale.png", contentType = "image/png" ) }, deleteFile = FALSE) }
    else{ output$img1 = renderImage({ list( src = "bscale.png", contentType = "image/png" ) }, deleteFile = FALSE) }

    tab = BuildDT(cl, GsN, GsM, GsQ)
    output$tab1 = DT::renderDataTable(tab)
    shinyjs::hideElement("btn4")
    shinyjs::hideElement("RenderPlot1")
    shinyjs::showElement("RenderTab2")
    shinyjs::showElement("btn12")
    shinyjs::hideElement("legend2")
	shinyjs::disable('btn17')
    shinyjs::showElement("GENENETWORK_OPTIONS")
    shinyjs::hideElement('btn2')
    shinyjs::showElement('btn16')
  })

  # Multi-Hub : find high ranked genes around all cluster, and return their intersected

  # Render Multihub
  observeEvent(input$RenderTab2,{
    shinyjs::showElement("DivContainTab2")
    shinyjs::showElement("ClearTab2",  anim = 'slide', time = 0.5)
	  shinyjs::hideElement("DivContainOpt5")

    output$tab2 = DT::renderDataTable(BuildMultiHub(cl,GsM,PPI, input$PPICutoff, ScoreCutoff) )
  })

  observeEvent(input$tab2_rows_selected,{
    js$HighlightTab()
  })

  # Render Wordcloud
  observeEvent(input$RenderPlot1,{
    shinyjs::showElement("DivContainPlot1", anim = 'slide', time = 0.5)
    shinyjs::showElement("ClearPlot1",  anim = 'slide', time = 0.5)
    shinyjs::hideElement("RenderPlot1", anim = 'slide', time = 0.5)
    shinyjs::runjs("$('#DivContainPlot1').css('z-index',1)")
    i = as.numeric(input$menuI)
    res = GetDisease(unique(unlist(GsM[cl[[i]]])))
    wc = wordcloud2(res)
    output$plot1 = renderWordcloud2( wc )
  })

  # Layout Sidebar
  observeEvent(input$btn5,{
    if(input$rad1 =='Cola'){js$ColaLayout()}
    if(input$rad1=='Circle'){js$CircleLayout()}
    shinyjs::hideElement("DivContainOpt1")

  })

  # Hide Div After Option selected
  observeEvent(input$btn10,{
    shinyjs::toggleElement("DivContainOpt1",  anim = 'slide', time = 0.5)
    shinyjs::hide('DivContainOpt2')
    shinyjs::hide('DivContainOpt3')
    shinyjs::hide('DivContainOpt4')
    shinyjs::hide('DivContainOpt5')
    shinyjs::hide('GENENETWORK_OPTIONS')
    })
  observeEvent(input$btn11,{
    shinyjs::toggleElement("DivContainOpt2",  anim = 'slide', time = 0.5)
    shinyjs::hide('DivContainOpt1')
    shinyjs::hide('DivContainOpt3')
    shinyjs::hide('DivContainOpt4')
    shinyjs::hide('DivContainOpt5')
    shinyjs::hide('GENENETWORK_OPTIONS')
    })
  observeEvent(input$btn12,{
    shinyjs::toggleElement("DivContainOpt3",  anim = 'slide', time = 0.5)
    shinyjs::hide('DivContainOpt1')
    shinyjs::hide('DivContainOpt2')
    shinyjs::hide('DivContainOpt4')
    shinyjs::hide('DivContainOpt5')
    shinyjs::hide('GENENETWORK_OPTIONS')
    })

  # FontSize
  observeEvent(input$btn6,{
    js$SetFontSize(input$sld1)
    shinyjs::hideElement("DivContainOpt2")
    js$CyFit();

  })

  # Clear Multihub
  observeEvent(input$ClearTab2, {
    shinyjs::hideElement('DivContainTab2')

  })

  # Clear Wordcloud
  observeEvent(input$ClearPlot1, {
    shinyjs::hideElement('DivContainPlot1')
    shinyjs::showElement('RenderPlot1')
    shinyjs::runjs("$('#DivContainPlot1').css('z-index',-1)")
  })

  # NodeSearch
  observeEvent(input$btn13, {
    v = paste0("#", input$sel1)
    js$SetNode(v);
    idx = which(input$sel1 == GsN)
    res = c()
    for(i in 1:length(cl)){ if(idx%in%cl[[i]]){res = c(res,i)} }
    if(length(res)>1){res = paste0(res, collapse = ',')}

    js$IndicateCluster(c(v,res))
    js$CheckNodeHigh(v)
    shinyjs::delay( ms = 2000, expr = { js$CheckNodeHigh(v) })
    shinyjs::delay( ms = 4000, expr = { js$CheckNodeHigh(v) })
    shinyjs::delay( ms = 6000, expr = { js$CheckNodeHigh(v) })
    shinyjs::delay( ms = 8000, expr = { js$CheckNodeHigh(v) })

  })

  # download network svg form
  observeEvent(input$btn3,{ js$download() })

  # ClusterOption changed
  observeEvent(input$btn8,{
    if(input$sel2 =='MM'){
      v = GetDO(GsM)
      cl <<- GetClust(input$num2,input$num1, v, 1, GsM)
    }
    if(input$sel2 =='pMM'){
      v = GetDOP(GsM, PPI, input$alpha)
      cl <<- GetClust(input$num2,input$num1, v, 2, GsM)
    }
    if(input$sel2 =='Kappa'){
      v = GetDK(GsM)
      cl <<- GetClust(input$num2,input$num1, v, 3, GsM)
    }

    nobj = BuildNetworkObj(cl, GsN, v, input$num2)

    cjn = createCytoscapeJsNetwork(
      nobj$nodeData,
      nobj$edgeData,
      edgeTargetShape = 'none',
      nodeLabelColor = 'black',
      edgeColor = '#001c54',
      nodeShape = 'ellipse',
      nodeHeight = '30',
      nodeWidth = '30'
    )

    output$CY = renderRcytoscapejs( rcytoscapejs(cjn$nodes, cjn$edges, highlightConnectedNodes = FALSE))
    UpdateNodeSearch(session, sort(nobj$nodeData[,"id"]))
    UpdateClusters(session, 1:length(cl))
    ClearCy()

    tab = BuildDT(cl, GsN, GsM, GsQ)
    output$tab1 = DT::renderDataTable(tab)

    shinyjs::hideElement("DivContainOpt3")
  })

  observeEvent(input$btn14,{
    shinyjs::hideElement('DivContainOpt4')

    i = as.numeric(input$menuI)
    genes = unique(unlist(GsM[cl[[i]]]))
    genes = intersect(genes, names(which(GS<ScoreCutoff)))
    E1 = E2 = E3 = E4 = E5 = E6 = 0
    if('Neighborhood' %in% input$che1) E1 = GetColorEdge(genes, l1, 'red')
    if('Gene-fusion' %in% input$che1) E2 = GetColorEdge(genes, l2, 'orange')
    if('Cooccurence' %in% input$che1) E3 = GetColorEdge(genes, l3, 'yellow')
    if('Coexpression' %in% input$che1) E4 = GetColorEdge(genes, l4, 'green')
    if('Experiment' %in% input$che1) E5 = GetColorEdge(genes, l5, 'blue')
    if('Database' %in% input$che1) E6 = GetColorEdge(genes, l6,'navy')
    #E7 = GetColorEdge(genes, l7,'purple')

    nobj = BuildGeneNetwork( genes, input$PPICutoff/1000, PPI, ScoreCutoff )
    suppressWarnings({
        if(E1!=0){nobj$edgeData = rbind(nobj$edgeData, E1)}
        if(E2!=0){nobj$edgeData = rbind(nobj$edgeData, E2)}
        if(E3!=0){nobj$edgeData = rbind(nobj$edgeData, E3)}
        if(E4!=0){nobj$edgeData = rbind(nobj$edgeData, E4)}
        if(E5!=0){nobj$edgeData = rbind(nobj$edgeData, E5)}
        if(E6!=0){nobj$edgeData = rbind(nobj$edgeData, E6)}
        #if(E7!=0){nobj$edgeData = rbind(nobj$edgeData, E7)}
    })

    cjn = createCytoscapeJsNetwork(
      nobj$nodeData,
      nobj$edgeData,
      edgeTargetShape = 'none',
      edgeColor = '#fc7bbe',
      nodeLabelColor = 'black',
      nodeHeight = '30',
      nodeWidth = '30'
    )

    suppressWarnings(
      for(i in 1:length(cjn$nodes)){ cjn$nodes[[i]]$data$href = GetGenecardURL(cjn$nodes[[i]]$data$id) }
    )


    output$CY = renderRcytoscapejs( rcytoscapejs(cjn$nodes, cjn$edges, highlightConnectedNodes = FALSE))

    shinyjs::delay(ms = 2000,{js$SetHref()})

    UpdateNodeSearch(session, sort(nobj$nodeData[,"id"]))
    shinyjs::show("legend2")
    output$legend2 = renderUI( BuildEviLegend() )
    ClearCy(hover = TRUE)

    shinyjs::delay(
      ms = 2000,
      expr = {
        # js$ClearEdge()
        # js$BorderNode()
        js$ColorLabelNode()
        js$StrongEdge()
      }
    )
  })

  # ColorEdges
  observeEvent(input$btn4,{
    shinyjs::hide('DivContainOpt1')
    shinyjs::hide('DivContainOpt2')
    shinyjs::hide('DivContainOpt3')
    shinyjs::hide('DivContainOpt5')
    shinyjs::hide('GENENETWORK_OPTIONS')
    shinyjs::toggleElement("DivContainOpt4",  anim = 'slide', time = 0.5)
    })

  observeEvent(input$menuI,{
    if(input$menuI !='Unselect'){
      i = as.numeric(input$menuI)

      Nodes = GsN[unique(unlist(cl[[i]]))]
      Nodes = paste0("#",Nodes,collapse = ',')
      js$SetNode(Nodes)
      js$CheckNodeHigh(Nodes)
      shinyjs::delay( ms = 2000, expr = { js$CheckNodeHigh(Nodes) })
      shinyjs::delay( ms = 4000, expr = { js$CheckNodeHigh(Nodes) })
      shinyjs::delay( ms = 6000, expr = { js$CheckNodeHigh(Nodes) })
      shinyjs::delay( ms = 8000, expr = { js$CheckNodeHigh(Nodes) })

    }

  })

  observeEvent(input$btn15,{
    shinyjs::hide('DivContainOpt1')
    shinyjs::hide('DivContainOpt2')
    shinyjs::hide('DivContainOpt3')
    shinyjs::hide('DivContainOpt4')
    shinyjs::hide('GENENETWORK_OPTIONS')
    toggleElement("DivContainOpt5",anim='slide',time=0.5)
    })

  observeEvent(input$btn16,{
  	shinyjs::showElement('ClearTab2')
    shinyjs::show('DivContainTab2')
    shinyjs::show("tab2")
	  tab = matrix(0,0,2)

  	for(i in 1:length(cl)){
  		m = cl[[i]]
  		V = v[m,m]
  		r = c()
  		for(j in 1:length(m)){
  			r[j] = length(which(V[j,]<=DC))
  			tab = rbind(tab,c(GsN[m[j]],r[j]))
  		}
  	}
  	colnames(tab) = c("Gene-set","Degree")
  	tab = tab[order(as.numeric(tab[,2]), decreasing = TRUE),]

  	output$tab2 = DT::renderDataTable(DT::datatable(tab[1:5,],
  	colnames = c("Cluster","Degree"),
  	options = list(dom = 't'),
  	selection='single',
    rownames = FALSE)
  	)
  	shinyjs::hideElement("DivContainOpt5")

  })

  observeEvent(input$btn17,{
	  shinyjs::showElement('ClearTab2')
	  shinyjs::hideElement('DivContainOpt5')
    shinyjs::show('DivContainTab2')
	  i = as.numeric(input$menuI)
    genes = unique(unlist(GsM[cl[[i]]]))
	  nobj = BuildGeneNetwork(genes, input$PPICutoff/1000, PPI, ScoreCutoff)
	  v = table(c(nobj$edgeData[,'source'], nobj$edgeData[,'target']))
	  v = cbind(names(v), unname(v))
	  v = v[order(as.numeric(v[,2]), decreasing = TRUE),]
	  output$tab2 = DT::renderDataTable(DT::datatable(v[1:5,],
		colnames = c("Gene","Degree"),
		options = list(dom = 't'),
		rownames = FALSE))
  })

  observeEvent(input$btn1, {
    shinyjs::toggleElement('GENENETWORK_OPTIONS', anim = TRUE, time = 0.5)
    shinyjs::hide('DivContainOpt1')
    shinyjs::hide('DivContainOpt2')
    shinyjs::hide('DivContainOpt3')
    shinyjs::hide('DivContainOpt4')
    shinyjs::hide('DivContainOpt5')
  })
}

shinyApp(ui = ui(), server = server , options = list(launch.browser = TRUE))

