var ThisNode = '';
var m;
var NodeSize = 30;
shinyjs.download = function(){download()}
shinyjs.SetCxtTap = function(){
cy.off("cxttap")
cy.on("cxttap", function(evt){ if(evt.cyTarget === cy){ shinyjs.BoxSelectionToggle(); } })
}

shinyjs.ClearMouseOverNode = function(){ cy.off("mouseover","node") }

shinyjs.ColaLayout = function(rand){
cy.layout({
name:"cola", 
nodeSpacing : function(node){return 3;},
edgeLength : function(edge){return 250;},
animate : true,
randomize : rand[0],
maxSimulationTime : 3000
})
}

shinyjs.CircleLayout = function(){
cy.layout({
name:"circle",
animate : true
})
}

shinyjs.SetFontSize = function(v){
cy.nodes().style("font-size", v+"em");
cy.nodes()[0].activate()
}
/*
shinyjs.SearchNode = function(id){
P = cy.pan();
nodeP = cy.nodes("#"+id).renderedPosition();
cy.pan({x: P.x + cy.width()/2 - nodeP.x, y : P.y + cy.height()/2 - nodeP.y});
}
*/

shinyjs.ToggleElem = function(id){ $("#"+id).toggle("medium") }
shinyjs.HideElem = function(id){ $("#"+id).hide("medium")}
shinyjs.CheckElemOpened = function(id){ return($("#"+id).css("display") == "block") }
shinyjs.BoxSelectionToggle = function(){ cy.boxSelectionEnabled(!cy.boxSelectionEnabled()) }
shinyjs.ClearEdge = function(){ cy.edges().style("opacity",0.5) }
shinyjs.BorderNode = function(){
	cy.nodes().style('border-color','#2c3e50')
	cy.nodes().style('border-width','4px')
}
shinyjs.ColorLabelNode = function(){
	cy.nodes().style('textOutlineColor','white')
	cy.nodes().style('textOutlineWidth','1px')
}
shinyjs.BorderGSNode = function(){
	cy.nodes().style('border-color','#f1592a')
	cy.nodes().style('border-width','4px')
}
shinyjs.DnGSNode = function(id){ 
	cy.nodes(id[0]).style('backgroundColor','#b4eab2') // b4eab2 (GREEN) , 5758BB( BLUE ) 
}

shinyjs.UpGSNode = function(id){
	cy.nodes(id[0]).style('backgroundColor','#F8caf9')	// f8caf9 (EPN PINK), Fda7df ( GS PINK )
}

shinyjs.SetHref = function(){	
	for(var i = 0;i<cy.nodes().length;i++){
		cy.nodes()[i].data('tooltip', 
		"<a target='_blank' href ='" 
		+cy.nodes()[i].data('href')["attribs"]["href"]
		+"'>"
		+"More Info."
		+"</a>"
		)		
	}	
}

shinyjs.SetHeight = function(){
	v = GetPageHeight()*0.85+'px';	
	$(".shiny-spinner-output-container").css("height",v)
	$("#CYCONTAINER").css("height",v)
}

shinyjs.CyFit = function(){ cy.fit(cy.zoom())}

shinyjs.SetClickNode = function(){
	// background : cancle
	cy.on('click', function(e){
		if(e.cyTarget === cy){
			if(HighlightedNode != undefined){
				e = HighlightedNode;
				neighborNodes = e.neighborhood().nodes();
				neighborEdges = e.neighborhood().edges()		
	
				for(var i =0;i<neighborNodes.length;i++){
					neighborEdges = neighborEdges.union(
					neighborNodes[i].edgesWith(neighborNodes.difference(neighborNodes[i]))
					)
				}
								
				for(var i=0;i<neighborNodes.length;i++){ 
					neighborNodes[i].style('background-color',v1[i])
				}
				e.style('background-color',v1[i])
				for(var i=0;i<neighborEdges.length;i++){
					neighborEdges[i].style('line-color',v2[i])
				}
				v1 = [];
				v2 = [];
				HighlightedNode = undefined;
			}
		}
	})
	cy.nodes().off("click")	
	cy.nodes().on('click', function(e){		
		e = e.cyTarget;	
		if(HighlightedNode !=undefined){ // nothing Clicked before
			neighborNodes = HighlightedNode.neighborhood().nodes()
			neighborEdges = HighlightedNode.neighborhood().edges()

			for(var i =0;i<neighborNodes.length;i++){
				neighborEdges = neighborEdges.union(
				neighborNodes[i].edgesWith(neighborNodes.difference(neighborNodes[i]))
				)
			}

			for(var i=0;i<neighborNodes.length;i++){ 
				neighborNodes[i].style('background-color',v1[i])
			}
			HighlightedNode.style('background-color',v1[i])

			for(var i=0;i<neighborEdges.length;i++){
				neighborEdges[i].style('line-color',v2[i])
			}
			v1 = [];
			v2 = [];
								
			HighlightedNode.unselect()
			HighlightedNode = undefined;			
		}
		
		if(!e.selected()){
			HighlightedNode = e;
			// nodes
			neighborNodes = e.neighborhood().nodes()
			for(var i = 0;i<neighborNodes.length;i++){
				v1[i] = neighborNodes[i].style('background-color')
			}
			neighborNodes.style('background-color','#FF00FF')
			v1[i] = e.style('background-color')
			v1 = JSON.parse(JSON.stringify( v1 ))
			e.style('background-color','#F1C40F')

			// edges
			neighborEdges = e.neighborhood().edges()
			for(var i =0;i<neighborNodes.length;i++){
				neighborEdges = neighborEdges.union(
				neighborNodes[i].edgesWith(neighborNodes.difference(neighborNodes[i]))
				)
			}
			
			for(var i =0;i<neighborEdges.length;i++){
				v2[i] = neighborEdges[i].style('line-color').slice()
			}
			neighborEdges.style('line-color','#FF00FF')
			v2 = JSON.parse(JSON.stringify( v2 ))
		}
		else{
			HighlightedNode = undefined;
			neighborNodes = e.neighborhood().nodes();
			neighborEdges = e.neighborhood().edges()

			for(var i =0;i<neighborNodes.length;i++){
				neighborEdges = neighborEdges.union(
				neighborNodes[i].edgesWith(neighborNodes.difference(neighborNodes[i]))
				)
			}

			for(var i=0;i<neighborNodes.length;i++){ 
				neighborNodes[i].style('background-color',v1[i])
			}
			e.style('background-color',v1[i])

			for(var i=0;i<neighborEdges.length;i++){
				neighborEdges[i].style('line-color',v2[i])
			}
			v1 = [];
			v2 = [];
		}
	
		
		
	})
}

shinyjs.StrongEdge = function(){ cy.edges().style('width','3px') }

shinyjs.HighlightTab = function(){	
	v = cy.nodes("#"+$("#tab2 .selected td")[0].innerText)
	if(v.length){ // node exist
		if(HighlightedNode == undefined){ // nothing selected before		
			v.trigger('click')
			v.select()
		}
		else if(HighlightedNode === v){			
			v.trigger('click')
			v.unselect()
		}
		else{	
			// Deselect pre-selected Nodes
			neighborNodes = HighlightedNode.neighborhood().nodes()
			neighborEdges = HighlightedNode.neighborhood().edges()
			for(var i =0;i<neighborNodes.length;i++){ neighborEdges = neighborEdges.union( neighborNodes[i].edgesWith(neighborNodes.difference(neighborNodes[i])) ) }
			for(var i=0;i<neighborNodes.length;i++){ neighborNodes[i].style('background-color',v1[i]) }
			HighlightedNode.style('background-color',v1[i])
			for(var i=0;i<neighborEdges.length;i++){ neighborEdges[i].style('line-color',v2[i]) }
			v1 = [];
			v2 = [];								
			HighlightedNode.unselect()
			HighlightedNode = undefined;			
			v.trigger('click')		
			v.select()		
		}
	}	
}


shinyjs.SetSoftZoom = function(){
	cy.renderer().wheelSensitivity = 0.1
	$(".ui-cytoscape-panzoom").remove()
	cy.panzoom()
}

shinyjs.IndicateCluster = function(input){
	shinyjs.removeCluster();
	input = input[0]
	id = input[0]
	i = input[1]	
	node = cy.nodes(id)
	var newDiv = document.createElement("div");	
	newDiv.id = 'Indicate';
	document.body.appendChild(newDiv);
	var newContent = document.createTextNode("Cluster "+i+'→');
	newDiv.appendChild(newContent)
	newDiv.style.position = "absolute";	
	newDiv.style.left = Number(node.renderedPosition('x'))+100 +'px';
	newDiv.style.top = Number(node.renderedPosition('y'))+50 + 'px';	
	newDiv.style.fontSize= '1.5em'
	newDiv.style.color = '#ff3ef1'		
}

shinyjs.removeCluster = function(){ $("#Indicate").remove();}
shinyjs.HighIndicate = function(){ $("#Indicate").show() }
shinyjs.DownIndicate = function(){ $("#Indicate").hide() }
shinyjs.SetNode = function(v){ 	
	if(ThisNode=='' & v[0]=='#'){		
		return
	} // initial 
	if(ThisNode!=''){			
		if($("#Indicate").length){shinyjs.DownIndicate()}
		shinyjs.DownNode(ThisNode)
	}	
	ThisNode = v[0];
}

shinyjs.HighNode = function(id){
	id = id[0]
	if(id=='#'){return} // unselect ? 	
	cy.nodes(id).style('background-color','#ff3ef1')
	cy.nodes(id).style('width',NodeSize*3+'px')
	cy.nodes(id).style('height',NodeSize*3+'px')
}

shinyjs.DownNode = function(id){	
	id = id[0]
	if(id=='#'){return}
	cy.nodes(id).style('width',NodeSize+'px')
	cy.nodes(id).style('height',NodeSize+'px')	
	id = id.split(',');
	for(var i=0;i<id.length;i++){ cy.nodes(id[i]).style('background-color',m.get(id[i].replace('#',''))) }
}

shinyjs.CheckNodeHigh = function(v){
	var vv = v;
	if(v[0]==ThisNode){
		shinyjs.HighIndicate()
		shinyjs.HighNode(vv)
		setTimeout(function() {
			shinyjs.DownIndicate();
			shinyjs.DownNode(vv);
			}, 1000)
	}
	else{shinyjs.removeCluster()}
}

shinyjs.defineColorMap = function(){
	m = new Map();
	var cyn = cy.nodes();
	for(var i=0;i<cyn.length;i++){ m.put(cyn[i].id(),cyn[i].style('background-color')); }
	return m;
}

shinyjs.SetFontSize = function(v){	
	cy.nodes().style("font-size", v[0]+"em");
	cy.nodes()[0].activate()
	cy.nodes()[0].unactivate()
}

shinyjs.SetNodeSize = function(v){
	NodeSize = v[0];
	cy.nodes().style("height", NodeSize+"px");
	cy.nodes().style("width", NodeSize+"px");
	cy.nodes()[0].activate()
	cy.nodes()[0].unactivate()
}