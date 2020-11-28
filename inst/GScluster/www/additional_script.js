var v1 = [];
var v2 = [];
var HighlightedNode;

function download(){
	var fileName = 'graph.svg'
	var k = document.createElement("a");
	k.id="dl_temp"

	var create_svg = cy.svgConvertor();
	var svg = $("#create svg");
	svg = svg[0];
	var serializer = new XMLSerializer();
	var source = serializer.serializeToString(svg);
	if(!source.match(/^<svg[^>]+xmlns="http\:\/\/www\.w3\.org\/2000\/svg"/)){ source = source.replace(/^<svg/, '<svg xmlns="http://www.w3.org/2000/svg"');}
	if(!source.match(/^<svg[^>]+"http\:\/\/www\.w3\.org\/1999\/xlink"/)){ source = source.replace(/^<svg/, '<svg xmlns:xlink="http://www.w3.org/1999/xlink"');}
	source = '<?xml version="1.0" standalone="no"?>\r\n' + source;
	var url = "data:image/svg+xml;charset=utf-8,"+encodeURIComponent(source);
	k.href = url;
	k.download = fileName;

	if (window.navigator.msSaveBlob) { // IE
		var blob = new Blob([source], {type:  "text/plain;charset=utf-8;"});
		window.navigator.msSaveBlob(blob, fileName)
	}
	else if (navigator.userAgent.search("Firefox") !== -1) { // Firefox
		k.click()
	}
	else { // Chrome
		k.click()
	}
	$("#create svg").remove()
	$("#dl_temp").remove()
}

function GetPageHeight(){
    var body = document.body;
    var html = document.documentElement;
    var height = Math.max( body.scrollHeight, body.offsetHeight, html.clientHeight, html.scrollHeight, html.offsetHeight );
    return height;
}

// refered http://atspeed.blogspot.kr/2011/01/javascript-hashmap.html
Map = function(){ this.map = new Object() };
Map.prototype = {
    put : function(key, value){ this.map[key] = value;},
    get : function(key){ return this.map[key]; },
    containsKey : function(key){ return key in this.map;},
    containsValue : function(value){
     for(var prop in this.map){ if(this.map[prop] == value) return true;}
     return false;
    },
    isEmpty : function(key){ return (this.size() == 0);},
    clear : function(){ for(var prop in this.map){ delete this.map[prop];}},
    remove : function(key){ delete this.map[key]; },
    keys : function(){
        var keys = new Array();
        for(var prop in this.map){ keys.push(prop);}
        return keys;
    },
    values : function(){
     var values = new Array();
        for(var prop in this.map){ values.push(this.map[prop]);}
        return values;
    },
    size : function(){
      var count = 0;
      for (var prop in this.map) { count++; }
      return count;
    },
    toString : function(){
      var s=[];
      for(var prop in this.map){ s.push(prop+':'+this.map[prop]);}
      return s.join(',');
    }
};

var ThisNode = '';
var m;
var NodeSize = 30;

shinyjs.download = function(){download()}

shinyjs.SetCxtTap = function(){
	cy.off("cxttap")
	cy.on("cxttap", function(evt){ if(evt.cyTarget === cy){ shinyjs.BoxSelectionToggle(); } })
}

shinyjs.ColaLayout = function(rand){
	cy.layout({
		name:"cola",
		nodeSpacing : function(node){return 3;},
		edgeLength : function(edge){return 250;},
		animate : true,
		randomize : rand[0],
		maxSimulationTime : 3000
		}).run()
}

shinyjs.CircleLayout = function(){
	cy.layout({
	name:"circle",
	animate : true
	}).run()
}

shinyjs.DagreLayout = function(){
	cy.layout({ name:'dagre', animate : true, animationDuration: 3000 }).run()
}

shinyjs.KlayLayout = function(){
	cy.layout({name:'klay', animate : true, animationDuration : 1500}).run()
}

shinyjs.SpreadLayout = function(){
	cy.layout({name:'spread'}).run()
}


shinyjs.SetFontSize = function(v){
	cy.nodes().style("font-size", v+"em");
	cy.nodes()[0].activate()
}

shinyjs.ToggleElem = function(id){ $("#"+id).toggle("medium") }
shinyjs.HideElem = function(id){ $("#"+id).hide("medium")}
shinyjs.CheckElemOpened = function(id){ return($("#"+id).css("display") == "block") }
shinyjs.BoxSelectionToggle = function(){ cy.boxSelectionEnabled(!cy.boxSelectionEnabled()) }
shinyjs.ClearEdge = function(){ cy.edges().style("opacity",0.5) }
shinyjs.BorderNode = function(){
	cy.nodes().style('border-color','#2c3e50')
	cy.nodes().style('border-width','4px')
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

shinyjs.SetHeight = function(){
	v = GetPageHeight()*0.85+'px';
	$(".shiny-spinner-output-container").css("height",v)
	$("#CYCONTAINER").css("height",v)
}

shinyjs.CyFit = function(){ cy.fit(cy.zoom())}

shinyjs.SetClickNode = function(){
	// background : cancle
	cy.on('click', function(e){
		if(e.target === cy){
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
		e = e.target;
		if(HighlightedNode != undefined){ // nothing Clicked before
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
	var newContent = document.createTextNode("Cluster"+i+'→');
	newDiv.appendChild(newContent)
	newDiv.style.position = "absolute";
	newDiv.style.backgroundColor = 'white';
	newDiv.style.border = '2px solid #48DBFB';
	newDiv.style.padding = '2px 2px 0px 2px';
	newDiv.style.left = Number(node.renderedPosition('x'))+100 +'px';
	newDiv.style.top = Number(node.renderedPosition('y'))+50 + 'px';
	newDiv.style.fontSize= '1.5em'
	newDiv.style.color = 'black'
	//newDiv.style.color = '#ff3ef1'

}

shinyjs.removeCluster = function(){ $("#Indicate").remove();}
shinyjs.HighIndicate = function(){
	$("#Indicate").css('left',Number(cy.nodes(ThisNode).renderedPosition('x'))+100 + 'px')
	$("#Indicate").css('top',Number(cy.nodes(ThisNode).renderedPosition('y'))+50 + 'px')
	$("#Indicate").show()
}
shinyjs.DownIndicate = function(){ $("#Indicate").hide() }


shinyjs.DownNode = function(id){
	id = id[0]
	if(id=='#'){return}
	cy.nodes(id).style('width',NodeSize+'px')
	cy.nodes(id).style('height',NodeSize+'px')
	id = id.split(',');
	for(var i=0;i<id.length;i++){ cy.nodes(id[i]).style('background-color',m.get(id[i].replace('#',''))) }
}

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
	cy.nodes(id).style('width',NodeSize+20+'px')
	cy.nodes(id).style('height',NodeSize+20+'px')
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
