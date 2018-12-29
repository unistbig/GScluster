

shinyjs.download = function(){download()}

shinyjs.SetCxtTap = function(){
cy.off("cxttap")
cy.on("cxttap", function(evt){
if(evt.cyTarget === cy){
shinyjs.BoxSelectionToggle();
}
})
}

shinyjs.ClearMouseOverNode = function(){
cy.off("mouseover","node")
}

shinyjs.ColaLayout = function(){
cy.layout({
name:"cola", 
nodeSpacing : function(node){return 3;},
edgeLength : function(edge){return 250;},
animate : true,
randomize : false,
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
}
/*
shinyjs.SearchNode = function(id){
P = cy.pan();
nodeP = cy.nodes("#"+id).renderedPosition();
cy.pan({x: P.x + cy.width()/2 - nodeP.x, y : P.y + cy.height()/2 - nodeP.y});
}
*/

shinyjs.HighNode = function(id){
id = id[0]
//cy.nodes(id).style('border-color','#e74c3c') // red
//cy.nodes(id).style('border-width','4px')

col = cy.nodes(id).style('background-color')

cy.nodes(id).style('background-color','#ff3ef1')
}

shinyjs.DownNode = function(id){
id = id[0]

cy.nodes(id).style('background-color',col)

//cy.nodes("#"+id).style("height",'30px');
//cy.nodes("#"+id).style("width",'30px');
//cy.nodes(id).style('border-width','0px')

}

shinyjs.ToggleElem = function(id){
$("#"+id).toggle("medium")
}

shinyjs.HideElem = function(id){
$("#"+id).hide("medium")
}

shinyjs.CheckElemOpened = function(id){
return($("#"+id).css("display") == "block")
}

shinyjs.BoxSelectionToggle = function(){
cy.boxSelectionEnabled(!cy.boxSelectionEnabled())
}

shinyjs.ClearEdge = function(){
cy.edges().style("opacity",0.5)
}

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
	console.log(v);
	$("#CYCONTAINER").css("height",v)
}