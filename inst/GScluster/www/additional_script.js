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

function GetPageHeight()
{
    var body = document.body;
    var html = document.documentElement;
    var height = Math.max( body.scrollHeight, body.offsetHeight, html.clientHeight, html.scrollHeight, html.offsetHeight );
    return height;
}
