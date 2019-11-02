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