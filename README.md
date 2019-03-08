GScluster is a standalone Shiny/R package for network-weighted gene-set clustering and network visualization.<br>

Installation
=========
Open R program and type following commands in R console.

> install.packages('devtools') <br> 
> library(devtools) <br>
> install_github('unistbig/GScluster') <br>
> library(GScluster) <br>

Example Run
=========
<b>To run demo file, please type following R code.</b><br>
> GScluster()
<br>
<b>To run user's own data, please modify following codes.</b><br>
1) Read gene-set analysis result table.<br>
> GSAresult=read.delim('https://github.com/unistbig/GScluster/raw/master/sample_geneset.txt', stringsAsFactors=FALSE)<br>
2) Read gene score table.<br>
> GeneScores=read.delim('https://github.com/unistbig/GScluster/raw/master/sample_genescore.txt', header=F)<br>
3) Run GScluster<br>
> GScluster(GSAresult = GSAresult, GeneScores = GeneScores, Species = 'H', alpha = 1, GsQCutoff = 0.25, GQCutoff = 0.05)<br>

User's Manual
===========
User's manual is available <a href = "https://github.com/unistbig/GScluster/raw/master/GScluster_manual_v1.1.6.pdf" target = "_blank">here</a>.

Example data download
===========
* Diagram Data
<br><a href = 'https://github.com/unistbig/GScluster/raw/master/inst/GScluster/sample_geneset.txt' target = "_blank">Gene-set analysis result file</a> (q-value cutoff: 0.25)<br>
<a href = 'https://github.com/unistbig/GScluster/raw/master/inst/GScluster/sample_genescore.txt' target = "_blank">Genescore file</a> (q-value cutoff: 0.01)<br>

* Colon Cancer Data
<br><a href = 'https://github.com/unistbig/GScluster/raw/master/sample_geneset2.txt' target = "_blank">Gene-set ananlysis result file</a> (q-value cutoff: 0.01) <br>
<a href = 'https://github.com/unistbig/GScluster/raw/master/sample_genescore2.txt' target = "_blank">Genescore file</a>  (q-value cutoff: 0.01) <br>

***
<b>Running time</b> of GScluster is shown below for different numbers of input gene-sets.
<img src = 'https://user-images.githubusercontent.com/6457691/52764859-0a0d8880-3065-11e9-9ee2-b14d77c28d8e.png' width =400></img><br>
***

<b>Contact: dougnam@unist.ac.kr <br></b>
Any feedback or comments are greatly appreciated!!<br>

