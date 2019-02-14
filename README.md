GScluster Readme
===========

GScluster is a standalone R package for clustering, network analysis and visualize of Gene-Set Analysis result. <br>

To Run GScluster appropriately, User should download Reference datas.<br>
[Download from Here](http://github.com/unistbig/GScluster-Data/) <br>
or use **DownloadData** function (**Recommended**) in GSCluster package.<br>

***
Also, user need to install rcytoscapejs package manually.
[link](https://github.com/cytoscape/cyjShiny/releases)

1. Download rcytoscapejs package (either tar.gz or zip)
![rcyjs1](https://user-images.githubusercontent.com/6457691/52613929-45784d80-2ed3-11e9-8b82-e64dba293f0f.png)

2. Install and Load library
![rcyjs2](https://user-images.githubusercontent.com/6457691/52613932-47421100-2ed3-11e9-80ab-76a8d2930e97.png)

***
Example datas.<br>
Diagram Data
<a href = 'https://github.com/unistbig/GScluster/raw/master/inst/GScluster/sample_geneset.txt' target = "_blank">Geneset file</a>
<a href = 'https://github.com/unistbig/GScluster/raw/master/inst/GScluster/sample_genescore.txt' target = "_blank">Genescore file</a>
<br>
Colon Cancer Data, 0.1 or 0.05 recommended for Geneset Qvalue cutoff.
<a href = 'https://github.com/unistbig/GScluster/raw/master/sample_geneset2.txt' target = "_blank">Geneset file</a>
<a href = 'https://github.com/unistbig/GScluster/raw/master/sample_genescore2.txt' target = "_blank">Genescore file</a>
<br>

***
GScluster's Expected runningtime increases as gene-set size increases.
Here are their brief relationship plotted.
<img src = 'https://user-images.githubusercontent.com/6457691/52764859-0a0d8880-3065-11e9-9ee2-b14d77c28d8e.png' width = 600></img><br>
It takes ~ 10 min within 1500 input genesets.
***

# Additional info can accessed via User manual.
