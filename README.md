## The effects of time-averaging on archaeological networks

### Abstract
It is well recognized that time-averaging of archaeological deposits results in significant biases in interpretations of the archaeological record. In this study, we investigate the biases introduced by time-averaging in the study of social and economic networks from the archaeological record. Using three different archaeological network datasets, we combine network slices from multiple periods to mimic the effects of time-averaging to understand how the palimpsest nature of the archaeological record affects our interpretations of the network. The results of our analysis indicate that time-averaging reduces the fidelity of network interpretations compared to the non-time-averaged networks when analyzing network or node properties. Our results also showed that the effects of time-averaging are highly dependent on initial network structures. This makes it difficult to establish general rules for how to interpret time-averaged networks in archaeology. However, our study shows that it is of paramount importance that archaeologists are aware of these biases and evaluate the reliability of their data accordingly.

### Data  
##### Prignano  
Settlement and road network dataset developed by Prignano and colleagues (2019) for southern Etruria from 950 to 500 BC  

Networks dated to   
   * the Early Iron Age 1 Early (950/925–900 BC),   
   * the Early Iron Age 1 Late (900–850/825 BC),   
   * the Early Iron Age 2 (850/825–730/720 BC),   
   * the Orientalizing Age (730/720–580 BC), and   
   * the Archaic Period (580–500 BC)  

##### Chaco  
Chaco Social Networks database (Peeples et al. 2016); apportioned ceramic data and associated similarity matrices from Mills and colleagues (2018)  

Ceramic assemblages were apportioned into 25-year intervals, which cover Late Pueblo I (PI) period through the end of the Pueblo III (PIII) period    

##### ICRATES  
Material distribution data from the ICRATES dataset (Bes 2015; Bes et al. 2019)  

Nearly 34,000 diagnostic tableware sherds dated to the Late Hellenistic and Roman periods from 275 sites throughout the Eastern Mediterranean (Bes et al. 2019)  

### Scripts  
Scripts to run to recreate figures:  
* Chaco-network-analysis.R  
* Chaco-node-analysis.R  
* ICRATES-network-analysis.R  
* ICRATES-node-analysis.R  
* Prignano-network-analysis.R  
* Prignano-node-analysis.R  

The above scripts were run on HPC clusters. 

All other scripts are helper scripts with functions for time averaging, calculating network and node metrics, and getting timeslices for the ICRATES dataset.    

### Figures
**/figures/metrics**      
For each dataset, the following figures are available:  
* original-network-metrics  
     Plot showing the network measures for each original graph in the dataset    
* original-<metric name>-density  
     Plots of the density distribution of node metrics for each original graphs in a dataset  
* original-degree-dist
     Plots of the degree distribution for each of the original graphs in a dataset  

**/figures/node-centrality**  
For each dataset, the following figure is available:  
* <dataset name>-centrality-similarity  
     Plots of how similar the 5 nodes with the highest or lowest degree, betweenness, and eigenvector centrality values for the time-averaged graphs are to the top or bottom 5 nodes from the original graphs  

**/figures/null-models**  
For each dataset, the following figures are available:  
* me_ta-to-orig  
   Comparison of network metric values for each time-averaged graph to a null model based on the original graphs  
* all_ta_me  
   Comparison of the network metric values for each time-averaged graph to null models based on the time-averaged graphs  

**/figures/pca**  
For each dataset, the following figure is available:  
* pca-biplot  
   PCA based on the network metric values for the time-averaged graphs with the original graphs (blue squares) plotted on  


 
**ICRATES and Chaco scripts were run using the NYU Greene HPC Cluster on 2x Intel Xeon Platinum 8268 24C 205W 2.9GHz processors**  
   https://sites.google.com/a/nyu.edu/nyu-hpc/systems/greene-cluster  
**Prignano scripts were run on Emily Coco's personal MacBook Air on a 1.4 GHz Dual-Core Intel Core i5 processor**  
