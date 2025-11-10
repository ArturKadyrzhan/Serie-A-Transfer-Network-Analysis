# ⚽ Serie A Transfer Network Analysis (2016–17 Summer Window)

> Final project for **Data Management – Unit 2**,
> exploring the Italian Serie A transfer market through **network science**, **graph theory**, and **data mining** in **R**.

This project was developed as part of the **Data Management Unit 2** course at Sapienza University of Rome.  
Theoretical foundation and methodologies were inspired by:
> **Barabási, A.-L. (2016). *Network Science*. Cambridge University Press.**
> To view the presentation of the project:
https://prezi.com/p/fgatikcnbslg/network-analysis-of-serie-a-teams-summer-transfer-season-2016-2017/



##  Project Overview
The goal was to model and analyze the **Serie A 2016–17 summer transfer market**  
as a **directed graph**, where:
- **Nodes** represent football clubs.
- **Edges** represent player transfers between clubs.

By analyzing structural properties (degree, clustering, connectedness) and comparing with league results,  
the project uncovers how **transfer activity and club influence** correlate with **on-field performance**.


##  Methodology

### 1️ Data Sources
- `transfer_data.csv` — transfer transactions  
- `SerieAclassifica15-16.csv` — previous season standings  
- `SerieAclassifica16-17.csv` — current season results  
- `SerieA16-17predictedtableBBC.csv` — predicted rankings (BBC dataset)

### 2️ Tools & Libraries
All analysis was performed in **R**, using:
```r
library(dplyr)
library(tidyr)
library(ggplot2)
library(igraph)
library(network)
library(intergraph)
library(readr)
