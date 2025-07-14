# Spatial Point Pattern Models for Opportunistic Cetacean Data in the Azores

**Using Bayesian Spatial Point Processes to Model Cetacean Distribution from Presence-Only Data**  
*Master’s Program in Biostatistics – Universitat de València (2024/2025)*  
**Author:** Diego Fernández-Fernández  

---

## 📌 Project Overview

This repository contains the R code and resources used in the Master's thesis. The project explores and models the spatial distribution of cetaceans using Bayesian Log-Gaussian Cox Processes (LGCP) based on opportunistic presence-only data. The focus is on the **Azores archipelago**.

---

## 📁 Repository Structure

Each folder is numbered in execution order:

- `01_download_occurrences.R`: Download OBIS / GBIF / iNaturalist data
- `02_data_filtering.R`: Clean OBIS / GBIF / iNaturalist data downloaded
- `03_env_variables.R`: Download and process environmental `.nc` data
- `04_model_LGCP.R`: Fit LGCP models and get prediction maps
- `outputs/`: Maps and model summaries

---

## 🧠 Methods Summary

- **Inference**: Log-Gaussian Cox Process (LGCP) via Integrated Nested Laplace Approximation (INLA)
- **Software**: [R](https://www.r-project.org/), [`inlabru`](https://inlabru-org.github.io/inlabru/), [R-INLA](https://www.r-inla.org/), [sf](https://cran.r-project.org/web/packages/sf/index.html), [terra](https://cran.r-project.org/web/packages/terra/index.html)
- **Covariates**: Bathymetry, SST, Mixed Layer Depth, Sea Surface height anomaly
- **Data Sources**:
  - Occurrences: [OBIS](https://obis.org), [GBIF](https://www.gbif.org), [iNaturalist](https://www.inaturalist.org/)
  - Environment: [Copernicus Marine Service](https://marine.copernicus.eu/), [GEBCO](https://www.gebco.net/)
- **Target species**: *Delphinus delphis* (Common Dolphin), *Physeter macrocephalus* (Sperm Whale)

---

## 🚀 Getting Started

To run the project, follow the script order from `01` to `04`. You’ll need:

### Requirements

- R version >= 4.3
- R packages: `sf`, `terra`, `tidyverse`, `ncdf4`, `stars`, `lubridate`, `viridis`
- INLA:  
```r
install.packages("INLA", repos = c(getOption("repos"),
  INLA = "https://inla.r-inla-download.org/R/stable"))
