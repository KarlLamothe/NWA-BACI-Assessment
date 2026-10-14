# Effects of aquatic vegetation removal on abiotic and biotic conditions in a dyked wetland

## Authors

Karl A. Lamothe, Robin C. Gáspárdy, Jason Barnucz, and D. Andrew R. Drake

## Overview

This work assesses the fish community composition, water quality, and vegetation data from a Before-After-Control-Impact study at the St. Clair National Wildlife Area on the removal of White Water Lily in the East Cell.

## Abstract

Achieving wetland management objectives often requires active interventions such as water-level drawdown or physical vegetation removal, which can have consequences on biodiversity. Here, we present the results of a two-year study investigating the effects of White water lily (*Nymphaea* sp.) removal on ecological conditions in a dyked wetland situated within a federally protected National Wildlife Area in southern Ontario, Canada that supports an endangered freshwater fish species, Lake Chubsucker (*Erimyzon sucetta*). A before-after-control-impact study design was used to assess the ecological effects of a 10 ha mechanical removal of White water lily in the East cell (‘treatment’) of the wetland relative to the West cell (‘control’) between 2023 and 2024. Water quality characteristics, the proportional cover of aquatic vegetation, vegetation composition, and fish community composition were monitored in both cells for both years. Overall, fish community structure remained relatively constant between years and wetland cells, with 13 species detected in each cell in each year. Fewer Lake Chubsucker were captured in 2024 than 2023 in both cells, but a linear model of catch-per-unit-effort indicated no significant year by wetland cell interaction effect. Annual differences in water quality were observed, with turbidity and conductivity declining by a greater magnitude in the East cell than the West cell from 2023 to 2024. In contrast, annual differences in composition were observed, but consistently across cells, suggesting an inability to attribute this change to the White water lily removal efforts. While mechanical removal efforts were successful at thinning the White water lily monoculture in the East cell, few ecological differences could be attributed to these efforts. Additional years of sampling would provide a greater opportunity to understand the success of the management action at promoting submerged aquatic vegetation growth and the long-term effects on wetland conditions and species.

## Repository Structure

```text
├── README.md
├── .gitignore
|
├── Data/
│   ├── 2023-2024-LCS-NWA_VegetationData (2026-09-22).csv
│   ├── Depth.csv
│   ├── Fish-TL-W(20260902).csv
│   ├── Fish-captures(20260902).csv
│   ├── Fish.total.csv
│   ├── LatLong.csv
│   ├── Rake_data_full_revised_commonnames.csv
│   ├── Site-information(20260902).csv
│
├── Results/
│   ├── Figures/
│   │   ├── All.NMDS.png
│   │   ├── ECDF.length.classes.png
│   │   ├── LW.Model.coefficients.png
│   │   ├── Loggers.png
│   │   ├── Rake.weight.volume.png
│   │   ├── Veg.boxplots.png
│   │   ├── WaterQ.boxplots.png
│   │   ├── spec.accum.gg.png
│   ├── Fish.LW.model.coefs.csv
│   └── Water.Quality.Models.csv
|
├── Rscript00-Packages-Theme.R
├── Rscript00-Veg-data-cleaning.R
├── Rscript00-miniDOT-Cleaning.R
├── Rscript01-Map.R
├── Rscript02-Water-Quality.R
├── Rscript03-Vegetation.R
├── Rscript04-Vegetation.Rake.R
├── Rscript05-Fish-Multivariate.R
├── Rscript06-Fish-Length-Weight.R
└── Rscript07-Loggers.R
```

## How to use:

The analysis is organized as a RProject with individual scripts. To use this file:
1. Clone the repository.
2. Open the .Rproj file in RStudio.
3. Run desired RScript.
