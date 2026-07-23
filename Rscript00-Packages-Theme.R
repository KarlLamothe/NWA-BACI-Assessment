# List of packages being used
list.of.packages <- c('ggplot2', 'dplyr', 'DHARMa','tidyr','vegan','reshape2',
                      'emmeans','broom','sf','terra','tidyterra','maptiles',
                      'ggspatial','cowplot','FSAmisc','patchwork','BiodiversityR')

# Identify packages in the list that are not on the computer
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# Install packages in "new.packages"
if(length(new.packages)) install.packages(new.packages); rm(list.of.packages); rm(new.packages)

# packages
library(ggplot2)
library(broom)
library(vegan)
library(dplyr)
library(reshape2)
library(DHARMa)
library(emmeans)
library(tidyr)
library(sf)
library(terra)    
library(tidyterra)    
library(maptiles)    
library(ggspatial)   
library(cowplot)    
library(FSAmisc)
library(patchwork)
library(BiodiversityR)

# set ggplot theme
theme_set(theme_bw() +
            theme(axis.title   = element_text(size=10,   family="sans", colour="black"),
                  axis.text.x  = element_text(size=9.5, family="sans", colour="black"),
                  axis.text.y  = element_text(size=9.5, family="sans", colour="black"),
                  strip.text   = element_text(size=9.5,   family="sans", colour="black"),
                  plot.title   = element_text(size=11,   family="sans", colour="black"),
                  panel.border = element_rect(colour="black")))
