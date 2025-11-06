# Oceanographic Data Analysis using R - 2-Week Course Module

## Course Overview
**Duration:** 2 weeks (10 working days)  
**Total Hours:** 40 hours (4 hours per day)  
**Target Audience:** Beginners with no prior R or programming experience  
**Prerequisites:** Basic computer literacy  
**Mode:** Hands-on practical training with real oceanographic datasets

---

## Week 1: Foundations & Data Preparation

### Day 1: R Basics & Environment Setup
**Duration:** 4 hours

| Aspect | Details |
|--------|---------|
| **Learning Outcomes** | • Install and configure R and RStudio<br>• Understand R interface and console basics<br>• Learn fundamental R syntax and operations<br>• Create and execute first R scripts |
| **Content Topics** | • R installation and RStudio environment<br>• Console, script editor, and workspace<br>• R markdown introduction<br>• Basic operators and syntax<br>• Variables, data types (numeric, character, logical) |
| **Practical Activities** | • Install R and RStudio on personal computer<br>• Explore RStudio layout and features<br>• Write and execute basic calculations<br>• Create and run a simple R script<br>• Use help and documentation functions |
| **Key R Packages** | tidyverse (overview), base R |
| **Assessment** | Hands-on verification: Successful installation and execution of demo script |

### Day 2: Data Structures & Data Exploration
**Duration:** 4 hours

| Aspect | Details |
|--------|---------|
| **Learning Outcomes** | • Create and manipulate vectors, matrices, lists, and data frames<br>• Load and explore oceanographic datasets<br>• Understand tidy data principles<br>• Apply summary and exploration functions |
| **Content Topics** | • Vectors and their operations<br>• Matrices and lists<br>• Data frames structure and properties<br>• Reading CSV and Excel files<br>• Data exploration functions (str, head, summary, glimpse)<br>• Intro to tidy data principles |
| **Practical Activities** | • Create different data structures<br>• Load sample oceanographic CTD data (CSV)<br>• Examine data structure with str() and summary()<br>• Identify variable types and distributions<br>• Handle basic missing values (NA)<br>• Calculate basic statistics |
| **Key R Packages** | readr, base R, tibble |
| **Assessment** | Lab exercise: Load provided CTD dataset, explore structure, document findings |

### Day 3: Data Cleaning & Transformation with dplyr
**Duration:** 4 hours

| Aspect | Details |
|--------|---------|
| **Learning Outcomes** | • Master dplyr's key verbs (select, filter, arrange, mutate)<br>• Use pipe operator for chained operations<br>• Aggregate and summarize data<br>• Prepare data for analysis |
| **Content Topics** | • dplyr verbs: select, filter, arrange, mutate, summarize<br>• Pipe operator (%>%) and workflow<br>• Group operations (group_by)<br>• Data aggregation and summarization<br>• Handling duplicates and outliers<br>• Creating derived variables |
| **Practical Activities** | • Select relevant oceanographic variables<br>• Filter data by depth, quality, or other criteria<br>• Arrange observations chronologically<br>• Calculate density from temperature and salinity<br>• Group data by sampling locations/depths<br>• Summarize by groups (mean, SD by layer)<br>• Remove or flag questionable data |
| **Key R Packages** | dplyr, tidyr, tidyverse |
| **Assessment** | Lab exercise: Clean and transform real oceanographic dataset; create analysis-ready data frame |

### Day 4: Data Visualization Fundamentals
**Duration:** 4 hours

| Aspect | Details |
|--------|---------|
| **Learning Outcomes** | • Create publication-quality plots with ggplot2<br>• Visualize different data types appropriately<br>• Customize plots with themes and aesthetics<br>• Understand grammar of graphics |
| **Content Topics** | • ggplot2 fundamentals and layered architecture<br>• Geoms (geom_point, geom_line, geom_boxplot, etc.)<br>• Aesthetics mapping (color, size, shape, fill)<br>• Faceting and subplots<br>• Themes and customization<br>• Color palettes and scales |
| **Practical Activities** | • Plot scatter plots (temperature vs. salinity)<br>• Create line plots (parameter vs. depth)<br>• Generate distribution plots (histograms, boxplots)<br>• Use faceting for multiple locations/conditions<br>• Customize colors, labels, and legends<br>• Combine multiple plots with gridExtra<br>• Export high-resolution figures for publication |
| **Key R Packages** | ggplot2, gridExtra, tidyverse |
| **Assessment** | Lab exercise: Create 3+ exploratory plots from oceanographic dataset; export publication-ready figures |

### Day 5: Oceanographic Parameters & Advanced Data Import
**Duration:** 4 hours

| Aspect | Details |
|--------|---------|
| **Learning Outcomes** | • Understand key oceanographic parameters and measurement techniques<br>• Import complex data formats (netCDF, HDF5)<br>• Process CTD and ADCP data<br>• Work with oceanographic data sources |
| **Content Topics** | • Physical parameters: temperature, salinity, pressure, density<br>• Biological parameters: dissolved oxygen, chlorophyll-a, fluorescence<br>• CTD instrument principles and profiles<br>• ADCP acoustic measurements<br>• netCDF and HDF5 file structures<br>• Data sources: ERDDAP, NASA Oceancolor, CMEMS<br>• Data quality flags and validation |
| **Practical Activities** | • Import netCDF oceanographic data<br>• Read and process CTD depth profiles<br>• Extract specific layers (surface, intermediate, deep)<br>• Apply quality flags and filters<br>• Convert units (e.g., pressure to depth)<br>• Handle missing data markers<br>• Create visualizations from multi-dimensional data |
| **Key R Packages** | oce (oceanographic analysis), ncdf4, rhdf5, stars, tidyverse |
| **Assessment** | Lab exercise: Import and explore multi-format oceanographic data; compare different data types |

---

## Week 2: Analysis & Advanced Applications

### Day 6: Time Series Analysis Foundations
**Duration:** 4 hours

| Aspect | Details |
|--------|---------|
| **Learning Outcomes** | • Create and work with time series objects<br>• Detect trends and seasonal patterns<br>• Perform time-based aggregation<br>• Visualize temporal patterns in oceanographic data |
| **Content Topics** | • Time series object creation and properties<br>• Frequency and temporal resolution<br>• Resampling and interpolation methods<br>• Trend detection and decomposition<br>• Seasonal pattern identification<br>• Gap handling in time series |
| **Practical Activities** | • Create time series from in-situ measurements<br>• Aggregate data at different temporal scales (hourly, daily)<br>• Interpolate gaps in measurements<br>• Decompose series into trend and seasonal components<br>• Plot multi-parameter time series<br>• Identify anomalies and extreme events<br>• Compare seasonal variations across years |
| **Key R Packages** | ts, zoo, lubridate, tseries, tidyverse |
| **Assessment** | Lab exercise: Time series analysis of oceanographic monitoring data with trend and seasonal decomposition |

### Day 7: Descriptive Statistics & Hypothesis Testing
**Duration:** 4 hours

| Aspect | Details |
|--------|---------|
| **Learning Outcomes** | • Calculate and interpret descriptive statistics<br>• Test statistical assumptions<br>• Perform correlation and regression analysis<br>• Make inference from oceanographic data |
| **Content Topics** | • Descriptive statistics (mean, median, SD, IQR)<br>• Distribution properties and visualization<br>• Normality testing (Shapiro-Wilk, Q-Q plots)<br>• Correlation analysis (Pearson, Spearman)<br>• Hypothesis testing framework<br>• Linear regression models<br>• Model diagnostics and interpretation |
| **Practical Activities** | • Summarize oceanographic variables by groups<br>• Test normality of key parameters<br>• Analyze correlations between T, S, DO<br>• Test hypotheses (e.g., differences between water masses)<br>• Fit linear models (temperature vs. depth)<br>• Assess statistical significance<br>• Interpret oceanographic relationships |
| **Key R Packages** | stats, broom, tidyverse |
| **Assessment** | Lab exercise: Conduct statistical analysis with hypothesis testing and regression; interpret results |

### Day 8: Multivariate Analysis & Water Mass Classification
**Duration:** 4 hours

| Aspect | Details |
|--------|---------|
| **Learning Outcomes** | • Perform Principal Component Analysis (PCA)<br>• Apply clustering techniques<br>• Identify and classify water masses<br>• Interpret multivariate oceanographic patterns |
| **Content Topics** | • Principal Component Analysis (PCA) principles<br>• Variance explained and component interpretation<br>• Hierarchical clustering methods<br>• k-means and fuzzy clustering<br>• Water mass properties and classification<br>• Multivariate visualization techniques<br>• Biplot interpretation |
| **Practical Activities** | • Perform PCA on multi-parameter CTD datasets<br>• Create and interpret biplots<br>• Cluster water samples by physical/chemical properties<br>• Identify distinct water masses from profiles<br>• Validate clustering results<br>• Compare clustering methods (hierarchical vs. k-means)<br>• Visualize water mass distributions |
| **Key R Packages** | vegan, FactoMineR, factoextra, ggplot2 |
| **Assessment** | Lab exercise: Multivariate analysis identifying water masses; clustering and PCA visualization |

### Day 9: Spatial Analysis & Oceanographic Mapping
**Duration:** 4 hours

| Aspect | Details |
|--------|---------|
| **Learning Outcomes** | • Create professional oceanographic maps<br>• Perform spatial interpolation<br>• Visualize spatial patterns and distributions<br>• Work with geographical coordinate systems |
| **Content Topics** | • Coordinate systems and map projections<br>• Spatial data types (sf, raster)<br>• Bathymetric data and bottom topography<br>• Spatial interpolation (Kriging, IDW)<br>• Oceanographic mapping conventions<br>• Satellite and modeled data visualization<br>• Station plots and contour maps |
| **Practical Activities** | • Create oceanographic maps with ggOceanMaps<br>• Plot survey station locations with measurements<br>• Interpolate CTD profiles across space<br>• Create contour/heatmaps of parameters<br>• Map water mass distributions<br>• Overlay bathymetry and data<br>• Create multi-panel oceanographic visualizations<br>• Export map products for publication/reports |
| **Key R Packages** | ggOceanMaps, sf, stars, ggplot2, raster |
| **Assessment** | Lab exercise: Create 2-3 publication-quality oceanographic maps showing spatial patterns |

### Day 10: Capstone Project & Reproducible Research Best Practices
**Duration:** 4 hours

| Aspect | Details |
|--------|---------|
| **Learning Outcomes** | • Integrate all course concepts in comprehensive analysis<br>• Implement reproducible research workflow<br>• Communicate findings effectively<br>• Create professional data analysis reports |
| **Content Topics** | • Reproducible research principles and workflows<br>• R markdown dynamic document generation<br>• Project organization and file structure<br>• Code documentation and commenting<br>• Version control basics (git/GitHub overview)<br>• Presenting oceanographic analysis results<br>• Creating comprehensive data reports |
| **Practical Activities** | • Conduct capstone analysis integrating 2+ weeks of learning<br>• Generate R markdown report with embedded analysis<br>• Create professional tables and figures<br>• Document data processing steps<br>• Write methods and results sections<br>• Create automated report generation<br>• Present findings to peers with Q&A<br>• Receive and give feedback |
| **Key R Packages** | rmarkdown, knitr, tidyverse, oce |
| **Assessment** | Capstone project: Comprehensive analysis report including data, code, methodology, visualizations, and conclusions |

---

## Course Learning Objectives Summary

By the end of this 2-week course, participants will be able to:

### Technical Skills
- Set up and use R and RStudio efficiently
- Load, clean, and transform oceanographic datasets
- Create publication-quality visualizations and maps
- Perform descriptive statistics and hypothesis testing
- Conduct multivariate analysis on oceanographic data
- Work with complex data formats (netCDF, HDF5)
- Perform spatial analysis and oceanographic mapping
- Generate reproducible research reports

### Conceptual Understanding
- Understand key oceanographic parameters and their relationships
- Recognize appropriate statistical methods for oceanographic data
- Interpret water mass characteristics from CTD profiles
- Apply remote sensing concepts to ocean analysis
- Evaluate data quality and limitations

### Professional Competencies
- Organize and document oceanographic data analysis projects
- Create reproducible analytical workflows
- Communicate scientific findings effectively
- Follow best practices in scientific computing
- Apply domain knowledge to real oceanographic problems

---

## Assessment Strategy

### Formative Assessment (Ongoing)
- Daily hands-on lab exercises with real oceanographic data
- Peer review of visualizations and analysis approaches
- Q&A sessions and troubleshooting
- Progress checks using provided datasets

### Summative Assessment (End of Course)
- **Week 1:** Individual data cleaning and visualization projects
- **Week 2:** Multivariate and spatial analysis labs
- **Capstone Project:** Comprehensive oceanographic analysis report (30-50 pages)
  - Includes data documentation, methodology, R code, figures, and conclusions
  - Must integrate concepts from multiple course modules
  - Professional presentation quality

---

## Software & Data Requirements

### Software (Free/Open Source)
- R (v4.1.0 or higher)
- RStudio (free version)
- Git/GitHub (optional, for version control)

### R Packages (Core)
- **Data Management:** tidyverse, dplyr, tidyr, readr
- **Oceanographic Analysis:** oce, gsw
- **Visualization:** ggplot2, ggOceanMaps, gridExtra
- **Spatial Analysis:** sf, stars, raster
- **Statistical Analysis:** vegan, FactoMineR, stats, broom
- **Time Series:** zoo, ts, lubridate, tseries
- **Data Import:** ncdf4, rhdf5
- **Reporting:** rmarkdown, knitr

### Datasets Provided
- Sample CTD profiles (temperature, salinity, dissolved oxygen)
- ADCP current measurements
- Ocean color data (chlorophyll-a, SST)
- Sea level time series
- Historical oceanographic databases

---

## Prerequisites & Target Audience

**Ideal for:**
- Oceanography and marine science students
- Environmental/climate researchers
- Oceanographic data managers
- Marine resource professionals
- Anyone interested in R for scientific data analysis

**No prior experience required in:**
- R or Python programming
- Statistical analysis
- Oceanography (though basic understanding of ocean physics helpful)

**Required:**
- Computer with internet access
- Ability to install software
- Willingness to engage with coding and data analysis
- Time commitment (40 hours over 2 weeks)

---

## Course Materials & Resources

### Provided Materials
- R scripts and templates for all lessons
- Oceanographic datasets in multiple formats
- R cheat sheets (dplyr, ggplot2, etc.)
- RMarkdown templates
- Reference documentation for all packages

### Recommended Resources
- "Oceanographic Analysis with R" by Kelley (2018)
- "R for Data Science" by Wickham & Grolemund
- NOAA CoastWatch satellite data tutorials
- Oceanographic data portals documentation

### Support
- Daily instructor support during lab sessions
- Email/Slack help desk for troubleshooting
- Peer collaboration during group exercises
- Access to compiled course notes and recordings (where applicable)

---

## Schedule Notes

- **Daily Structure:** 4 hours per day with 15-minute breaks
- **Flexibility:** Topics can be adjusted based on participant experience level
- **Pace:** Designed for beginners; advanced participants can explore additional topics
- **Software:** All software is free and cross-platform (Windows, Mac, Linux)
- **Post-Course:** Graduates can access course materials and updates for reference

