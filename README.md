# Analyzing Chronic Condition Risk Factors

This R package provides tools and workflows to analyze and model chronic health risks using data from the Canadian Community Health Survey (CCHS 2019–2020).

## Authors
- Shubhpreet
- Jay Mewada
- Parabhuda Deore

## Course
**Data Analysis STAT 4620/5620 – Winter 2024–2025**

## GitHub Repository
<https://github.com/ShubhpreetDal/AnalyzingChronicConditionGroup5>

## Features

- Data decoding and cleaning for CCHS 2019–2020
- Categorical transformation and descriptive visualizations
- Bivariate analysis with chi-square tests
- Modeling pipelines:
  - Regularized Logistic Regression
  - Decision Trees
  - Generalized Additive Models (GAMs)
- Perceived health vs actual condition analysis
- Final ROC plots and confusion matrices

## How to Run

1. Install required packages (see `DESCRIPTION`).
2. Load the package: 
```
devtools::load_all()
```
3. Run the full pipeline:
```
source("run_pipeline.R")
```

## Final Report

[View the final PDF report](vignettes/final_report.pdf)

Render the report manually:
```
quarto::quarto_render("vignettes/final_report.qmd", output_format = "pdf")
```
