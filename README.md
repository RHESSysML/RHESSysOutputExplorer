
<h1 align="center"> RHESSysOutputExplorer </h1>

<h4 align="center"> A comprehensive workflow to determine and visualize variable importance in RHESSys model output. </h4>

<p align="center">
    <a href="https://github.com/RHESSysML/RHESSysOutputExplorer/commits/main">
    <img src="https://img.shields.io/github/last-commit/RHESSysML/RHESSysOutputExplorer.svg?style=flat-square&logo=github&logoColor=white"
         alt="GitHub last commit">
    <a href="https://github.com/RHESSysML/RHESSysOutputExplorer/issues">
    <img src="https://img.shields.io/github/issues-raw/RHESSysML/RHESSysOutputExplorer.svg?style=flat-square&logo=github&logoColor=white"
         alt="GitHub issues">
    <a href="https://github.com/RHESSysML/RHESSysOutputExplorer/pulls">
    <img src="https://img.shields.io/github/issues-pr-raw/RHESSysML/RHESSysOutputExplorer.svg?style=flat-square&logo=github&logoColor=white"
         alt="GitHub pull requests">
    <img src="https://img.shields.io/github/repo-size/RHESSysML/RHESSysOutputExplorer?style=flat-square"
         alt="GitHub repo size">
</p>
      
<p align="center">
  <a href="#about">About</a> •
  <a href="#how-to-use">How to Use</a> •
  <a href="#choosing-a-model">Choosing a Model</a> •
  <a href="#wiki">Wiki</a> 
</p>

---

## About

<table>
<tr>
<td>
      
RHESSysOutputExplorer provides a template and worked example for quickly exploring and identifying interesting variable relationships in RHESSys output data. This workflow is intended for users after the RHESSys model has been run and calibrated. 
    
In addition to the templates needed to use this workflow for a new dataset, there are two completed workflow implementations included for reference:
    
- Sagehen Creek: Use files/folders ending in `_sagehen` for an example of splitting a RHESSys dataset into two scenarios in order to compare results.
    
- Chap: Use files/folders ending in `_chap` for an example of viewing variable importance and other results from a single model.

**This repository contains the following directories:**    
    
- `R/`: R functions used in the workflow.

- `data/`: Folder to place RHESSys data for use in the workflow. Also contains data sets used in the completed workflow implementations.
    
    - `input/`: contains original RHESSys output dataset and data files resulting from the data preparation notebooks.

    - `output/`: contains data files with model output from workflow notebooks.
    
    - `supporting_docs_data/`: contains data files used in the supporting docs found in `notebooks/supporting_docs`.
    
- `docs`: folder to save RandomForestExplainer HTML output, if specified. Only applicable to the workflow using random forest.

- `notebook_templates/`: Blank workflow notebooks for use with new datasets.

- `notebooks/`: Notebooks used in completed workflow implementations.

  - `supporting_docs/`: Notebooks supporting key choices made in the workflow. 

- `shiny_sagehen/`: contains files and subdirectories associated with the Shiny application. This is the application for Sagehen Creek and can be referenced for similar datasets (split by scenario).
    
- `shiny_chap/`: contains files and subdirectories associated with the shiny_chap interactive visualization application. This can be referenced for more general datasets.

- `renv/`: Files and subdirectories created by the `renv` package.

</td>
</tr>
</table>


## How to Use

1. Fork and clone this repository.     
        
2. Place your RHESSys data in the `data/input/` folder.
        
**If this is your first time using this workflow, we suggest viewing the files within `notebooks` for steps 3-4 for more explanation and an example of a completed analysis.**   
        
3. In `notebook_templates`, use "data_preparation.Rmd" to prepare data. We **suggest aggregating by water year for the best results.** 
        
4. In `notebook_templates`, run "rf_variable_importance.Rmd" or "gb_variable_importance.Rmd". For most use cases, **"rf_variable_importance.Rmd" is preferred**.
        
5. Refer to `shiny_sagehen/` or `shiny_chap` depending on your dataset. Open any of the server.R, global.R, or ui.R files in the shiny application’s directory and clicking “Run App” within R Studio. The app can also be run in a chunk at the bottom of the Rmd from step 4.
        

## Choosing a Model

For most use cases, the random forest method is the recommended machine learning technique of the two provided in this repository. Random forest has proven effective in assessing variable importance in numerous fields including ecology. Additionally, it requires less hyper-parameter tuning than other techniques.
        
Gradient boosting is included as an alternative because it often attains greater predictive accuracy than random forests. This comes with the cost of a more difficult tuning process and a more computationally intensive training process.
        
|                            | Random Forest      | Gradient Boosting |
| -------------------------- | :----------------: | :-------------: |
| Faster run time            |         ✔️         |        ❌        |
| Less tuning                |         ✔️         |        ❌        |
| Accurate predictive power  |         ✔️         |        ✔️        |
| Better maximum accuracy    |         ❌️         |        ✔️        |
        
        
## Wiki

Do you **need some help**? For help specific to this workflow, check the documentation and guidance within `notebooks`. For help with RHESSys, check the _articles_ from the [wiki](https://github.com/RHESSys/RHESSys/wiki/).
