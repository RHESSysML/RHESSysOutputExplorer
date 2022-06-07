
<h1 align="center"> RHESSysOutputExplorer R Shiny App</h1>

<h4 align="center"> An interactive application that allows users to view their results from the RHESSysOutputExplorer workflow, as well as futher explore their datasets.</h4>

<p align="center">
  <a href="#about">About</a> •
  <a href="#how-to-use">How to Use</a> •
</p>

---

## About

<table>
<tr>
<td>
      
The RHESSysOutputExplorer R Shiny App allows RHESSys users to view their variable importance results from the RHESSysOutputExplorer workflow. The app also allows for users to use those results in order to further analyze and explore their datasets. 

This repository contains the following directories:    
    
- `www/`: Folder containing any images (jpeg, png, etc.) being used within the app.

- `ui.R`: Script that contains the code that builds the user interface for the app.

- `server.R`: Script that contains the code that builds the server for the app.

- `global.R`: Script that contains the code that builds all of the global options for the code including importing the data and creating user input options.

- `metadata.Rmd`: R Markdown document that contains the code that creates the user's metadata. The metadata is originally built for the RHESSysOutputExplorer Sagehen Creek case study, but this Rmd allows RHESSys users to edit the metadata to fit their own datasets.

- `aggregated_datasets/`: Folder that contains all of the modified datasets from the RHESSysOutputExplorer workflows. These datasets should be automatically updated and sent to this folder when a user goes through the workflows.

</td>
</tr>
</table>

## How to Use

This app was designed to be as simple as possible for RHESSys users. When a user goes through the RHESSysOutputExplorer workflows, all of their results will be automatically exported to the app, and the user shouldn't have to do anything other than hit run. In order to run the app, the user has to open one of three scripts, either the `ui.R`, `server.R`, or `global.R`. Once one of these files is open, the user just hits the "Run App" button in the upper right corner. The app can also be run via the command line using `R -e “shiny::runApp(‘/shiny’)”`. Once the app opens up, the user will be brought to the app "Welcome!" page where there will be more information about the app itself. 

Users may also be interested in having the correct metadata for their data in the app, which they will have to do themselves. If the user choses not to worry about the metadata, the app will still function just fine, but it may be slightly less informative. The initial metadata data table is designed for the RHESSysOutputExplorer Sagehen Creek case study, but the `metadata.Rmd` file makes it easy for users to edit the metadata to match their own dataset. First the user will open the `metadata.Rmd` file and run it. This will create a data table that contains each variable along with its fullname, its units, and its description. Once the data table is created, the `metadata.Rmd` file also contains two functions that make it easy for the user to add and remove variables from the data table. So the user can go through and remove all variables that their original dataset doesn't contain, and add any variables that aren't in the example metadata. After the metadata is set up to their own dataset, the `metadata.Rmd` code will save the RDS table, and the Shiny app will automatically use it.
