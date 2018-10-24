# Further Education Outcome Based Success Measures
Shiny App for Outcome Based Success Measures

## Summary
 
This project defines an [R Shiny](https://shiny.rstudio.com/) application that visualises the published [Further Education Outcome Based  Success Measures](https://www.gov.uk/government/collections/statistics-outcome-based-success-measures) data.
 
The application is under development.
 
## What does the application do?
 
The application allows you to do the following:
 
1. Select a Further Education (FE) provider
2. See how it performs compared to other FE providers
3. See detailed outcomes for the selected FE providers learner categories 
 
## Running the project
 
To run the project on your machine you need to do the following:
 
1. Clone the repo.
 
`git clone https://github.com/dfe-analytical-services/fe-outcome-success.git`
 
2. Ensure that you have [Rtools](https://cran.r-project.org/bin/windows/Rtools/) installed. This is so that packrat can install dependencies correctly.
 
3. Open the project by double clicking the fe-outcome-success.Rproj file.
 
4. Wait for packrat to install the packages (this can take a while). Until you see the following do not stop the code:
 
`Packrat bootstrap successfully completed. Restarting R and entering packrat mode...`
   
5. Open the UI.R or Server.R file and hit Run App.

## Data folder

This folder contains the data for the tool. 
- **learner_cat_levels.csv** - list of the learner categories
- **obsm.csv** - detailes provider outcomes by learner categories
- **summary.csv** - summarised provider outcomes by quintiles
- **thresholds.csv** - thresholds for quintiles
