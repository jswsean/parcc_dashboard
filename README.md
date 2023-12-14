Folder structure:
* `final_project`: hosts relevant R scripts and a project documentation which describes the data source, methodology, key insights presented in the dashboard
  * `script`: contains Rscripts 1.1-1.8, which builds the dashboard data in order. Within this folder is the `parcc_score_dashboard` folder, which hosts the Shiny `app` files and all the relevant data files that are used in the dashboard. 
  * `project_documentation`: contains PDF and QMD file for the documentation file.
* `data`: contains intermediate data files used in script 1.8, which builds and copies data files that are directly used in the dashboard. The raw Excel data files are not hosted in this folder due to the file size, but they are accessible at [this link](https://osse.dc.gov/parcc)
