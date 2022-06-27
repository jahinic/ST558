
## R script just to render our .Rmd file
## Author: John Hinic

rmarkdown::render(
  "Project1/Hinic_Project1.Rmd", 
  output_format = "github_document",
  output_file = "README.md"
)
