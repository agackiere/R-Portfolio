working_dir <- paste0(getwd(), "/docs/")
timestamp <- Sys.Date()

# render english report
rmarkdown::render(input = paste0("./Secret_file/Edit_portfolio_template.Rmd"),
  output_format = "html_document",
  output_file = paste0("edit_portfolio_template.html"),
  output_dir = working_dir)
