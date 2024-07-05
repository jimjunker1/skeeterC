# script to run daily report

rmarkdown::render(here::here("R/skeeter-forecast-template.Rmd"),
                  output_format = "pdf_document",
                  output_dir = here::here("forecasts/"),
                  output_file = paste0(Sys.Date(),"_skeeter-forecast.pdf"),
                  knit_root_dir = here::here(),
                  params = list(),
                  clean = TRUE)

