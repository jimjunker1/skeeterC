# script to run daily report

rmarkdown::render(here::here("code/skeeter-forecast-template.Rmd"),
                  output_format = "pdf_document",
                  output_dir = here::here("forecasts/"),
                  output_file = paste0(min(Sys.Date(),as.Date("2024-07-13")),"_skeeter-forecast.pdf"),
                  knit_root_dir = here::here(),
                  params = list(),
                  clean = FALSE)

