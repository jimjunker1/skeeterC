# initiate set ups
source(here::here("code/01_setup.R"))
i_am("code/05_metabolism.R")

library(LakeMetabolizer)

## read in the logger metadata
logger_meta <- readxl::read_excel(metab_meta_filepath, col_types = c('text',
                                                                     'numeric',
                                                                     'text',
                                                                     'text',
                                                                     'text',
                                                                     'text',
                                                                     'date',
                                                                     'date',
                                                                     'date',
                                                                     'date',
                                                                     'text')) %>%
  rename_with(tolower) %>%
  mutate(`deploy time` = gsub("\\d{4}-\\d{2}-\\d{2} (\\d{2}\\:\\d{2}\\:\\d{2})", "\\1", `deploy time`),
         `collection time` = gsub("\\d{4}-\\d{2}-\\d{2} (\\d{2}\\:\\d{2}\\:\\d{2})", "\\1", `collection time`)) %>%
  unite("deploy", `deploy date`:`deploy time`, sep = " ", remove = TRUE) %>%
  unite("collection", `collection date`:`collection time`, sep = " ", remove = TRUE) %>%
  mutate(deploy = as.POSIXct(deploy, format = '%Y-%m-%d %H:%M:%S'),
         collection = as.POSIXct(collection, format = '%Y-%m-%d %H:%M:%S'),
         `logger id` = as.character(`logger id`))

# read in the logger data
logger_filepath = list.files(metab_folderpath, all.files = TRUE, recursive = TRUE, full.names = TRUE)
logger_names = gsub(".*/DO logger data/(\\d{6}).TXT","\\1",logger_filepath)

logger_df = logger_filepath%>%
  map(\(txt) read_miniDOT(txt, list = FALSE)) %>%
  setNames(nm = logger_names) %>%
  bind_rows(.id = "loggerID") %>%
  left_join(logger_meta %>% select(loggerID = 'logger id', block, mesocosm, treatment),., by = "loggerID")

## DO plots

logger_df %>%
  select(loggerID, treatment, time = `Central Standard Time (none)`, do = `Dissolved Oxygen Saturation (%)` ) %>%
  filter(!is.na(do)) %>%
  ggplot()+
  geom_line(aes(x = time, y = do, group = loggerID, color = loggerID))+
  facet_wrap(~treatment)+
  theme(legend.position = 'none')

