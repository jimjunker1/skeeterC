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

## DO sat plots

logger_df %>%
  select(loggerID, treatment, time = `Central Standard Time (none)`, do_sat = `Dissolved Oxygen Saturation (%)` ) %>%
  filter(!is.na(do_sat)) %>%
  ggplot()+
  geom_line(aes(x = time, y = do_sat, group = loggerID, color = loggerID))+
  facet_wrap(~treatment)+
  theme(legend.position = 'none')

## DO plots

logger_df %>%
  select(loggerID, treatment, time = `Central Standard Time (none)`, do = `Dissolved Oxygen (mg/l)` ) %>%
  filter(!is.na(do)) %>%
  ggplot()+
  geom_line(aes(x = time, y = do, group = loggerID, color = loggerID))+
  facet_wrap(~treatment)+
  theme(legend.position = 'none')

## temp plots

logger_df %>%
  select(loggerID, treatment, time = `Central Standard Time (none)`, do = `Dissolved Oxygen Saturation (%)`,
         temp = `Temperature (deg C)`) %>%
  filter(!is.na(do)) %>%
  ggplot()+
  geom_line(aes(x = time, y = temp, group = loggerID, color = loggerID))+
  facet_wrap(~treatment)+
  theme(legend.position = 'none')

# Model the temperature timeseries above threshold
logger_list = logger_df %>%
  select(loggerID, treatment, time = `Central Standard Time (none)`, do_sat = `Dissolved Oxygen Saturation (%)`,
         temp_C = `Temperature (deg C)`) %>%
  junkR::named_group_split(loggerID)

x = logger_list[[4]] %>%
  mutate(time_seq = 1:n())

plot(x$time[250:1000], x$temp_C[250:1000])

smoothsplineData = smooth.spline(x[,c('time_seq','temp_C')])
# smoothData = smooth(x$temp_C)
# loessData = loess(temp_C ~ time_seq, span = 0.005, data = x)
plot(smoothsplineData$y[250:1000], type ='l', col = 'blue')
lines(x$time_seq, x$temp_C, col = 'red', add = TRUE)
# calculate metabolism

working_loggerID = "026311"

# clean timeseries to deployment time

ts026311 = logger_df %>%
  filter(loggerID %in% working_loggerID,
         between(`Central Standard Time (none)`,
                 as.POSIXct(unlist(logger_meta[which(logger_meta$`logger id` == working_loggerID), 'deploy'])), as.POSIXct("2024-07-22 18:00:00"))) %>%
  select(loggerID, block, treatment, time = `Central Standard Time (none)`,
         temp = `Temperature (deg C)`,
         do_mgL = `Dissolved Oxygen (mg/l)`,
         do_sat = `Dissolved Oxygen Saturation (%)`) %>%
  mutate(depth = 11.5)

# estimate k time series
k600_cole = k.cole(ts026311 %>% select(datetime = "time") %>% mutate(wnd = 5))
k.gas = k600.2.kGAS.base(k600_cole$wnd, ts026311$temp, 'O2')

do_sat = o2.at.sat.base(ts026311$temp, 201)

irr = streamMetabolizer::calc_light()

metab026311 <- metab.bookkeep(ts026311$do_mgL,
                              do.sat = do_sat,
                              k.gas = k.gas,
                              ts026311$time)



