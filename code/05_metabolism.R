# initiate set ups
source(here::here("code/01_setup.R"))
i_am("code/05_metabolism.R")

raft_filepath = sprintf("C:/Users/%s/OneDrive - UNT System/AERI Seed Insect Carbon/Data/2024_aeri_seed_mosquito_data.xlsx", Sys.info()[['user']])

library(streamMetabolizer)
library(LakeMetabolizer)
library(junkR)

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

logger_labels = logger_df %>%
  select(loggerID, treatment, block) %>%
  distinct() %>%
  mutate(label = paste(treatment," Block: ", block," ID: ",loggerID, sep = ""))
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
  filter(!grepl("NA",loggerID)) %>%
  select(loggerID, treatment, block, time = `Central Standard Time (none)`, do_sat = `Dissolved Oxygen Saturation (%)`, do_mgL = `Dissolved Oxygen (mg/l)`,
         temp_C = `Temperature (deg C)`) %>%
  junkR::named_group_split(loggerID)

##
# debugonce(tempFill_custom)
# y = x %>%
#   named_group_split(day) %>%
#   purrr::map(~tempFill_custom(.x, too_hot = 36.1)) %>%
#   bind_rows(.id = "day")

logger_df = logger_list %>%
  map(~.x %>%
        mutate(day = as.Date(time, tz = 'America/Chicago')) %>%
        named_group_split(day) %>%
        map(~tempFill_custom(.x, too_hot = 36.2 )) %>%
        bind_rows(.id = 'day')) %>%
  bind_rows(.id = "loggerID")

logger_df %>%
  select(loggerID, treatment, time,
         temp_C) %>%
  ggplot()+
  geom_line(aes(x = time, y = temp_C, group = loggerID, color = loggerID))+
  facet_wrap(~treatment)+
  theme(legend.position = 'none')

# calculate metabolism
## split by loggerID
logger_metabDfList = logger_df %>%
  named_group_split(loggerID) %>%
  purrr::map(\(x){
    k600_cole = k.cole(x %>% select(datetime = "time") %>% mutate(wnd = 5))
    cat()
    k_gas = k600.2.kGAS.base(k600_cole$wnd, x$temp_C, 'O2')
    do_sat = o2.at.sat.base(x$temp_C, altitude = 201)
    irr = streamMetabolizer::calc_light(calc_solar_time(x$time,
                                                        longitude = -97.133064),
                                        latitude = 33.214840,
                                        longitude = -97.133064
                                        )
    irr_bin = ifelse(irr > 0, 1,0)
    depth = 10.5/100
    wtr = x$temp_C
    lake.lat = 33.214840
    data.frame(datetime = x$time,
               do.obs = x$do_mgL,
               do.sat = do_sat,
               k.gas = k_gas,
               z.mix = depth,
               irr = irr,
               irr.bin = irr_bin,
               wtr = wtr,
               lake.lat = lake.lat) %>%
      mutate(k.gas = ifelse(wtr > 46.8, 10, k.gas))
  })

## bookkeeping metabolism
logger_bookkeep_metabList = logger_metabDfList %>%
  purrr::map(\(x){
    y = x %>% mutate(day = as.Date(datetime, tz = 'America/Chicago')) %>% named_group_split(day) %>% .[-c(1,length(.))] %>% purrr::map(~data.frame(.x)) %>% bind_rows()

    metab(y, method = "bookkeep", wtr.name = 'wtr', irr.name = 'irr.bin', do.obs.name = 'do.obs')
  })

logger_bookkeep_metabDf = logger_bookkeep_metabList %>%
  bind_rows(.id = 'loggerID')

logger_bookkeep_metabDf %>%
  left_join(logger_labels, by = "loggerID") %>%
  mutate(label = factor(label)) %>%
  ggplot()+
  ggtitle("Bookkeeping")+
  geom_hline(yintercept = 0)+
  geom_line(aes(x = doy, y = GPP), color = 'green')+
  geom_point(aes(x = doy, y = GPP), shape = 21, color = 'black', fill = 'green')+
  geom_line(aes(x = doy, y = R), color = 'brown')+
  geom_point(aes(x = doy, y = R), shape = 21, color = 'black', fill = 'brown')+
  ylab(expression("GPP or ER ( mg"~O[2]~d^-1~")"))+
  facet_wrap(~label, labeller = 'label_value')+
  theme(axis.title.x = element_blank())

## maximum likelihood metabolism
logger_mle_metabList = logger_metabDfList %>%
  purrr::map(\(x){
    y = x %>% mutate(day = as.Date(datetime, tz = 'America/Chicago')) %>% named_group_split(day) %>% .[-c(1,length(.))] %>% purrr::map(~data.frame(.x)) %>% bind_rows()

    metab(y, method = "mle", wtr.name = 'wtr', irr.name = 'irr.bin', do.obs.name = 'do.obs')
  })

logger_mle_metabDf = logger_mle_metabList %>%
  bind_rows(.id = 'loggerID')

logger_mle_metabDf %>%
  left_join(logger_labels, by = "loggerID") %>%
  mutate(label = factor(label)) %>%
  ggplot()+
  ggtitle("MLE")+
  geom_hline(yintercept = 0)+
  geom_line(aes(x = doy, y = GPP), color = 'green')+
  geom_point(aes(x = doy, y = GPP), shape = 21, color = 'black', fill = 'green')+
  geom_line(aes(x = doy, y = R), color = 'brown')+
  geom_point(aes(x = doy, y = R), shape = 21, color = 'black', fill = 'brown')+
  ylab(expression("GPP or ER ( mg"~O[2]~d^-1~")"))+
  facet_wrap(~label, labeller = 'label_value')+
  theme(axis.title.x = element_blank())

## ols metabolism
logger_ols_metabList = logger_metabDfList %>%
  purrr::map(\(x){
    y = x %>% mutate(day = as.Date(datetime, tz = 'America/Chicago')) %>% named_group_split(day) %>% .[-c(1,length(.))] %>% purrr::map(~data.frame(.x)) %>% bind_rows()

    metab(y, method = "ols", wtr.name = 'wtr', irr.name = 'irr.bin', do.obs.name = 'do.obs')
  })

logger_ols_metabDf = logger_ols_metabList %>%
  bind_rows(.id = 'loggerID')

logger_ols_metabDf %>%
  left_join(logger_labels, by = "loggerID") %>%
  mutate(label = factor(label)) %>%
  ggplot()+
  ggtitle("OLS")+
  geom_hline(yintercept = 0)+
  geom_line(aes(x = doy, y = GPP), color = 'green')+
  geom_point(aes(x = doy, y = GPP), shape = 21, color = 'black', fill = 'green')+
  geom_line(aes(x = doy, y = R), color = 'brown')+
  geom_point(aes(x = doy, y = R), shape = 21, color = 'black', fill = 'brown')+
  ylab(expression("GPP or ER ( mg"~O[2]~d^-1~")"))+
  facet_wrap(~label, labeller = 'label_value')+
  theme(axis.title.x = element_blank())

## kalman filter metabolism
logger_kalman_metabList = logger_metabDfList %>%
  purrr::map(\(x){
    y = x %>% mutate(day = as.Date(datetime, tz = 'America/Chicago')) %>% named_group_split(day) %>% .[-c(1,length(.))] %>% purrr::map(~data.frame(.x)) %>% bind_rows()

    metab(y, method = "kalman", wtr.name = 'wtr', irr.name = 'irr.bin', do.obs.name = 'do.obs')
  })

logger_kalman_metabDf = logger_kalman_metabList %>%
  bind_rows(.id = 'loggerID')

logger_kalman_metabDf %>%
  left_join(logger_labels, by = "loggerID") %>%
  mutate(label = factor(label)) %>%
  ggplot()+
  ggtitle("Kalman")+
  geom_hline(yintercept = 0)+
  geom_line(aes(x = doy, y = GPP), color = 'green')+
  geom_point(aes(x = doy, y = GPP), shape = 21, color = 'black', fill = 'green')+
  geom_line(aes(x = doy, y = R), color = 'brown')+
  geom_point(aes(x = doy, y = R), shape = 21, color = 'black', fill = 'brown')+
  ylab(expression("GPP or ER ( mg"~O[2]~d^-1~")"))+
  facet_wrap(~label, labeller = 'label_value')+
  theme(axis.title.x = element_blank())

## Bayesian metabolism
logger_bayes_metabList = readRDS(here::here("data/models/logger_bayes_metabList.rds"))
#   logger_metabDfList %>%
#   purrr::map(\(x){
#     y = x %>% mutate(day = as.Date(datetime, tz = 'America/Chicago')) %>% named_group_split(day) %>% .[-c(1,length(.))] %>% purrr::map(~data.frame(.x)) %>% bind_rows()
#
#     metab(y, method = "bayesian", wtr.name = 'wtr', irr.name = 'irr.bin', do.obs.name = 'do.obs')
#   })
# saveRDS(logger_bayes_metabList, here::here("data/models/logger_bayes_metabList.rds"))
logger_bayes_metabDf = logger_bayes_metabList %>%
  bind_rows(.id = 'loggerID')

logger_bayes_metabDf %>%
  left_join(logger_labels, by = "loggerID") %>%
  mutate(label = factor(label)) %>%
  ggplot()+
  ggtitle("Bayesian")+
  geom_hline(yintercept = 0)+
  geom_line(aes(x = doy, y = GPP), color = 'green')+
  geom_point(aes(x = doy, y = GPP), shape = 21, color = 'black', fill = 'green')+
  geom_line(aes(x = doy, y = R), color = 'brown')+
  geom_point(aes(x = doy, y = R), shape = 21, color = 'black', fill = 'brown')+
  ylab(expression("GPP or ER ( mg"~O[2]~d^-1~")"))+
  facet_wrap(~label, labeller = 'label_value')+
  theme(axis.title.x = element_blank())

##### End metabolism models----
### Summarise metabolism data by treatment -----

logger_bayes_metabDf %>%
  left_join(logger_meta %>% rename(loggerID = 'logger id'), by = "loggerID") %>%
  group_by(treatment, doy) %>%
  mutate(R = ifelse(GPP < 0, R + GPP, R)) %>%
  mutate(GPP = ifelse(GPP < 0, 0, GPP)) %>%
  summarise(GPP = mean(GPP, na.rm = TRUE),
            R = mean(R, na.rm = TRUE)) %>%
  ggplot()+
  geom_hline(aes(yintercept = 0))+
  geom_line(aes(x = doy, y = GPP, group = treatment, color = treatment), linewidth = 2) +
  geom_line(aes(x = doy, y = R, group = treatment, color = treatment), linetype = 'dashed', linewidth = 2)


raft_data = readxl::read_excel(raft_filepath) %>%
  select(-Block, -Mesocosm, -Treatment, -Site)

# identify columns with numbers
raftNumCols = names(raft_data) %>% as.numeric %>% is.na %>% `!`

# rename cols to readable dates
raft_data = raft_data %>%
  rename_with(~as.character(openxlsx::convertToDate(.x, origin='1900-01-01')), .cols = names(.)[raftNumCols]) %>%
  filter(!is.na(ID))

pred_data = readxl::read_excel(pred_filepath, skip = 1) %>%
  filter(is.na(name) |
           name != 'Observed')
predNumCols = names(pred_data) %>% as.numeric %>% is.na %>% `!`

pred_data = pred_data %>%
  rename_with(~as.character(openxlsx::convertToDate(.x, origin='1900-01-01')), .cols = names(.)[predNumCols]) %>%
  mutate(across(-c(name), as.numeric))

# count_vec = readxl::read_excel(pred_filepath)%>% filter(name == 'Total')
count_df = raft_data %>% select(which(raftNumCols)) %>% colSums(na.rm = TRUE) %>% enframe(name = 'date', value = 'count') %>%
  mutate(date = as.Date(date))

current_date = ifelse(Sys.Date() > as.Date("2024-07-13"), as.character("2024-07-13"), as.character(Sys.Date()))

name_df = c('GD' = 'Grace',
            'AM' = 'Arya',
            'EAC' = 'Emily',
            'JD' = 'Dr. D',
            'JMR' = 'Julia',
            'JRJ' = 'Jim',
            'JRB' = 'Dr. B',
            'ZC' = 'Z',
            'ARM' = 'Angel',
            'CRG' = 'Chris',
            'JK' = 'Juwan')
pred_plotDf = pred_data %>%
  pivot_longer(-name, names_to = 'date', values_to = 'prediction') %>%
  mutate(date = as.Date(date),
         old = ifelse(as.Date(date)<(Sys.Date()+1), TRUE, FALSE))

# set plotting attributes
ylims = c(0,10^(signif(log10(max(c(count_df$count,na.omit(pred_plotDf$prediction)))))))

labelsDf = data.frame(date = as.numeric(count_df$date), datelab = as.character(format(count_df$date, "%b-%d")), day = paste0("Day ",1:nrow(count_df)))# %>% unite('label', date:day, sep = "") %>% unlist %>% unname

# do the plotting
ggplot()+
  geom_point(data = pred_plotDf, aes(x = as.numeric(date), y = prediction, color = old), shape = 21, fill = 'white', size = 1.2)+
  geom_line(data = count_df , aes(x = as.numeric(date), y = count ), linewidth = 1.1, color = 'black')+
  scale_y_continuous(name = "Skeeter Rafts (#)", limits = ylims)+
  scale_x_continuous(breaks = labelsDf$date, labels = labelsDf$datelab, sec.axis = dup_axis(labels = labelsDf$day))+
  scale_color_manual(values = c('black','darkgrey'))+
  theme(axis.title.x = element_blank(),
        legend.position = 'none',
        axis.text.x.bottom = element_text(size = 10, angle = 45, hjust = 1, vjust = 1),
        axis.text.x.top = element_text(size = 10, angle = 45, hjust = 0, vjust = 0),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank())





x = logger_metabDfList[[1]] %>% mutate(day = as.Date(datetime, tz = 'America/Chicago')) %>% named_group_split(day) %>% .[-c(1,length(.))] %>% purrr::map(~data.frame(.)) %>%  bind_rows()

x_split = x %>% named_group_split(day) %>% purrr::map(~.x %>% select(-day))
y = x_split[[1]]
with(y, metab.bookkeep(datetime = datetime,
                       do.obs = do.obs,
                       do.sat = do_sat,
                       k.gas = k.gas,
                       irr = irr.bin,
                       z.mix = z.mix,
                       wtr = wtr))

#%>% purrr::map(~metab(.x, method = 'bookkeep', irr.name = 'irr.bin'))

working_loggerID = "026311"

# clean timeseries to deployment time

ts026311 = logger_df %>%
  filter(loggerID %in% working_loggerID,
         between(time,
                 as.POSIXct(unlist(logger_meta[which(logger_meta$`logger id` == working_loggerID), 'deploy'])), as.POSIXct("2024-07-22 18:00:00"))) %>%
  select(loggerID, block, treatment, time,
         temp_C,
         do_mgL,
         do_sat) %>%
  mutate(depth = 10.5)

# estimate k time series
k600_cole = k.cole(ts026311 %>% select(datetime = "time") %>% mutate(wnd = 5))
k.gas = k600.2.kGAS.base(k600_cole$wnd, ts026311$temp_C, 'O2')

do_sat = o2.at.sat.base(ts026311$temp_C, 201)

irr = streamMetabolizer::calc_light(calc_solar_time(ts026311$time, longitude = -97.133064),
                                    latitude = 33.214840,
                                    longitude = -97.133064
                                    )
irr = ifelse(irr > 0, 0,1)


ts026311_metab = data.frame(do.obs = ts026311$do_mgL,
                            do.sat = do_sat,
                            k.gas = k.gas,
                            z.mix = ts026311$depth/100,
                            irr = irr,
                            datetime = ts026311$time) %>%
  mutate(day = as.Date(datetime, tz = 'America/Chicago')) %>%
  named_group_split(day) %>% .[-c(1,length(.))]

metab026311 <- ts026311_metab %>%
  purrr::map(~with(.x,
                   metab.bookkeep(do.obs = do.obs,
                                  do.sat = do_sat,
                                  k.gas = k.gas,
                                  irr = irr,
                                  z.mix = z.mix,
                                  datetime = datetime)))

x = ts026311_metab[[1]]

x_metab = with(x,metab.bookkeep(do.obs = do.obs,
                 do.sat = do.sat,
                 k.gas = k.gas,
                 irr = irr,
                 z.mix = z.mix,
                 datetime = datetime))

