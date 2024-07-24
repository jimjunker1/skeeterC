## this script imports and tidys up raft count data

raft_data = readxl::read_excel(raft_filepath) %>%
  select(-Block, -Mesocosm, -Treatment, -Site)

# identify columns with numbers
raftNumCols = names(raft_data) %>% as.numeric %>% is.na %>% `!`

# rename cols to readible dates
raft_data = raft_data %>%
  rename_with(~as.character(openxlsx::convertToDate(.x, origin='1900-01-01')), .cols = names(.)[raftNumCols])

pred_data = readxl::read_excel(pred_filepath) %>%
  filter(is.na(name) |
         name != 'Total')
predNumCols = names(pred_data) %>% as.numeric %>% is.na %>% `!`

pred_data = pred_data %>%
  rename_with(~as.character(openxlsx::convertToDate(.x, origin='1900-01-01')), .cols = names(.)[predNumCols])

#create a
# count_vec = readxl::read_excel(pred_filepath)%>% filter(name == 'Total')
count_df = raft_data %>% select(which(raftNumCols)) %>% colSums(na.rm = TRUE) %>% enframe(name = 'date', value = 'count')
