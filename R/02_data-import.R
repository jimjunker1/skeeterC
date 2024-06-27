## this script imports and tidys up raft count data

raft_data = readxl::read_excel(raft_filepath) %>%
  select(-Block, -Mesocosm, -Treatment, -Site)

# identify columns with numbers
excelNumCols = names(raft_data) %>% as.numeric %>% is.na %>% `!`

# rename cols to readible dates
raft_data = raft_data %>%
  rename_with(~as.character(openxlsx::convertToDate(.x, origin='1900-01-01')), .cols = names(.)[excelNumCols])


