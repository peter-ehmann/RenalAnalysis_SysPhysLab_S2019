library(readxl)
library(tidyverse)

# set working directory to where the dataset is located and read in data
# download the Excel sheet (.xlsx) from ...
setwd("set_working_directory")
df <- read_excel("./Original_Data.xlsx", sheet = 1)

# move information in these 5 columns of the 2nd row up a row
df[1,2:6] = df[2,2:6]

# change group name, and fill in NA values with appropriate group name
for (i in 1:length(df$Variable)) {
  if (is.na(df$Variable[i]) & i > 3) {
    df$Variable[i] = df$Variable[i-1]
  } else if (str_detect(df$Variable[i], "^200 mL")) {
    df$Variable[i] = "Salt"
  } else if (str_detect(df$Variable[i], "^1.0 L")) {
    df$Variable[i] = "H2O"
  } 
}

# remove 1st and 2nd row - no information provided
df <- df[-c(1,2),]

# switch information in first and second columns
df[,c(1,2)] <- df[,c(2,1)]

# format column names
columns <- c("Subject_ID", "Group_Name", "Group_ID", "Section", "Section_Day", "Section_Time", 
             "Flow_Rate_0", "Flow_Rate_30", "Flow_Rate_60", "Flow_Rate_90", 
             "Specific_Gravity_0", "Specific_Gravity_30", "Specific_Gravity_60", "Specific_Gravity_90", 
             "NaCl_conc_0", "NaCl_conc_30", "NaCl_conc_60", "NaCl_conc_90", 
             "NaCl_exr_rate_0", "NaCl_exr_rate_30", "NaCl_exr_rate_60", "NaCl_exr_rate_90", 
             "Osmolality_0", "Osmolality_30", "Osmolality_60", "Osmolality_90")
colnames(df) <- columns

# create Day_ID factor column based on Section_Day data
df$Day_ID <- ifelse(df$Section_Day == "Mon",   1, 
             ifelse(df$Section_Day == "Tues",  2, 
             ifelse(df$Section_Day == "Wed",   3, 
             ifelse(df$Section_Day == "Thurs", 4, 
             ifelse(df$Section_Day == "Fri",   5, NA)))))

# create Time_ID factor column based on Section_Time data
df$Time_ID <- ifelse(df$Section_Time == "8:40am", 1, 
              ifelse(df$Section_Time == "1:40pm", 2, 
              ifelse(df$Section_Time == "6:40pm", 3, NA)))

# gather data
df <- df %>% 
  gather(c(Flow_Rate_0, Specific_Gravity_0, NaCl_conc_0, NaCl_exr_rate_0, Osmolality_0), 
         key = "Variable1", value = "Time_0") %>% 
  gather(c(Flow_Rate_30, Specific_Gravity_30, NaCl_conc_30, NaCl_exr_rate_30, Osmolality_30), 
         key = "Variable2", value = "Time_30") %>% 
  gather(c(Flow_Rate_60, Specific_Gravity_60, NaCl_conc_60, NaCl_exr_rate_60, Osmolality_60), 
         key = "Variable3", value = "Time_60") %>% 
  gather(c(Flow_Rate_90, Specific_Gravity_90, NaCl_conc_90, NaCl_exr_rate_90, Osmolality_90), 
         key = "Variable4", value = "Time_90") %>% 
  filter(str_detect(Variable1, "Flow_Rate_") & 
         str_detect(Variable2, "Flow_Rate_") & 
         str_detect(Variable3, "Flow_Rate_") & 
         str_detect(Variable4, "Flow_Rate_") | 
         str_detect(Variable1, "Specific_Gravity_") & 
         str_detect(Variable2, "Specific_Gravity_") & 
         str_detect(Variable3, "Specific_Gravity_") & 
         str_detect(Variable4, "Specific_Gravity_") | 
         str_detect(Variable1, "NaCl_conc_") & 
         str_detect(Variable2, "NaCl_conc_") & 
         str_detect(Variable3, "NaCl_conc_") & 
         str_detect(Variable4, "NaCl_conc_") | 
         str_detect(Variable1, "NaCl_exr_rate_") & 
         str_detect(Variable2, "NaCl_exr_rate_") & 
         str_detect(Variable3, "NaCl_exr_rate_") & 
         str_detect(Variable4, "NaCl_exr_rate_") | 
         str_detect(Variable1, "Osmolality_") & 
         str_detect(Variable2, "Osmolality_") & 
         str_detect(Variable3, "Osmolality_") & 
         str_detect(Variable4, "Osmolality_")) %>% 
  select(-c(Variable2, Variable3, Variable4)) %>% 
  rename(Variable = Variable1) %>% 
  mutate(Variable = str_remove(Variable, regex("_\\d\\d?")))

# create Day_ID factor column based on Section_Day data
df$Variable_ID <- ifelse(df$Variable == "Flow_Rate",        1, 
                  ifelse(df$Variable == "Specific_Gravity", 2, 
                  ifelse(df$Variable == "NaCl_conc",        3, 
                  ifelse(df$Variable == "NaCl_exr_rate",    4, 
                  ifelse(df$Variable == "Osmolality",       5, NA)))))

# order variables and write to .csv file
df %>% select(Subject_ID, Group_Name, Group_ID, Section, Section_Day, Day_ID, Section_Time, 
              Time_ID, Variable, Variable_ID, Time_0, Time_30, Time_60, Time_90) %>% 
  write_csv("S2019_Renal_Data.csv", na = "")
