##### API Refinitiv Datastream ################################################


############################# PRE-LOADING #######################################
# setting working directory
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )

# sourcing utility script (dependencies and functions)
source("utility.R")



# loading local refinitiv data & pre-wrangling
data_refinitiv <- read.xlsx("~/OneDrive - University of St Andrews/working_excel.xlsx", detectDates = TRUE)[,-1] 
colnames(data_refinitiv) <- data_refinitiv[1,]
data_refinitiv <- data_refinitiv[-1,] %>% 
  remove_rownames()



# separating data frame to generate one frame per stock

dat <- data_refinitiv # copy of refinitiv data
for (i in 1:length(stocks)){
  assign(stocks[i],
         dat[,1:2] %>% 
           # adding date
           mutate(date = as.Date(Timestamp)) %>% 
           # renaming Closing prices
           mutate(Close = as.numeric(TRDPRC_1)) %>% 
           #removing Timestamp and TRDPRC_1
           dplyr::select(-c(Timestamp, TRDPRC_1)) %>% 
           # adding discrete returns
           mutate(R = discrete_returns(Close)) %>% 
           # adding continuous returns
           mutate(r = log_returns(Close))
         )
  # from copied refinitiv data frame remove first two columns
  # as they are stored in the stock-specific data frame
  dat <- dat[,-(1:2)]
}

# removing dat
rm(dat)





  


