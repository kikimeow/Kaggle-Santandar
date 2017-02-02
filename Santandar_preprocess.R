# set your working directory
path <- "~/Kaggle/Kaggle- Santandar"
setwd(path)

# load packages
packages <-c("data.table", "Hmisc", "plyr", "dplyr", "zoo")  #  load plyr ALWAYS before dplyr, AND not load plyr again.

lapply(packages, FUN = function(X) {
  do.call("library", list(X)) 
})

# memory management
memory.limit(size = 100000)

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# Read data
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# files from Kaggle
fullTrain <- fread("Data/train_ver2.csv", stringsAsFactors=F, showProgress = T) # 2015-01-28 - 2016-05-28
test <- fread("Data/test_ver2.csv", stringsAsFactors=F, showProgress = T)  #2016-06-28 

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# Skip less popular products to save space
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# keep top 17 out of 24 products
fullTrain <- subset(fullTrain, select = -c(cod_prov, ind_ahor_fin_ult1,ind_aval_fin_ult1,ind_cder_fin_ult1,ind_deco_fin_ult1,ind_deme_fin_ult1,ind_dela_fin_ult1, ind_viv_fin_ult1))
test <- subset(test, select = -c(cod_prov))
gc(verbose = getOption("verbose"), reset = TRUE)

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# change column names from Spanish to English for ease of understanding
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
fullTrain <- plyr:: rename(fullTrain, c("fecha_dato" = "date", 
                "ncodpers" = "customerCode",
                "ind_empleado" = "employeeIndex", #Employee index: A active, B ex employed, F filial, N not employee, P pasive
                "pais_residencia" = "countryResidence",
                "sexo" = "sex",
                "fecha_alta" = "firstDate", #The date in which the customer became as the first holder of a contract in the bank
                "ind_nuevo" = "newCustomerIndex", # 1 if the customer registered in the last 6 months.
                "antiguedad" = "customerSeniority", # Customer seniority (in months)
                "indrel" = "firstPrimary", # 1 (First/Primary), 99 (Primary customer during the month but not at the end of the month)
                "ult_fec_cli_1t" = "lastPrimaryDate", #Last date as primary customer (if he isn't at the end of the month)
                "indrel_1mes" = "customerType", #Customer type at the beginning of the month ,1 (First/Primary customer), 2 (co-owner ),P (Potential),3 (former primary), 4(former co-owner)
                "tiprel_1mes" = "relationType", #Customer relation type at the beginning of the month, A (active), I (inactive), P (former customer),R (Potential)
                "indresi" = "residenceIndex", #Residence index (S (Yes) or N (No) if the residence country is the same than the bank country)
                "indext" = "foreignerIndex", #Foreigner index (S (Yes) or N (No) if the customer's birth country is different than the bank country)
                "conyuemp" = "employeeSpouse", #Spouse index. 1 if the customer is spouse of an employee
                "canal_entrada" = "channel", #	channel used by the customer to join
                "indfall" = "deceased",
                "tipodom" = "primaryResidence",
                "nomprov" = "provinceName",
                "ind_actividad_cliente" = "activeClient", #	Activity index (1, active customer; 0, inactive customer)
                "renta" = "grossIncome",
                "segmento" = "segmentation", #segmentation: 01 - VIP, 02 - Individuals 03 - college graduated
                "ind_cco_fin_ult1" = "prod_1_currentAcct",
                "ind_cno_fin_ult1" = "prod_2_payrollAcct",
                "ind_ctju_fin_ult1" = "prod_3_juniorAcct",
                "ind_ctma_fin_ult1" = "prod_4_masIndividualAcct",
                "ind_ctop_fin_ult1" = "prod_5_individualAcct",
                "ind_ctpp_fin_ult1" = "prod_6_individualPlusAcct",
                "ind_ecue_fin_ult1" = "prod_7_eAccount",
                "ind_fond_fin_ult1" = "prod_8_funds",
                "ind_hip_fin_ult1" = "prod_9_mortgage",
                "ind_plan_fin_ult1" = "prod_10_pensions1",
                "ind_pres_fin_ult1" = "prod_11_loans",
                "ind_reca_fin_ult1" = "prod_12_taxes",
                "ind_tjcr_fin_ult1" = "prod_13_creditCard",
                "ind_valo_fin_ult1" = "prod_14_securities",
                "ind_nomina_ult1" = "prod_15_payroll",
                "ind_nom_pens_ult1" = "prod_16_pension2",
                "ind_recibo_ult1" = "prod_17_directDebit"))

fullTrain$customerCode <- as.character(fullTrain$customerCode)
fullTrain$customerType <- as.character(fullTrain$customerType)

test <- plyr:: rename(test, c("fecha_dato" = "date", 
                         "ncodpers" = "customerCode",
                         "ind_empleado" = "employeeIndex", #Employee index: A active, B ex employed, F filial, N not employee, P pasive
                         "pais_residencia" = "countryResidence",
                         "sexo" = "sex",
                         "fecha_alta" = "firstDate", #The date in which the customer became as the first holder of a contract in the bank
                         "ind_nuevo" = "newCustomerIndex", # 1 if the customer registered in the last 6 months.
                         "antiguedad" = "customerSeniority", # Customer seniority (in months)
                         "indrel" = "firstPrimary", # 1 (First/Primary), 99 (Primary customer during the month but not at the end of the month)
                         "ult_fec_cli_1t" = "lastPrimaryDate", #Last date as primary customer (if he isn't at the end of the month)
                         "indrel_1mes" = "customerType", #Customer type at the beginning of the month ,1 (First/Primary customer), 2 (co-owner ),P (Potential),3 (former primary), 4(former co-owner)
                         "tiprel_1mes" = "relationType", #Customer relation type at the beginning of the month, A (active), I (inactive), P (former customer),R (Potential)
                         "indresi" = "residenceIndex", #Residence index (S (Yes) or N (No) if the residence country is the same than the bank country)
                         "indext" = "foreignerIndex", #Foreigner index (S (Yes) or N (No) if the customer's birth country is different than the bank country)
                         "conyuemp" = "employeeSpouse", #Spouse index. 1 if the customer is spouse of an employee
                         "canal_entrada" = "channel", #	channel used by the customer to join
                         "indfall" = "deceased",
                         "tipodom" = "primaryResidence",
                         "nomprov" = "provinceName",
                         "ind_actividad_cliente" = "activeClient", #	Activity index (1, active customer; 0, inactive customer)
                         "renta" = "grossIncome",
                         "segmento" = "segmentation")) #segmentation: 01 - VIP, 02 - Individuals 03 - college graduated

test$customerCode <- as.character(test$customerCode)
test$customerType <- as.character(test$customerType)

# combine test and fulltrain
products <- grep("^prod_", colnames(fullTrain), value=TRUE)
productsTest <- matrix(0, nrow = nrow(test), ncol = 17) # create matrix of 0
colnames(productsTest) <- products
test <- cbind(test, productsTest)
fullTrain$set <- "train"
test$set <- "test"
data <- dplyr::bind_rows(fullTrain, test)

rm(fullTrain)
rm(productsTest)
rm(test)
gc(verbose = getOption("verbose"), reset = TRUE)

# replace NA in training set for prod_22_payroll and prod_23_pension2
data[is.na(data$prod_15_payroll),"prod_15_payroll"] <- 0
data[is.na(data$prod_16_pension2),"prod_16_pension2"] <- 0

#saveRDS(data, file="Data/data_1.Rda")

# replace blank demographics data with NA
setDF(data)
character <- sapply(data[3:23], is.character)
numeric <- sapply(data[3:23], is.numeric)
demoCharacter <- colnames(data[3:23][character])
demoNumeric <- colnames(data[3:23][numeric])

data[demoCharacter] <- lapply(data[demoCharacter], function(x) ifelse(x == "", NA, as.character(x)))
data[demoNumeric] <- lapply(data[demoNumeric], function(x) ifelse(x == "", NA, as.numeric(x)))

# back-fill ActiveClient and RelationType  Time difference of 2.94779 mins
print("backfill activeClient and RelationType")
startT <- Sys.time()
data <- dplyr:: arrange(data, customerCode, desc(date))
filledData <- data %>%
  select(customerCode, activeClient, relationType)%>%
  group_by(customerCode) %>%
  mutate_each(funs(newVal = zoo::na.locf(., na.rm = FALSE)))

data$activeClient <- filledData$activeClient_newVal
data$relationType <- filledData$relationType_newVal
rm(filledData)
endT <- Sys.time()
print(endT - startT)

#saveRDS(data, file="Data/data_2.Rda")
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# Adding features - add product lags 
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# combine products into one string (prodChain) Time difference of 3.576643 mins
print("combine products into one string (prodChain)")
startT <- Sys.time()
data <- setDF(data)  # convert back to data.frame so reduce function works
productList <- grep("prod_", names(data))[1:17]
prodChain <- as.data.frame(Reduce(function(a,b)paste0(a,b),data[productList]))
colnames(prodChain)[1] <- "prodChain"
data$prodChain <- prodChain$prodChain
data$prodChain <- as.character(data$prodChain)
rm(prodChain)
endT <- Sys.time()
print(endT - startT)

#saveRDS(data, file="Data/data_3.Rda")

# add lag 1 to 5 of individual products and combined products in one string (prodChain) in new columns Time difference of 1.673343 mins
print("add lag 1 to 5 of individual products and combined products in one string (prodChain) in new columns")
startT <- Sys.time()
data <- dplyr:: arrange(data, customerCode, date)
data <- setDT(data)
for (i in 1:5){
  nm1 <- c(grep("^prod", colnames(data), value=TRUE), "activeClient", "relationType")
  nm2 <- paste("lag", i , nm1, sep= ".")
  data <- data[, (nm2) := shift(.SD, i), by = customerCode, .SDcols = nm1]
}  
endT <- Sys.time()
print(endT - startT)

#saveRDS(data, file="Data/data_4.Rda")


# Concatenate product for prior month (change from lag2 to lag1) for every product into a chain (00, 01, 10, 11) 
# Concatenate activeClient and relationType change from prior to current month 
# takes about 1.5 hrs to run
print("Concatenate product for prior month (change from lag2 to lag1) for every product into a chain (00, 01, 10, 11) ")
print("Concatenate activeClient and relationType change from prior to current month ")

startT <- Sys.time()
setDF(data)
lag1 <- c(grep("lag.1.prod_", colnames(data), value=TRUE), "activeClient", "relationType")
lag2 <- c(grep("lag.2.prod_", colnames(data), value=TRUE), "lag.1.activeClient", "lag.1.relationType")
newNames <- paste0("changeChain.", c(grep("^prod_", colnames(data), value = TRUE), "activeClient", "relationType"))
ProdChange <- mapply(function(lag2, lag1) paste0(lag2, lag1, collape = NULL), data[lag2], data[lag1])
colnames(ProdChange) <- newNames
ProdChange <- as.data.frame(ProdChange)
data <- dplyr:: bind_cols(data, ProdChange)
ProdChange$date <- data$date
ProdChange$customerCode <- data$customerCode
endT <- Sys.time()
print(endT - startT)

#saveRDS(ProdChange, file = "Data/ProdChange.Rda")
#saveRDS(data, file="Data/data_5.Rda")

gc(verbose = getOption("verbose"), reset = TRUE)

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# Adding features that require aggregation
# 1. hadIt - has customer ever had product up to data date  Time difference of 12.1132 mins
# 2. hadItMean - mean % of time of having the product up to data date
# 3. prodChangeSum - prodChange category 00, 01, 10, 11 of each product (sum) up to data date
# 4. prodChangeMean - prodChange category 00, 01, 10, 11 of each product (mean) up to data date
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

#data <- readRDS(file="Data/data_customersWithChanges.Rda")

dataDates <-c("2015-01-28", "2015-02-28", "2015-03-28", "2015-04-28", "2015-05-28", "2015-06-28", "2015-07-28", 
              "2015-08-28", "2015-09-28", "2015-10-28", "2015-11-28", "2015-12-28", "2016-01-28", "2016-02-28",
              "2016-03-28", "2016-04-28", "2016-05-28","2016-06-28")
##
# hadIt & hadItMean
##
print("Adding features that require aggregation- hadIt,hadItMean" )
startT <- Sys.time()
setDF(data)
# create dataframe to store aggregated data
summaryData <- data.frame(matrix(vector(), 0,0))

#loop from "2015-06-28" to "2016-06-28"
for(i in c(6:(length(dataDates)))){ 
  print(dataDates[i])
  subsetData <- dplyr:: select(data, date, customerCode, lag.1.prod_1_currentAcct:lag.1.prod_17_directDebit)
  subsetData[is.na(subsetData)] <- 0
  subsetData <- dplyr:: filter(subsetData, date %in% dataDates[1:i]) %>%
    dplyr:: select(customerCode, lag.1.prod_1_currentAcct:lag.1.prod_17_directDebit)%>%
    dplyr:: group_by(customerCode) %>%
    dplyr:: summarise_each(funs(hadIt= max(.,na.rm = TRUE),
                                hadItMean= mean(., na.rm = TRUE)))
  subsetData$date <- dataDates[i]
  summaryData <- dplyr::bind_rows(summaryData, subsetData)
}
endT <- Sys.time()
print(endT - startT)

colnames(summaryData) <- gsub("lag.1.", "", names(summaryData))
#saveRDS(data, file="Data/summaryDataHadIt.Rda")

# Merge summaryData with data
data <- dplyr::left_join(data, summaryData, by = c("customerCode" = "customerCode", "date" = "date"))
rm(subsetData)
#saveRDS(data, file="Data/data_6.Rda")

##
# count and mean of prodChange types 00, 01, 10, 11
##

print("count and mean of prodChange types 00, 01, 10, 11- add columns for groups 00, 01, 10, 11")
startT <- Sys.time()
# create dataframe to store aggregated data
summaryDataProdChange <- data.frame(matrix(vector(), 0,0))

# add columns for groups 00, 01, 10, 11  Time difference of 3.736583 mins
prodChangeData <- data %>%
  dplyr:: select(customerCode, date, changeChain.prod_1_currentAcct:changeChain.prod_17_directDebit)%>%
  dplyr:: mutate_each(funs(g00 = ifelse(. == "00" | . == "NA0" | . == "NANA", 1, 0),
                           g01 = ifelse(. == "01" | . == "NA1", 1, 0),
                           g10 = ifelse(. == "10", 1, 0),
                           g11 = ifelse(. == "11", 1, 0)))
prodChangeData <- dplyr:: select(prodChangeData, date, customerCode, 
                 changeChain.prod_1_currentAcct_g00:changeChain.prod_17_directDebit_g00, 
                 changeChain.prod_1_currentAcct_g01:changeChain.prod_17_directDebit_g01,
                 changeChain.prod_1_currentAcct_g10:changeChain.prod_17_directDebit_g10, 
                 changeChain.prod_1_currentAcct_g11:changeChain.prod_17_directDebit_g11)
endT <- Sys.time()
print(endT - startT)

#saveRDS(prodChangeData, file = "Data/prodChangeData.Rda")

# count and mean of prodChange types 00, 01, 10, 11- changeChainSum and Mean
print("count and mean of prodChange types 00, 01, 10, 11- changeChainSum and Mean") # Time difference of 29.08525 mins
startT <- Sys.time()

for(i in c(6:(length(dataDates)))){ 
  print(dataDates[i])
  subsetData <- dplyr:: filter(prodChangeData, date %in% dataDates[1:i]) %>%
    dplyr:: select(- date )%>%
    dplyr:: group_by(customerCode) %>%
    dplyr:: summarise_each(funs(changeChainSum = sum(.,na.rm = TRUE),
                                changeChainMean= mean(., na.rm = TRUE)))
  subsetData$date <- dataDates[i]
  summaryDataProdChange <- dplyr::bind_rows(summaryDataProdChange, subsetData)
}
endT <- Sys.time()
print(endT - startT)

colnames(summaryDataProdChange) <- gsub("changeChainSum", "Sum", names(summaryDataProdChange))
colnames(summaryDataProdChange) <- gsub("changeChainMean", "Mean", names(summaryDataProdChange))
#saveRDS(summaryDataProdChange, file="Data/summaryDataProdChange.Rda")  # save this file and merge with main dataset after reducing # of rows in data

# filter to data from June 15 to June 16
data <- dplyr:: filter(data, date %in% dataDates[6:length(dataDates)]) # reduced from 14576924 to 11432540 obs
#saveRDS(data, file="Data/data_7.Rda")

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# Add features: 
# 1. FlagAddition - flag for addition (model will predict this feature)
# 2. sumProdLag - sum of lag.1 (Number of product hold in the previous month)
# 3. date features
# 4. reduce dataset to add back the summaryDataProdChange (sum, mean of 00, 01, 10, 11)
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# add flag to signify product changes
data$changedFlag <- ifelse(data$prodChain == data$lag.1.prodChain, 0, 1)

# FlagAddition - flag for addition (model will predict this feature)
curr <- colnames(data[which(colnames(data) == "prod_1_currentAcct") :(which(colnames(data) == "prod_1_currentAcct")+16)])
lag1 <- colnames(data[which(colnames(data) == "lag.1.prod_1_currentAcct") :(which(colnames(data) == "lag.1.prod_1_currentAcct")+16)])

change <- mapply(function(curr, lag1) ifelse((curr ==  1 & lag1 == 0)|(curr == 1 & is.na(lag1)), 1, 0)
                 ,data[curr], data[lag1])

colnames(change) <- paste0("FlagAddition.", colnames(change))
change <- as.data.frame(change)
data <- dplyr:: bind_cols(data, change)

#saveRDS(data, file="Data/data_8.Rda")

# Remove some columns that are not needed
data <- data %>%
  select(-lag.5.activeClient, -lag.5.relationType,-lag.4.activeClient, -lag.4.relationType,-lag.3.activeClient, -lag.3.relationType,-lag.2.activeClient, -lag.2.relationType )

# Remove customers that never HAD product
notAllZero <- dplyr::select(data, customerCode, prodChain) %>% 
  filter(prodChain != "00000000000000000") %>%
  select(customerCode)
notAllZero <- unique(notAllZero)  #719716 customers has owned a product

train <- data %>%
  filter(set == "train") %>%
  right_join(notAllZero, by = "customerCode") # keep only customers that had products in training set(8158544 in training obs)

data <- data %>%  # data reduced from 11432450 to 9088070 (less 2,344,380)
  filter(set == "test") %>%
  bind_rows(train) %>%
  arrange(customerCode, date)

#saveRDS(data, file="Data/data_9.Rda")

# Sum Product Additions
data <- data %>%
  mutate(sumProdAdded = select(., contains("FlagAddition.")) %>%
           rowSums())

# remove customers that has never added products from the training set
custChange <- data %>%
  select(customerCode, sumProdAdded) %>%
  filter(sumProdAdded != 0) %>%
  select(customerCode)
custChange <- unique(custChange) #243199 customers had product changes
  
train <- data %>%
  filter(set == "train") %>%
  right_join(custChange, by = "customerCode") # keep only customers that had product changes in training set(2489295 in training obs)

data <- data %>%  # data reduced from 11432450 to 9088070 to 3418910 (less 2,344,380 then less 5,669,160)
  filter(set == "test") %>%
  bind_rows(train) %>%
  arrange(customerCode, date)

#saveRDS(data, file="Data/data_10.Rda")

# sumProdLag - sum of lag.1 (Number of product hold in the previous month)
data <- setDF(data)
data$sumProdLag <- rowSums(data[,c(which(colnames(data) == "lag.1.prod_1_currentAcct"): which(colnames(data) == "lag.1.prod_17_directDebit"))],  na.rm = TRUE)  # this only works for data.frame

# Merge summaryDataProdChange with data (Time difference of 1.522818 mins)
print("join summary aggregation of ChangeChainSum and ChangeChainMean to data")
startT <- Sys.time()
data <- dplyr::left_join(data, summaryDataProdChange, by = c("customerCode" = "customerCode", "date" = "date"))
endT <- Sys.time()
print(endT - startT)
rm(summaryDataProdChange)

# change firstDate to yymm
data$firstDate <- as.Date(data$firstDate, "%Y-%m-%d")
data$firstMY <- format(data$firstDate, "%y%m")
data$firstMY  <- as.factor(data$firstMY)

# change firstDate to yy
data$firstYY <- format(data$firstDate, "%y")
data$firstYY  <- as.factor(data$firstYY)

# change firstDate to mm
data$firstMM <- format(data$firstDate, "%m")
data$firstMM  <- as.factor(data$firstMM)

# add month of data to data frame
data$date <- as.Date(data$date, "%Y-%m-%d")
data$dateM <- format(data$date, "%m")
data$dateM <- as.factor(data$dateM)

#saveRDS(data, file="Data/data_11.Rda")

###
# Clean each demographics variable
###

## employee index 
data$employeeIndex <- gsub("A", "Y", data$employeeIndex)
data$employeeIndex <- gsub("B", "Y", data$employeeIndex)
data$employeeIndex <- gsub("F", "Y", data$employeeIndex)
data$employeeIndex <- gsub("S", "Y", data$employeeIndex)
data$employeeIndex <- as.factor(data$employeeIndex)
levels(data$employeeIndex) <- list(employee = "Y", notEmployee = "N")

## countryResidence
data$countryResidence <- ifelse(data$countryResidence != "ES", "Other", "ES")
data$countryResidence <- as.factor(data$countryResidence)

## sex 
# sex (recode H to Female, V to Male).
data$sex <- gsub("H", "F", data$sex)
data$sex <- gsub("V", "M", data$sex)
data$sex <- as.factor(data$sex)
levels(data$sex) <- list(male = "M", female = "F")

## age
data$age <- as.integer(data$age)
# for age > 90, change to 90
data[!is.na(data$age) & data$age > 90, "age"] <- 90
# for age <= 18, change to 18
data[!is.na(data$age) & data$age <= 18, "age"] <- 18

## firstPrimary
data$firstPrimary <- as.factor(data$firstPrimary)
levels(data$firstPrimary) <- list("primary" = 1,  "notPrimary" = 99)

## lastPrimaryDate
data$lastPrimaryDate <- NULL   #(Does not seem meaningful, already captured by flag- Delete)

## customerType
#Customer type at the beginning of the month ,1 (First/Primary customer), 2 (co-owner ),P (Potential),3 (former primary), 4(former co-owner)
data$customerType <- gsub("1.0", "1", data$customerType)
data$customerType <- gsub("2.0", "2", data$customerType)
data$customerType <- gsub("3.0", "3", data$customerType)
data$customerType <- gsub("4.0", "4", data$customerType)
data$customerType <- as.factor(data$customerType)
levels(data$customerType) <- list("primary" = 1,  "coOwner" = 2, "formerPrimary" = 3, "formerCoOwner" = 4, "potential" = "P")

## relationType 
# Customer relation type at the beginning of the month, A (active), I (inactive), P (former customer),R (Potential)
data[which(data$relationType == "N"), "relationType"] <- "R"  # change potential customer to potential relationtype like the others
data$relationType <- as.factor(data$relationType)
levels(data$relationType) <- list(active = "A", inactive = "I", formerCustomer = "P", potential = "R")

## residenceIndex
data$residenceIndex <- as.factor(data$residenceIndex)
levels(data$residenceIndex) <- list(resident = "S", notResident = "N")

## foreignerIndex
data$foreignerIndex <- as.factor(data$foreignerIndex)
levels(data$foreignerIndex) <- list(foreigner = "S", notForeigner = "N")

## employeeSpouse
# N (526, 99%), S (5, 1%) 
##
data$employeeSpouse <- NULL

## channel
data$channel <- as.factor(data$channel)

## deceased
data$deceased <- as.factor(data$deceased)
levels(data$deceased) <- list(deceased = "S", notDeceased = "N")

## primaryResidence
data$primaryResidence <- NULL

## provinceCode
data$provinceCode <- NULL # duplicates provinceName

## provinceName
data$provinceName <- as.factor(data$provinceName)

## activeClient
data$activeClient <- as.factor(data$activeClient)
levels(data$activeClient) <- list(activeClient = 1, notActiveClient = 0)

## grossIncome
data$grossIncome <- as.numeric(data$grossIncome)

## segmentation
data$segmentation <- as.factor(data$segmentation)

#saveRDS(data, file="Data/data_12.Rda")

## back fill rest of demographics data (ones with many missing)
data <- arrange(data, customerCode, desc(date)) # sort for back fill and for appending later
filledData <- data %>%
  select(customerCode, date, customerType, channel, provinceName, grossIncome, segmentation)%>%
  group_by(customerCode) %>%
  mutate_each(funs(newVal = zoo::na.locf(., na.rm = FALSE)))
filledData <- as.data.frame(filledData)
#saveRDS(filledData, file = "Data/filledDataDemo.Rda")

data$customerType <- filledData$customerType_newVal
data$channel <- filledData$channel_newVal
data$provinceName <- filledData$provinceName_newVal
data$grossIncome <- filledData$grossIncome_newVal
data$segmentation <- filledData$segmentation_newVal

## newCustomerIndex
# customers registered in the last 6 months (180 days).  Recalculate in case data is wrong.
data$accountLengthDays <- data$date - data$firstDate
data$accountLengthDays <- as.integer(data$accountLengthDays)
data[!is.na(data$accountLengthDays) & data$accountLengthDays <= 180, "newCustomerIndex"] <- 1
data[!is.na(data$accountLengthDays) & data$accountLengthDays > 180, "newCustomerIndex"] <- 0
data$newCustomerIndex <- as.factor(data$newCustomerIndex)

## customerSeniority
# data has a lot of wrong calculation.  Re-calculate
data$customerSeniority <- as.integer(data$customerSeniority)
data$customerSeniority <- round(data$accountLengthDays/365*12,0)

#saveRDS(data, file="Data/data_13.Rda")

##
# check character variables and convert to factor level if appropriate
characterCols <- names(data[, sapply(data, is.character)]) 
characterCols <- characterCols[4:9] # convert lag1 prodChain and relationType
data[characterCols] <- lapply(data[characterCols], factor)

saveRDS(data, file="Data/data.Rda")


# Split data into training and testing set.  
# 1. Filter out features that should not be in model
# 2. Keep only rows with changes for training set
test <- dplyr:: filter(data, set == "test") %>%
  select(-firstDate, -(prod_1_currentAcct:prod_17_directDebit), -prodChain, -set, -changedFlag, -sumProdAdded, -firstMY, -accountLengthDays)

train <- dplyr:: filter(data, set == "train", sumProdAdded > 0) %>%
  select(-firstDate, -(prod_1_currentAcct:prod_17_directDebit), -prodChain, -set, -changedFlag, -sumProdAdded, -firstMY, -accountLengthDays)

saveRDS(test, file="Data/test.Rda")
saveRDS(train, file = "Data/train.Rda")


# Melt data for multinomial models
# trainMelt <- train %>%
#   tidyr::gather(prodAdded, FlagAddition, FlagAddition.prod_1_currentAcct:FlagAddition.prod_17_directDebit) %>%
#   filter(FlagAddition == 1)%>%
#   select(-FlagAddition)
# 
# dropCols <- setdiff(names(test), names(trainMelt)) 
# testMelt <- test %>% select(-one_of(dropCols))
# 
# saveRDS(testMelt, file="Data/testMelt.Rda")
# saveRDS(trainMelt, file = "Data/trainMelt.Rda")





