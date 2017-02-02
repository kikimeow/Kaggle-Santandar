# set your working directory
path <- ("~/Kaggle/Kaggle- Santandar")
setwd(path)

# load packages
packages <-c("dplyr")  #  load plyr ALWAYS before dplyr, AND not load plyr again.

lapply(packages, FUN = function(X) {
  do.call("library", list(X)) 
})

# memory management
memory.limit(size = 100000)

# load files
predictions <- readRDS(file = "Output/prediction.Rda")
test <- readRDS(file="Data/test.Rda")

##
# revise probability
##

predictions <- prodProbabilityTest 

# prod_15_payroll is not bought if customer does not own prod_16_pension2.  If prod_15_payroll >  prod_16_pension2, reverse order
prod_16_pension2 <- ifelse(test$lag.1.prod_15_payroll == 0 & test$lag.1.prod_16_pension2 == 0 & predictions$prod_15_payroll > predictions$prod_16_pension2, predictions$prod_15_payroll, predictions$prod_16_pension2)
prod_15_payroll <-  ifelse(test$lag.1.prod_15_payroll == 0 & test$lag.1.prod_16_pension2 == 0 & predictions$prod_15_payroll > predictions$prod_16_pension2, predictions$prod_16_pension2, predictions$prod_15_payroll)
predictions$prod_16_pension2 <- prod_16_pension2
predictions$prod_15_payroll <- prod_15_payroll


# if customer owned product in prior month, probability of prediction = 0
prodPred <- c(grep("^prod_", colnames(predictions), value=TRUE))
lag1 <- c(grep("lag.1.prod_", colnames(test), value=TRUE))
revisedPrediction <- mapply(function(prodPred, lag1) ifelse(lag1 == 1, 0, prodPred), predictions[prodPred], test[lag1])
revisedPrediction <- as.data.frame(revisedPrediction)

##
# rank and produce submission file
##

# rank top 7 products by predicted probability of addition
# rank each product.  Highest probability = 1
productRank <- t(apply(-revisedPrediction, 1, rank, ties.method = "first")) # transpose
productRank <- as.data.frame(productRank)
rownames(productRank) <- NULL

# create data frame to store top 7 products
productNum <- 1:7
top7products <- data.frame(matrix(vector(), nrow(revisedPrediction), 8))
colnames(top7products) <- c("customerCode", paste0("product_", productNum))
top7products$customerCode <- test$customerCode

for(num in productNum){
  top7products[num+1] <- apply(productRank[1:17], 1, function(x) which(x == num))
}

top7products$customerCode <- as.character(top7products$customerCode)
top7products$product_1 <- as.character(top7products$product_1)
top7products$product_2 <- as.character(top7products$product_2)
top7products$product_3 <- as.character(top7products$product_3)
top7products$product_4 <- as.character(top7products$product_4)
top7products$product_5 <- as.character(top7products$product_5)
top7products$product_6 <- as.character(top7products$product_6)
top7products$product_7 <- as.character(top7products$product_7)

saveRDS(top7products, "Output/top7products.Rda")

# function to convert product number back to names
getNamesSpanish <- function(x)   
{
  x <- gsub("^1$", "ind_cco_fin_ult1", x)
  x <- gsub("^2$", "ind_cno_fin_ult1", x)
  x <- gsub("^3$", "ind_ctju_fin_ult1", x)
  x <- gsub("^4$", "ind_ctma_fin_ult1", x)
  x <- gsub("^5$", "ind_ctop_fin_ult1", x)
  x <- gsub("^6$", "ind_ctpp_fin_ult1", x)
  x <- gsub("^7$", "ind_ecue_fin_ult1", x)
  x <- gsub("^8$", "ind_fond_fin_ult1", x)
  x <- gsub("^9$", "ind_hip_fin_ult1", x)
  x <- gsub("^10$", "ind_plan_fin_ult1", x)
  x <- gsub("^11$", "ind_pres_fin_ult1", x)
  x <- gsub("^12$", "ind_reca_fin_ult1", x)
  x <- gsub("^13$", "ind_tjcr_fin_ult1", x)
  x <- gsub("^14$", "ind_valo_fin_ult1", x)
  x <- gsub("^15$", "ind_nomina_ult1", x)
  x <- gsub("^16$", "ind_nom_pens_ult1", x)
  x <- gsub("^17$", "ind_recibo_ult1", x)
}

# create submission file using the Test set
for (i in names(top7products)[2:8])top7products[, i] <- getNamesSpanish(top7products[,i])
top7products$added_products <- paste(top7products$product_1, top7products$product_2, top7products$product_3, top7products$product_4, top7products$product_5, top7products$product_6, top7products$product_7, sep = " ")
submission <- select(top7products, customerCode, added_products)
names(submission) <- c("ncodpers", "added_products")
write.csv(submission, file = "Output/submission.csv", quote=FALSE, row.names = FALSE)
saveRDS(revisedPrediction, "Output/revisedPrediction.Rda")
saveRDS(productRank, "Output/productRank.Rda")