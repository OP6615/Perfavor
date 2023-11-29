install.packages("VIM")
library(readxl)
library(dplyr)
library(VIM)

#Import the data txt named TraqqRaw, NEVO, SVT
#clean the not used columns of the raw data from traqq
Traqq <- select(TraqqRaw, -participantId,-invitationId,-invitationNotes,-startTime,-endTime,-openingTime,-closingTime,-responseCount,-responseId,-sendTime,-receiveTime,-manualSend,-deviceInfo,-appVersion,-timeZone,-ipHash,-consumptionCount,-consumptionId,-productId,-momentId,-momentName,-consumptionTime,-selectionTime,-collectionId,-collectionName)
Traqq

# Character conversion
Traqq$nevoCode <- as.character(Traqq$nevoCode)
SVT$NEVO_code <- as.character(SVT$NEVO_code)
NEVO$NEVO.code<- as.character(NEVO$NEVO.code)

# Merging the NEVO and SVT datasets
NEVOSVT <- left_join(NEVO, SVT, by = c("NEVO.code" = "NEVO_code"))

# Combine the Traqq and SVT
TraqqSVT <- left_join(Traqq, SVT, by = c("nevoCode" = "NEVO_code"))
TraqqSVT$nevocode <- NULL

# Select the nevo code of the missing food and unique them 
selected_values <- TraqqSVT$nevoCode[is.na(TraqqSVT$m_sweet)]
uniquemissing_code <- unique(selected_values)
uniquemissing_code

# Prepare the dataset for knn, which has the whole food group with determined nutrient and sensory profile and the missing food
  # Remove rows from NEVOSVT that meet the following conditions:
  # 1) NEVO.code is not in uniquemissing_code
  # 2) sweet column is NA
NEVOSVT_miss <- NEVOSVT[!(is.na(NEVOSVT$m_sweet) & !(NEVOSVT$NEVO.code %in% uniquemissing_code)), ]

# Scale the nutrient part
scaled_miss <- NEVOSVT_miss
str(scaled_miss[, 12:32])

# Select the columns to be standardised
columns_to_normalize <- c(12:17, 19:23, 27:32)

# Normalisation using the scale function
scaled_miss[, columns_to_normalize] <- scale(scaled_miss[, columns_to_normalize])
scaled_miss

# knn
# Extract the names of the columns to be interpolated 
var_cols <- colnames(scaled_miss)[156:179]

# Extract the names of the columns to be interpolated 
dist_cols <- colnames(scaled_miss)[c(12:32, 156:179)]

# Group by Food.group column(column 3) and apply kNN interpolation
imputed_data <- scaled_miss %>%
  group_by(NEVOSVT_miss[[3]]) %>%
  do(kNN(., variable = var_cols, dist_var =dist_cols , k = 5))
write.csv(imputed_data,'imputed_data.csv')

# Consider whether nutrient is meaningful for calculating distance
# Following is only using sensory profile in calculating distance
var_cols_sen <- colnames(scaled_miss)[156:179]
dist_cols_sen <- colnames(scaled_miss)[156:179]
imputed_data_sen <- scaled_miss %>%
  group_by(NEVOSVT_miss[[3]]) %>%
  do(kNN(., variable = var_cols_sen, dist_var =dist_cols_sen , k = 5))
write.csv(imputed_data2,'imputed_data_sen.csv')

# compare the dataset using whether use the nutirnt as
columns_to_compare <- 156:179
df1_selected <- imputed_data[, columns_to_compare]
df2_selected <- imputed_data_sen[, columns_to_compare]

# Calculate the difference between the two datasets on these columns
differences <- abs(df1_selected - df2_selected)
differences

# Calculate the proportion of difference
threshold <- 5  # can change
difference_ratio <- sum(differences > threshold) / length(differences)
difference_ratio
