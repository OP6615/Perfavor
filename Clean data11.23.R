library(readxl)

#clean the not used columns of the raw data from traqq
Traqq <- select(TraqqRaw, -participantId,-invitationId,-invitationNotes,-startTime,-endTime,-openingTime,-closingTime,-responseCount,-responseId,-sendTime,-receiveTime,-manualSend,-deviceInfo,-appVersion,-timeZone,-ipHash,-consumptionCount,-consumptionId,-productId,-momentId,-momentName,-consumptionTime,-selectionTime,-collectionId,-collectionName)
Traqq

#clean the not used columns of the SVT-mean data
SVT <- `SVTraw-mean`

#Type Conversion
Traqq$nevoCode <- as.character(Traqq$nevoCode)
SVT$nevocode <- as.character(SVT$nevocode)

# Combine two database(Traqq+SVT)
Combinedata <- left_join(Traqq, SVT, by = c("nevoCode" = "nevocode"))
Combinedata$nevocode <- NULL
Combinedata

#See the missing data
Combinedata_NA <- filter(Combinedata, is.na(m_sweet))
Combinedata_NA

  #The name of the missing food
  Name_NA <- Combinedata_NA %>%
    select(nevoCode, productName, productGroupCode) %>%
    distinct(nevoCode, .keep_all = TRUE)
