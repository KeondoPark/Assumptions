library(ggplot2)

#Data processing
#Read data
dataset <- read.delim("data/Claim_RawData.tsv", sep = "\t", header = T)
Base_Mort <- read.delim("data/Base_Mort.tsv", sep = "\t", header = T)


#Group the data by Gender, t, Attained age and Age Group
data_Adj <- subset(dataset, select = c(No_Policy, No_Claim))
Sum_data <- aggregate(data_Adj, 
                      by = list(dataset$Year, dataset$Gender, dataset$t, dataset$AttAge, dataset$AgeGroup2), 
                      FUN = "sum")
colnames(Sum_data) <- c("Year","Gender", "t", "Age", "AgeGroup", "No_Policy", "No_Claim")


#Join Base Mortality rate to the dataset
Join_data <- merge(x = Sum_data, y = Base_Mort, by = "Age", all.x = TRUE)
Male  <- subset(Join_data, Gender == 1)
Male_Epd <- cbind(Male, Expected = Male$No_Policy * Male$Male)

Female <- subset(Join_data, Gender == 2)
Female_Epd <- cbind(Female, Expected = Female$No_Policy * Female$Female)

Epd_Act <- rbind(Male_Epd, Female_Epd)


# Group the data again excluding Attained age
Epd_Act_Adj <- subset(Epd_Act, select = c(No_Policy, No_Claim, Expected))
Epd_Act_Grp <- aggregate(Epd_Act_Adj, by = list(Epd_Act$Year, Epd_Act$Gender, Epd_Act$t, Epd_Act$AgeGroup), FUN = "sum")
colnames(Epd_Act_Grp) <- c("Year", "Gender", "t", "AgeGroup", "No_Policy", "No_Claim", "Expected")


# Group the data by Year, excluding t, Gender and AgeGroup to find the trend
Grp_ByYear <- aggregate(subset(Epd_Act_Grp, select = c(No_Policy, No_Claim, Expected)), 
                        by = list(Epd_Act_Grp$Year),
                        FUN = "sum")

data_Trend <- cbind(Grp_ByYear, 
                    AERatio = Grp_ByYear$No_Claim / Grp_ByYear$Expected, 
                    t = Grp_ByYear$Group.1 - min(Grp_ByYear$Group.1) + 1)
colnames(data_Trend) <- c("Year", "No_Policy", "No_Claim", "Expected", "AERatio", "t")


#Linear regression
fit1 <- lm(AERatio ~ t, data = data_Trend)

# Show the fitted equation on the plot.
cf1 <- round(coef(fit1),5)
eq1 <- paste("AERatio = ", 
             ifelse(sign(cf1[1]) == 1, " + ", " - "), abs(cf1[1]), 
             ifelse(sign(cf1[2]) == 1, " + ", " - "), abs(cf1[2]), "t")

#Log-linear regression
fit2 <- lm(AERatio ~ log(t), data = data_Trend)

# Show the fitted equation on the plot.
cf2 <- round(coef(fit2),5)
eq2 <- paste("AERatio = ", 
             ifelse(sign(cf2[1]) == 1, " + ", " - "), abs(cf2[1]), 
             ifelse(sign(cf2[2]) == 1, " + ", " - "), abs(cf2[2]), "log(t)")

xStart <- max(data_Trend$t) + 1

shinyServer(function(input, output) {
  
    output$plot <- renderPlot({
      UltimateYear <- as.numeric(input$UYear)   
      if(input$RegMethod == "Linear regression"){
        
      plot(data_Trend$t, data_Trend$AERatio, xlab = "t", ylab = "AERatio", xlim = c(1,UltimateYear), ylim = c(0,1))
    
      #Find the confidence interval and make it as coordinates
      PredictResult1 <- predict(fit1, 
                                data.frame(t = c(1:UltimateYear)), 
                                interval = "prediction")
        
      cord.x <- c(xStart - 1, c(xStart: UltimateYear), c(UltimateYear:xStart))
      cord.yLower <- c(PredictResult1[(xStart-1):UltimateYear,1], PredictResult1[UltimateYear:xStart,2])
      cord.yUpper <- c(PredictResult1[(xStart-1):UltimateYear,1], PredictResult1[UltimateYear:xStart,3])

      #Draw area for confidence interval
      polygon(cord.x, cord.yLower, col='gray', border = NA)
      polygon(cord.x, cord.yUpper, col='gray', border = NA)

      #draw the linear regression line
      par(new = T)
      curve(coef(fit1)[1] + coef(fit1)[2] * x,
            from = min(data_Trend$t), to = max(data_Trend$t),
            col = "red",
            xlab="t", ylab="AERatio", xlim = c(1,UltimateYear), ylim = c(0,1))

      par(new = T)
      curve(coef(fit1)[1] + coef(fit1)[2] * x,
            from = max(data_Trend$t), to = UltimateYear,
            col = "red", lty = 2,
            xlab="t", ylab="AERatio", xlim = c(1,UltimateYear), ylim = c(0,1))

      #Write the formula on the plot
      mtext(eq1, 3, line = -2)
      
      } else {
      plot(data_Trend$t, data_Trend$AERatio, xlab = "t", ylab = "AERatio", xlim = c(1,UltimateYear), ylim = c(0,1))
      
      #Find the confidence interval and make it as coordinates
      PredictResult2 <- predict(fit2, 
                                data.frame(t = c(1:UltimateYear)), 
                                interval = "prediction")
          
      cord.x <- c(xStart - 1, c(xStart: UltimateYear), c(UltimateYear:xStart))
      cord.yLower <- c(PredictResult2[(xStart-1):UltimateYear,1], PredictResult2[UltimateYear:xStart,2])
      cord.yUpper <- c(PredictResult2[(xStart-1):UltimateYear,1], PredictResult2[UltimateYear:xStart,3])  
      
      #Draw area for confidence interval
      polygon(cord.x, cord.yLower, col='gray', border = NA)
      polygon(cord.x, cord.yUpper, col='gray', border = NA)
        
      par(new = T)
      curve(coef(fit2)[1] + coef(fit2)[2] * log(x), 
            from = min(data_Trend$t), to = max(data_Trend$t),
            col = "red",
            xlab="t", ylab="AERatio", xlim = c(1,UltimateYear), ylim = c(0,1))
      
      par(new = T)
      curve(coef(fit2)[1] + coef(fit2)[2] * log(x), 
            from = max(data_Trend$t), to = UltimateYear,
            col = "red", lty = 2,
            xlab="t", ylab="AERatio", xlim = c(1,UltimateYear), ylim = c(0,1))
      
      #Write the formula on the plot
      mtext(eq2, 3, line = -2)
      
      }
    })
    
    
    # Output data table
      output$Table1 <- renderDataTable({
        
        UltimateYear <- as.numeric(input$UYear)   
        
        if(input$RegMethod == "Linear regression"){
          PredictResult1 <- predict(fit1, 
                                    data.frame(t = c(1:UltimateYear)), 
                                    interval = "prediction")
          
          data.frame(cbind(c(1:UltimateYear), round(PredictResult1,5)))
        } else {
          
          PredictResult2 <- predict(fit2, 
                                    data.frame(t = c(1:UltimateYear)), 
                                    interval = "prediction")
          
          data.frame(cbind(c(1:UltimateYear), round(PredictResult2,5)))
          
        }
      })
    
})

