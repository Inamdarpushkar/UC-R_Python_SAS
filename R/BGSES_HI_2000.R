#Packages---------------------------------------------------------------------------
install.packages("descr")
install.packages("DescTools")
library(DescTools)
library(descr)
lib_pack<-c("plyr","dplyr","tidyr","sas7bdat","stargazer","psych","formattable",
            "tidyverse","foreign","stringr","rgdal","censusapi","readr","data.table","DescTools")

lapply(lib_pack,require,character.only=TRUE)

cat("\014")
-------------------------------------------------------------------------------------
  #Set directory
  
setwd("M:\\LCS_UPDATED\\SES\\Hawaii\\2000\\BG")
-------------------------------------------------------------------------------------
  #Read and join all data togather
  
  #for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i])) # to open all cvs in R  
  
  temp=list.files(pattern="*.csv")

#read all csv files from the folder and create the ID
for (i in 1:length(temp)) {
  df<-read.csv(temp[i])
  df$county <- sprintf("%03d",df$county)
  df$tract<-sprintf("%06d",df$tract)
  df$ID <- paste0(df$state,df$county,df$tract,df$block_group)
  write.csv(df,temp[i])
}
--------------------------------------------------------------------------------------
  #Read and join all data togather
  
  #myfiles=lapply(temp,read.csv,head=T)
  #df<-join_all(myfiles, by="ID")
  --------------------------------------------------------------------------------------
  
  multmerge=function(mypath){
    filenames=list.files(path=mypath,full.names = TRUE)
    datalist=lapply(filenames,function(x){read.csv(file=x,header=T)})
    Reduce(function(x,y){merge(x,y,by="ID")},datalist)
  }
df<-multmerge("M:\\LCS_UPDATED\\SES\\Hawaii\\2000\\BG")
-------------------------------------------------------------------------------------
  #check null values in the data
  na_count<-sapply(df,function(y)sum(length(which(is.na(y)))))
na_count<-data.frame(na_count)

------------------------  ------------------------------------------------------------
  
  ## Start a new dataframe 
  
  ndf<- as.data.frame(df$state.x)
colnames(ndf)[1] <- "State"


ndf$County <- df$county.x

ndf$Tract <- df$tract.x
ndf$BG<-df$block_group.x

#ndf$ID1 <- paste0(ndf$State,ndf$County,ndf$Tract,ndf$BG)
ndf$ID<-df$ID
--------------------------------------------------------------------------------------
  ### California Tract level analysis
  
  ## Poverty
  sum_list <- c("P088002","P088003","P088004","P088005","P088006","P088007","P088008","P088009")
ndf$Poverty <- (rowSums(df[,sum_list]) / df$P088001) #No CHANGE
--------------------------------------------------------------------------------------
  ## Education 
  
  # Less than high school 
  sum_list <- c("P037003","P037004","P037005","P037006","P037007","P037008","P037009",
                "P037010","P037020", "P037021", "P037022", "P037023", "P037024",
                "P037025", "P037026" ,"P037027")
ndf$Lthighsch <- (rowSums(df[,sum_list])/df$P037001) #No CHANGE

# High school 
sum_list <- c("P037011",
              "P037028"
)
ndf$HS <- (rowSums(df[,sum_list])/df$P037001) #No CHANGE
# College 
sum_list <- c("P037015","P037016","P037017",
              "P037018","P037032","P037033",
              "P037034","P037035","P037012","P037013","P037029","P037030","P037014","P037031")
ndf$College <- (rowSums(df[,sum_list])/df$P037001) #No CHANGE

# Education Index 
ndf$Education_Index <- (ndf$College*16)+(ndf$HS*12)+(ndf$Lthighsch*9) #No CHANGE

ndf$Lthighsch <- NULL
ndf$HS <- NULL
ndf$College <- NULL 
-------------------------------------------------------------------------------------  
  # Median Household Income
  ndf$Median_HHI <- df$P053001 #No CHANGE
-------------------------------------------------------------------------------------
  # Median Gross Rent
  ndf$Median_Rent <- df$H063001 #No CHANGE
-------------------------------------------------------------------------------------
  # Median value of owner-occupied housing unit
  ndf$Median_hv <- df$H085001 #No CHANGE

---------------------------------------------------------------------------------------
  # Unemployment updated:: total male+female labor force in denominator
  ndf$Unemployment <- (df$P043007+df$P043014)/(df$P043005+df$P043012)
-------------------------------------------------------------------------------------
  # Blue Collar CNDS version
  #sum_list <- c("P050024","P050027","P050028","P050029", "P050030","P050031", "P050034","P050035", 
  #                "P050041","P050071","P050074","P050075","P050076","P050077",
  #               "P050078","P050081","P050082","P050088")
  
  #ndf$Blue_CollarCPIC <- (rowSums(df[,sum_list],na.rm = T)/df$P050001)
  
  # Blue Collar CCR version
  #sum_list <- c("P050028","P050029", "P050030","P050031","P050035", 
  #             "P050041","P050075","P050076","P050077",
  #            "P050078","P050082","P050088")

#ndf$Blue_CollarCCR <- (rowSums(df[,sum_list],na.rm = T)/df$P050001)

# Blue Collar updated version:: Farming,fishing,forestry|other forestry occupations excluded.

sum_list <- c("P050024","P050028","P050029", "P050030","P050031","P050035", 
              "P050041","P050071","P050075","P050076","P050077",
              "P050078","P050082","P050088")

ndf$Blue_Collarupdated <- (rowSums(df[,sum_list],na.rm = T)/df$P050001)


-------------------------------------------------------------------------------------
  original_ndf<-ndf
ndf<-na.omit(ndf)
na_count<-sapply(original_ndf,function(y)sum(length(which(is.na(y)))))
na_count<-data.frame(na_count)


#CPICndf<-ndf
#CPICndf$Blue_CollarCCR<-NULL
#write.csv(CPICndf,"M:\\LCS_UPDATED\\SES\\California\\2000\\BGSES_data_CPIC.csv" )

#CCRndf<-ndf
#CCRndf$Blue_CollarCPIC<-NULL

write.csv(ndf,"M:\\LCS_UPDATED\\SES\\Hawaii\\2000\\BGSES_UpdatedCCR2000.csv" )

--------------------------------------------------------------------------------------
  BGSES_UpdatedCCR2000<-read_csv("M:\\LCS_UPDATED\\SES\\Hawaii\\2000\\BGSES_UpdatedCCR2000.csv")
BGSES_UpdatedCCR2000$Q<- CutQ(BGSES_UpdatedCCR2000$Prin1, breaks = quantile(BGSES_UpdatedCCR2000$Prin1, seq(0, 1, by = 0.20), na.rm = TRUE)) 
write.csv(BGSES_UpdatedCCR2000,"M:\\LCS_UPDATED\\SES\\Hawaii\\2000\\BGSES_UpdatedCCR2000.csv" )  


# SES_data_CPIC<-read_csv("M:\\LCS_UPDATED\\SES\\California\\2000\\BGSES_data_Updated CNDS.csv")
# SES_data_CPIC$Q<- CutQ(SES_data_CPIC$Prin1, breaks = quantile(SES_data_CPIC$Prin1, seq(0, 1, by = 0.20), na.rm = TRUE)) 
# write.csv(SES_data_CPIC,"M:\\LCS_UPDATED\\SES\\California\\2000\\BGSES_data_Updated CNDS.csv" )  

--------------------------------------------------------------------------------------
  #CNDS<-read.csv("M:\\LCS_UPDATED\\SES\\California\\2000\\BGSES_data_Updated CNDS.csv")
  
  
  --------------------------------------------------------------------------------------
  #SAS test
  # Sas_test<-read_csv("M:\\LCS_UPDATED\\SES\\California\\2000\\SAS_test\\sastest.csv")
  # Sas_test$Q1<- CutQ(Sas_test$Prin1, breaks = quantile(Sas_test$Prin1, seq(0, 1, by = 0.20), na.rm = TRUE)) 
  # write.csv(Sas_test,"M:\\LCS_UPDATED\\SES\\California\\2000\\SAS_test\\sastest.csv") 
  
  
  #To find unmatched records
  #sas$ID[!(sas$ID%in%updated$ID1)]
  
  
  #crosstab
crosstab(merged.data$Q.x,merged.data$Q.y)





