#Clear the environemnt

rm(list=ls())

#Set the working directorate

setwd("D:/SALES")



#Clear the working environments' folders

unlink("D:/SALES/*")



library(odbc)#package necessary for the database connection

#Database log ins

con <- dbConnect(odbc(),

                 Driver = "SQL Server",

                 Server = "",

                 Database = "",

                 UID = "HNjagi",

                 PWD = "",

                 Port = 1433)



# Sales Performance (Raw Dataset)

results411 <- dbSendQuery(con,"select o.TripID,o.SaleID,VRegNO,trim(VehicleModel)VehicleModel,

v.LoadCapacity, v.UsageType,concat(FirstName,' ',MiddleName,' ',LastName)DriverName, s.PayrollNumber,s.PhoneNumber,

c.CustomerID,CustomerName,c.ContactPerson, c.ContactNumber,r.RouteName,r.Region,c.Latitude as CustLat,c.Longitude as CustLon,

    (Sold6KG-coalesce(Exchange6KG,0))Outright6KG,

    (Sold13KG-coalesce(Exchange13KG,0))Outright13KG,

    (Sold50KG-coalesce(Exchange50KG,0))Outright50KG,

    coalesce(Exchange6KG,0)Refill6KG,

    coalesce(Exchange13KG,0)Refill13KG,

    coalesce(Exchange50KG,0)Refill50KG,

    coalesce(SoldBurner,0)SoldBurner,

    coalesce(SoldGrill,0)SoldGrill,

    coalesce(SoldHosePipe,0)SoldHosePipe,

    coalesce(SoldRegulator,0)SoldRegulator,TransAmount,

       Z.TerminalName, o.TransTime,z.Latitude as ConLat,z.Longitude as ConLon,z.Terminalid, z.TerminalID

    from SaleOrder o inner join

    (

    select SaleID,

    sum( CASE WHEN p.size='6KG' THEN Quantity else 0 end)Sold6KG,

    sum( CASE WHEN p.size='13KG' THEN Quantity else 0 end)Sold13KG,

    sum( CASE WHEN p.size='50KG' THEN Quantity else 0 end)Sold50KG,

    sum( CASE WHEN p.size='BRN' THEN Quantity else 0 end)SoldBurner,

    sum( CASE WHEN p.size='GRL' THEN Quantity else 0 end)SoldGrill,

    sum( CASE WHEN p.size='LoP' THEN Quantity else 0 end)SoldHosePipe,

    sum( CASE WHEN p.size='RGT' THEN Quantity else 0 end)SoldRegulator,

    sum(SKUTransAmount)SKUTransAmount

    FROM SaleDetail s inner join Product p on p.SAPMaterialCode=s.MaterialCode

    group by SaleID

    )y on  o.SaleID=y.SaleID

    left join(

    select SaleID,

    sum( CASE WHEN ex.size='6KG' THEN Quantity else 0 end)Exchange6KG,

    sum( CASE WHEN ex.size='13KG' THEN Quantity else 0 end)Exchange13KG,

    sum( CASE WHEN ex.size='50KG' THEN Quantity else 0 end)Exchange50KG

    FROM SaleRefillExchange Se

    INNER JOIN ExchangePool ex on ex.MaterialCode=Se.MaterialCode

    group by SaleID

    )k on k.SaleID=o.SaleID

    OUTER APPLY(Select * from(SELECT TripID,FromTerminalID,TerminalName,Latitude,Longitude,TerminalID FROM StockMovement s inner join Terminal t

       on S.FromTerminalID=t.TerminalID where movementtype=1)z where z.TripID=o.TripID)Z

    INNER JOIN TripPlanning p on p.TripID=o.TripID

    inner join hrmbase.dbo.tblstaff s on s.UserID=p.DriverID

    INNER JOIN Vehicle v on v.ID=p.VehicleID

    INNER JOIN Customer c on c.CustomerID=o.CustomerID

    INNER JOIN Routes r on r.id=c.RouteID

    where cast(o.TransTime as date) >= '2021-01-01' and cast(o.TransTime as date) <= '2021-06-30'

    order by cast(o.TransTime as date)")

WorkingData <- dbFetch(results411)



library(chron)

library(lubridate)

Hours <- format(as.POSIXct(strptime(WorkingData$TransTime,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M:%S")

Hours <- chron(times = Hours)

Dates <- format(as.POSIXct(strptime(WorkingData$TransTime,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%Y-%m-%d")

MonthNumber <- month(as.POSIXlt(WorkingData$TransTime, format="%d/%m/%Y")) #MonthNumber (No s in month)

Month <- months(as.POSIXlt(WorkingData$TransTime, format="%d/%m/%Y")) #Month String

Day <- weekdays(as.POSIXct(WorkingData$TransTime), abbreviate = F)

WeekNumber <- strftime((WorkingData$TransTime), format = "")







#write.csv(CustCon,"Customer.csv")



Hours1 <- chron(times = Hours)



length(Hours)





WorkingData$Dates <- Dates

WorkingData$Hours <- Hours

WorkingData$MonthNumber <- MonthNumber

WorkingData$Month <- Month

WorkingData$Day <- Day

WorkingData$WeekNumber <- WeekNumber

names(WorkingData)

#View(WorkingData)







attach(WorkingData)

library(dplyr)

library(writexl)



#Container Sales

ContainerSales <- WorkingData%>%

  filter(!VRegNO == 'Proto Home Delivery' & !VehicleModel == "Virtual" & !VehicleModel == "virtual"

         & !VehicleModel == "RetailShop" & !VehicleModel == "RetailSHop"& !VehicleModel == 'STAFF')%>%

  group_by(TerminalName,Month)%>%

  summarise(DaysActive = n_distinct(Dates),

            Visits = n_distinct(SaleID),

            Served = n_distinct(CustomerID),

            Refills = sum(Refill6KG,Refill13KG,Refill50KG),

            Outright = sum(Outright6KG,Outright13KG,Outright50KG),

            TotalSales = sum(Refills,Outright),

            DailyDropSize_Container = round(TotalSales/DaysActive,0),

            DailyDropSize_Cust = round(TotalSales/Visits,0))

View(ContainerSales)

write.csv(ContainerSales,"ContainerSales.csv")





CustCon <- WorkingData%>%

  group_by(CustomerName,VRegNO,TerminalName,CustLat,CustLon, ConLat,ConLon,Dates)%>%

  summarise(Refills = sum(Refill6KG,Refill13KG,Refill50KG),

            Outright = sum(Outright6KG,Outright13KG,Outright50KG),

            TotalSales = sum(Refills,Outright))

CustCon <- CustCon%>%

  filter(!TotalSales==0)

##Frequency



View(CustCon)

write.csv(CustCon,"Customer2.csv")



library("xlsx")



#write.xlsx(WorkingData,file = "myworkbook.xlsx", sheetName = "WorkingData",

#col.names = TRUE, row.names = TRUE, append = FALSE)



##########

TerminalSales <- WorkingData%>%

  filter(!VRegNO == 'Proto Home Delivery' & !VehicleModel == "Virtual" & !VehicleModel == "virtual"

         & !VehicleModel == "RetailShop" & !VehicleModel == "RetailSHop"& !VehicleModel == 'STAFF')%>%

  group_by(TerminalName,Dates)%>%

  summarise(TotalSales = sum(Refill6KG,Refill13KG,Refill50KG,Outright6KG,Outright13KG,Outright50KG),

            TimeStart = min(Hours),

            TimeEnd = max(Hours),

            TimeInMarket = max(Hours)-min(Hours))



#View(TerminalSales)



#write.csv(WorkingData,"SalesRAWDATA.csv")







YesterdayAssetReport <- WorkingData%>%

  filter(!VRegNO == 'Proto Home Delivery' & !VehicleModel == "Virtual" & !VehicleModel == "virtual"

         & !VehicleModel == "RetailShop" & !VehicleModel == "RetailSHop"& !VehicleModel == 'STAFF',Dates == Sys.Date()-1)%>%

  group_by(VRegNO,VehicleModel,LoadCapacity,Region,TerminalName,Month,Dates,Day)%>%

  summarise(RouteServed = n_distinct(RouteName),

            No.Of.Trips = n_distinct(TripID),

            Visits = n_distinct(SaleID),

            SoldRefill6KG = sum(Refill6KG),

            SoldRefill13KG = sum(Refill13KG),

            SoldRefill50KG = sum(Refill50KG),

            SoldOutright6KG = sum(Outright6KG),

            SoldOutright13KG = sum(Outright13KG),

            SoldOutright50KG = sum(Outright50KG),

            TotalSales = sum(Refill6KG,Refill13KG,Refill50KG,Outright6KG,Outright13KG,Outright50KG),

            TimeStart = min(Hours),

            TimeEnd = max(Hours),

            TimeInMarket = max(Hours)-min(Hours))

#library(janitor)

#AllSales <- YesterdayAssetReport%>%

#adorn_totals("row")

#View(AllSales)



#write.csv(YesterdayAssetReport,"YesterdayAssetReport.csv")

library("xlsx")

#write.xlsx(YesterdayAssetReport,file = "YesterdayAssetReport.xlsx", sheetName = "YesterdayAssetReportt")



############################################3ALL

MTDAssetReport<- WorkingData%>%

  filter(!VRegNO == 'Proto Home Delivery' & !VehicleModel == "Virtual" & !VehicleModel == "virtual"

         & !VehicleModel == 'STAFF' & !Region == 'Retail Shops')%>%

  group_by(Dates,VRegNO)%>%

  summarise(RouteServed = n_distinct(RouteName),

            No.Of.Trips = n_distinct(TripID),

            Visits = n_distinct(SaleID),

            SoldRefill6KG = sum(Refill6KG),

            SoldRefill13KG = sum(Refill13KG),

            SoldRefill50KG = sum(Refill50KG),

            SoldOutright6KG = sum(Outright6KG),

            SoldOutright13KG = sum(Outright13KG),

            SoldOutright50KG = sum(Outright50KG),

            TotalSales = sum(Refill6KG,Refill13KG,Refill50KG,Outright6KG,Outright13KG,Outright50KG),

            SoldOutright = sum(Outright6KG,Outright13KG,Outright50KG),

            TimeStart = min(Hours),

            TimeEnd = max(Hours),

            TimeInMarket = max(Hours)-min(Hours))



###########################################################3





######################3333

YesterdayAssetReport1 <- WorkingData%>%

  filter(!VRegNO == 'Proto Home Delivery' & !VehicleModel == "Virtual" & !VehicleModel == "virtual"

         & !VehicleModel == 'STAFF' & Dates == Sys.Date()-1 & !Region == 'Retail Shops')%>%

  group_by(VRegNO,VehicleModel,LoadCapacity,Dates,Day)%>%

  summarise(RouteServed = n_distinct(RouteName),

            No.Of.Trips = n_distinct(TripID),

            Visits = n_distinct(SaleID),

            SoldRefill6KG = sum(Refill6KG),

            SoldRefill13KG = sum(Refill13KG),

            SoldRefill50KG = sum(Refill50KG),

            SoldOutright6KG = sum(Outright6KG),

            SoldOutright13KG = sum(Outright13KG),

            SoldOutright50KG = sum(Outright50KG),

            TotalSales = sum(Refill6KG,Refill13KG,Refill50KG,Outright6KG,Outright13KG,Outright50KG),

            TimeStart = min(Hours),

            TimeEnd = max(Hours),

            TimeInMarket = max(Hours)-min(Hours))

#write.csv(YesterdayAssetReport1,"YesterdayAssetReport2.csv")







#library(janitor)

#AllSales <- YesterdayAssetReport1%>%

#adorn_totals("row")

#View(AllSales)

#View(YesterdayAssetReport1)

#########################

YesterdayAssetReport1%>%

  summarise(AssetNo = n_distinct(YesterdayAssetReport1$VRegNO))





#JanaData <- YesterdayAssetReport%>%

# select(VRegNO,VehicleModel,LoadCapacity,Region,TerminalName,Month,Dates,Day,

#       RouteServed,No.Of.Trips,Visits,SoldRefill6KG,SoldRefill13KG,SoldRefill50KG,

#      SoldOutright6KG,SoldOutright13KG,SoldOutright50KG,TotalSales,TimeStart,TimeEnd,TimeInMarket)

#View(JanaData)







AssetTimeMarketMore6Hrs <- YesterdayAssetReport1%>%

  filter(TimeInMarket >= '06:00:00')

#View(AssetTimeMarketMore6Hrs)



#write.csv(AssetTimeMarketMore6Hrs,"AssetTimeMarketMore6Hrs.csv")







df <- AssetTimeMarketMore6Hrs%>%

  group_by(Dates)%>%

  mutate(cums = cumsum(TotalSales))

#View(df)









AssetReport <- WorkingData%>%

  filter(!VRegNO == 'Proto Home Delivery' & !VehicleModel == "Virtual" & !VehicleModel == "virtual"

         & !VehicleModel == "RetailShop" & !VehicleModel == "RetailSHop"& !VehicleModel == 'STAFF')%>%

  group_by(VRegNO,VehicleModel,LoadCapacity,Month,Dates,Day)%>%

  summarise(RouteServed = n_distinct(RouteName),

            No.Of.Trips = n_distinct(TripID),

            Visits = n_distinct(SaleID),

            SoldRefill6KG = sum(Refill6KG),

            SoldRefill13KG = sum(Refill13KG),

            SoldRefill50KG = sum(Refill50KG),

            SoldOutright6KG = sum(Outright6KG),

            SoldOutright13KG = sum(Outright13KG),

            SoldOutright50KG = sum(Outright50KG),

            TotalSales = sum(Refill6KG,Refill13KG,Refill50KG,Outright6KG,Outright13KG,Outright50KG),

            TimeStart = min(Hours),

            TimeEnd = max(Hours),

            TimeInMarket = max(Hours)-min(Hours))

#library(janitor)

#AllSales <- AssetReport%>%

# adorn_totals("row")

#View(AssetReport)

#write.csv(AssetReport,"DailyAssetPerformance.csv")



Salesalll <- WorkingData%>%

  group_by(Dates)%>%

  summarise(Visits = n_distinct(SaleID),

            Assets = n_distinct(VRegNO),

            SoldRefill6KG = sum(Refill6KG),

            SoldRefill13KG = sum(Refill13KG),

            SoldRefill50KG = sum(Refill50KG),

            SoldOutright6KG = sum(Outright6KG),

            SoldOutright13KG = sum(Outright13KG),

            SoldOutright50KG = sum(Outright50KG),

            TotalSales = sum(Refill6KG,Refill13KG,Refill50KG,Outright6KG,Outright13KG,Outright50KG),

            AvgSalesPerAsset = round(TotalSales/Assets,0),

            AvgVisitsPerAsset = round(Visits/Assets,0),

            DropSize = round(TotalSales/Visits,0),

            TimeStart = min(Hours),

            TimeEnd = max(Hours),

            TimeInMarket = max(Hours)-min(Hours))

View(Salesalll)

#write.csv(Salesalll,"Salesalll.csv")



##Considering All sold cylinders in the market

AllAssetSalesMarket <- WorkingData%>%

  group_by(VRegNO,VehicleModel,LoadCapacity,Month,Dates,Day)%>%

  summarise(RouteServed = n_distinct(RouteName),

            No.Of.Trips = n_distinct(TripID),

            Visits = n_distinct(SaleID),

            SoldRefill6KG = sum(Refill6KG),

            SoldRefill13KG = sum(Refill13KG),

            SoldRefill50KG = sum(Refill50KG),

            SoldOutright6KG = sum(Outright6KG),

            SoldOutright13KG = sum(Outright13KG),

            SoldOutright50KG = sum(Outright50KG),

            TotalSales = sum(Refill6KG,Refill13KG,Refill50KG,Outright6KG,Outright13KG,Outright50KG),

            TimeStart = min(Hours),

            TimeEnd = max(Hours),

            TimeInMarket = max(Hours)-min(Hours))

#View(AllAssetSalesMarket)

#AllAssetsMore than 6 hours in Market



AllDates <- AllAssetSalesMarket%>%

  filter(!VehicleModel == "Virtual" & !VehicleModel == "virtual"

         & !VehicleModel == "Retailshop" & !VehicleModel == "RetailSHop"& !VehicleModel == 'STAFF')%>%

  group_by(Dates)%>%

  summarise(TotalAssets = n_distinct(VRegNO),

            MoreThan6Hrs = sum(TimeInMarket >= '06:00:00'))



View(AllDates)





#AssetMore6Hrs <- AllAssetSalesMarket%>%

# filter(TimeInMarket >= '06:00:00' & !VRegNO == 'Proto Home Delivery' & !VehicleModel == "Virtual" & !VehicleModel == "virtual"

#       & !VehicleModel == "Retailshop" & !VehicleModel == "RetailSHop"& !VehicleModel == 'STAFF')%>%

#group_by(Dates)%>%

#summarise(TotalAssets = n_distinct(VRegNO))



#View(AssetMore6Hrs)



#Total Daily Selling Assets

DailyAssetFiltered <- WorkingData%>%

  filter(!VehicleModel == "Virtual" & !VehicleModel == "virtual"

         & !VehicleModel == "Retailshop" & !VehicleModel == "RetailSHop"& !VehicleModel == 'STAFF')%>%

  group_by(Dates)%>%

  summarise(TotalAssets = n_distinct(VRegNO))



View(DailyAssetFiltered)



DailyAssetALL <- WorkingData%>%

  group_by(Dates)%>%

  summarise(TotalSales = sum(Refill6KG,Refill13KG,Refill50KG,Outright6KG,Outright13KG,Outright50KG),

            SoldOutright = sum(Outright6KG,Outright13KG,Outright50KG),

            CountSaleID = sum(n_distinct(SaleID)))

#View(DailyAssetALL)



TotalAssets <- DailyAssetFiltered$TotalAssets

CountSaleIDALL <- DailyAssetALL$CountSaleID

TotalSalesAll <- DailyAssetALL$TotalSales

SoldOutrightALL <- DailyAssetALL$SoldOutright

MoreThan6Hrs <- AllDates$MoreThan6Hrs



DailyReport2 <- DailyAssetALL



Rep <- rep(1,length(DailyReport2$Dates))

DailyReport2$Rep <- Rep

DailyReport2$TotalAssets <- TotalAssets

DailyReport2$TotalSaleAll <- TotalSalesAll

DailyReport2$SoldOutrightALL <- SoldOutrightALL

DailyReport2$CountSaleIDALL <- CountSaleIDALL

DailyReport2$MoreThan6Hrs <- MoreThan6Hrs

names(DailyReport2)



DailyReport3 <- DailyReport2%>%

  group_by(Rep)%>%

  mutate(RunRate = cumsum(TotalSales),

         RunRate2 = cumsum(TotalSaleAll))

names(DailyReport3)



DailyReport4 <- DailyReport3[, c(1,4,2,12,3,6,10)]

View(DailyReport4)

names(DailyReport4)



DailyReport5 <- DailyReport4%>%

  group_by(Dates)%>%

  summarise(CountSaleID = CountSaleID,

            NormalisedForecast = sum(),

            TotalSales = TotalSales,

            RunRate2 = RunRate2,

            SoldOutright = SoldOutright,

            TotalAssets = TotalAssets,

            MoreThan6Hrs = MoreThan6Hrs,

            AvgSalesPerAsset = round(TotalSales/TotalAssets,0),

            AvgVisitsPerAsset = round(CountSaleID/TotalAssets,0),

            DropSize = round(TotalSales/CountSaleID,0))

#View(DailyReport5)



#Container Sales

ContainerSales <- WorkingData%>%

  filter(!VRegNO == 'Proto Home Delivery' & !VehicleModel == "Virtual" & !VehicleModel == "virtual"

         & !VehicleModel == "RetailShop" & !VehicleModel == "RetailSHop"& !VehicleModel == 'STAFF' & Dates == Sys.Date()-1)%>%

  group_by(TerminalName,Dates)%>%

  summarise(RouteServed = n_distinct(RouteName),

            No.Of.Trips = n_distinct(TripID),

            Visits = n_distinct(SaleID),

            SoldRefill6KG = sum(Refill6KG),

            SoldRefill13KG = sum(Refill13KG),

            SoldRefill50KG = sum(Refill50KG),

            SoldOutright6KG = sum(Outright6KG),

            SoldOutright13KG = sum(Outright13KG),

            SoldOutright50KG = sum(Outright50KG),

            TotalSales = sum(Refill6KG,Refill13KG,Refill50KG,Outright6KG,Outright13KG,Outright50KG),

            TimeStart = min(Hours),

            TimeEnd = max(Hours),

            TimeInMarket = max(Hours)-min(Hours))



##Plots



write.csv(ContainerSales,"ContainerSales.csv")



library(writexl)

write.xlsx(as.data.frame(DailyReport5), file='SalesPerformance.xlsx', sheetName="AllSales", col.names=TRUE, row.names=FALSE, append=TRUE)

write.xlsx(as.data.frame(MTDAssetReport), file='SalesPerformance.xlsx', sheetName="MTDCylinderSales", col.names=TRUE, row.names=FALSE, append=TRUE)

write.xlsx(as.data.frame(YesterdayAssetReport1), file='SalesPerformance.xlsx', sheetName="YesterdayAssetReport", col.names=TRUE, row.names=FALSE, append=TRUE)

write.xlsx(as.data.frame(ContainerSales), file='SalesPerformance.xlsx', sheetName="YesterdayContainerSales", col.names=TRUE, row.names=FALSE, append=TRUE)



Data1 <- data.frame(DailyReport5)

# Create HTML object

library(RDCOMClient)

library(xtable)



x <- Data1

y <- print(xtable(x), type="html", print.results=FALSE)

body1 <- paste0("<html>", y, "</html>")



OutApp <- COMCreate("Outlook.Application")

outMail = OutApp$CreateItem(0)

#outMail[["To"]] = anjenga@p.com

outMail[["To"]] = HNjagi@Protoenergy.com


#outMail[["Cc"]]= paste(CHormes@P.com,SHabakkuk@P.com,sep=";", collapse=NULL)

#outMail[["bcc"]] = paste(SHabakkuk@P.com,CHormes@P.com,HNjagi@P.com,sep=";", collapse=NULL)

outMail[["subject"]] = paste("Daily Sales Report")

outMail[["HTMLbody"]] = sprintf('

<h1>Sales Performance</h1>

<p>Hello Henry,</p>



<p>Please receive the attached.</p>



%s



<p> Kind Regards </p>

<p> Henry Njagi </p>



', body1)

outMail[["Attachments"]]$Add("D:\\SALES\\SalesPerformance.xlsx")





outMail$Send()
