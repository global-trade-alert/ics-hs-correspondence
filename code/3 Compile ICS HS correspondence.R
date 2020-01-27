
### Discussion with JF:
## 1. Compile ICS-HS correspondence based on (a) list of 500 ICS codes in the Excel sheet from the VDMA project, (b) the conversion app, (c) unique correspondences in the WTO notification, potentially (d) Julia's work
## 2. Check if all important HS codes (especially exported by Commonwealth countries) are in the correspondence table.
## 3. Based on Simon's response, develop requested charts and statistics for the 25-pager for the CS.

rm(list=ls())
# devtools::install_github("global-trade-alert/gtasql@origin")
# install.packages("pool")
# install.packages("RMySQL")
library(gtalibrary)
library(pool)
library(RMySQL)
library(gtasql)
library(splitstackshape)
library(openxlsx)

gta_setwd()
path = "0 projects/36 ICS-HS correspondence/"
probability.threshold=.9 #for the app results [JF: I think .9 is on the very high side. Should we ask someone to check sensibility of .9-.7 choices?]

ics.codes <- gtalibrary::ics.names
raw.ics = ics.codes # This 'raw ICS table' is just for checking the ICS code descriptions.
ics.codes <- subset(ics.codes, most.granular==T) #We only need the most granular ICS codes


## 1a: The Excel sheet with the 500 ICS Codes from the VDMA project
vdma = gtalibrary::ics.to.hs
length(unique(vdma$ics)) ## we have 178 ICS codes here


## 1b: App results
## Possible APP exit status:
# 1 (UNPROCESSED) NOT ENOUGH ENTRIES TO PROCESS PHRASE
# 2 (PROCESSED) IF CODE SELECTED AND CODE SUGGESTED ARE AVAILABLE
# 3 (NOT A PRODUCT) IF MAJORITY OF CHECKS LABEL AS "NOT A PRODUCT"
# 4 (NO CODES) IF CHECKED ENOUGH TIMES BUT NO CODES HAVE BEEN SELECTED FOR THIS PHRASE
# 5 (ROUND LIMIT) IF NROUND >=4


database <- "ricardomain"

gta_sql_kill_connections()
gta_sql_pool_open(db.title=database,
                  db.host = gta_pwd(database)$host,
                  db.name = gta_pwd(database)$name,
                  db.user = gta_pwd(database)$user,
                  db.password = gta_pwd(database)$password,
                  table.prefix = "hs_")
source("17 Shiny/5 HS code finder/code/functions/gta_hs_check_job_results.R")

app.results=gta_hs_check_job_results(c(61,62))$all ## exports entire app incl. unfinished phrases plus unmatched HS codes (see probability column for likelihood HS belongs to phrase)
app.backup = app.results
# Note: have to re-merge ics. names to this. All these in the job are ics.names$most.granular==T, could be that there is white space at the end (clean using gsub("\\s+$","",variable))


## 1b Evaluate App results
table(unique(subset(app.results, select = c("exit.status", "phrase.id")))$exit.status)


## 12% are not products apparently
not.products = unique(subset(app.results, exit.status==3, select = c("phrase.id", "phrase"))$phrase) ## PL: Most correct IMO, though IT software could be 852830
round.limit = unique(subset(app.results, exit.status==5, select = c("phrase.id", "phrase"))$phrase)


## Exclude search results with a probability below X%
length(unique(subset(app.results, exit.status==2 & probability >=probability.threshold)$phrase.id))/length(unique(subset(app.results, exit.status==2)$phrase.id))
# app.results = subset(app.results, probability >= probability.threshold | exit.status %in% c(1,3))
app.results = subset(app.results, probability >= probability.threshold | exit.status %in% c(3)) ## We keep only results above the threshold or those identified as not products.


## Add the ICS codes
app.results$phrase =gsub("\\s+$","",app.results$phrase)
app.results$phrase[app.results$phrase=="Integrated circuits. Microelectronics Graphical symbols in general"] = "Integrated circuits. Microelectronics"

app.results = merge(app.results, unique(subset(ics.codes, select = c("ics.code", "ics.description"))), by.x = "phrase", by.y = "ics.description", all.x = T)
nrow(subset(app.results, is.na(ics.code)))

## There is still a problem that 8 descriptions were identical even though they belonged in multiple different categories. For now, we will take them out and I will assign HS codes again manually in step
multiples = subset(aggregate(ics.code ~ phrase, app.results, function(x) length(unique(x))), ics.code > 1)$phrase
app.results = subset(app.results, !(phrase %in% multiples))



## 1c Unique correspondences from the WTO notifications
load(file="data/WTO SPS & TBT/WTO SPS & TBT database.Rdata")
notifications = subset(notifications, date.announced >= "2008-11-01") # Narrow it down to relevant WTO notifications
products.hs = subset(products.hs, wto.id %in% notifications$wto.id)
products.hs$hs.code = as.character(products.hs$hs.code)
products.ics = subset(products.ics, wto.id %in% notifications$wto.id)
products.ics$ics.code = as.character(products.ics$ics.code)
length(unique(notifications$wto.id))


single.hs=subset(aggregate(hs.code ~ wto.id, products.hs, function(x) length(unique(x))), hs.code==1)$wto.id
single.ics=subset(aggregate(ics.code ~ wto.id, products.ics, function(x) length(unique(x))), ics.code==1)$wto.id

correspondence = merge(subset(products.hs, wto.id %in% c(single.hs, single.ics)), subset(products.ics, wto.id %in% c(single.hs, single.ics)), by = "wto.id")
correspondence = unique(subset(correspondence, select = c("hs.code", "ics.code"))); rm(single.hs, single.ics)

codes.to.check = unique(products.ics$ics.code) ## There are 1151 ICS codes in relevant WTO notifications


##############################################################################################
## 2: Check if all ICS codes assigned

complete = data.frame(ics.code=as.integer(),hs.code=as.integer(),source=as.character())

complete = rbind(complete, data.frame(ics.code=vdma$ics, hs.code=vdma$hs6, source="VDMA"))
complete = rbind(complete, data.frame(ics.code=subset(app.results, exit.status==2)$ics.code, hs.code=subset(app.results, exit.status==2)$hs.code.6, source="app"))
complete = rbind(complete, data.frame(ics.code=correspondence$ics.code, hs.code=correspondence$hs.code, source="WTO"))



## 2a Produce a set of ICS codes that do not have any assignment yet and aren't still in the process in the app. Look at only codes appearing in WTO notifications (codes.to.check) and those that were not classified as a service in the app (exit status 3).
manual.check = subset(ics.codes, ics.code %in% codes.to.check[!(codes.to.check %in% complete$ics.code) & !(codes.to.check %in% subset(app.results, exit.status %in% c(1,3))$ics.code)])
# manual.check = subset(ics.codes, ics.code %in% codes.to.check[!(codes.to.check %in% complete$ics.code) & !(codes.to.check %in% subset(app.results, exit.status %in% c(3))$ics.code)])
manual.check$sector = stringr::str_extract(manual.check$ics.code, "\\d{2}")
manual.check = subset(manual.check, ! grepl("(\\.01$)|(\\.99$)", ics.code)) ## Reduction 2: taking out general and other codes
manual.check = subset(manual.check, !(sector %in% c("01","03", "07"))) ## Reduction 3: taking out chapter 1,3 and 7

manual.check$hs.code = as.character(NA)
openxlsx::write.xlsx(manual.check, paste0(path, "help files/Manual.ICS.xlsx"))

manually.added = openxlsx::read.xlsx(paste0(path, "help files/Manual.ICS.checked.xlsx"))
manually.added=cSplit(manually.added, which(colnames(manually.added)=="hs.code"), direction="long", sep=",")

complete = rbind(complete, data.frame(ics.code=subset(manually.added, hs.code!="-")$ics.code, hs.code=subset(manually.added, hs.code!="-")$hs.code, source="manual"))

complete = unique(complete)



## 2b Making sure all HS codes are 6-digt HS2012 codes
complete$hs.code <- as.numeric(as.character(complete$hs.code))

length(unique(complete$ics.code))
length(unique(subset(complete, (hs.code %in% gtalibrary::hs.codes$hs.code))$ics.code))

nonexistent.hs.codes = subset(complete, !(hs.code %in% gtalibrary::hs.codes$hs.code))
for(hs in 1:nrow(nonexistent.hs.codes)) {
  nonexistent.hs.codes$hs.code[hs] <- paste(gta_hs_code_check(gta_hs_vintage_converter(codes=as.numeric(nonexistent.hs.codes$hs.code[hs]))), collapse = ",")
}
complete = rbind(nonexistent.hs.codes, subset(complete, (hs.code %in% gtalibrary::hs.codes$hs.code)))
complete=cSplit(complete, which(colnames(complete)=="hs.code"), direction="long", sep=",")

complete$hs.code <- as.numeric(as.character(complete$hs.code))




## 2c Unmatched ICS codes
unmatched.ics.codes = unique(subset(products.ics, !(ics.code %in% complete$ics.code))$ics.code)


## 2d Cross-check HS code assignment across sources
test = subset(complete,ics.code %in% subset(aggregate(source ~ ics.code, complete, function(x) length(unique(x))), source > 1)$ics.code)
test = aggregate(source ~ ics.code + hs.code, test, function(x) length(unique(x)))
paste("Out of", length(unique(test$ics.code)), "ICS codes that have assigned HS codes from multiple sources, only", length(unique(subset(test, source>1)$ics.code)), "have the same HS code specified from at least 2 sources.")
rm(test)


##############################################################################################
## 3: Cross-check With Julia Schmidt's correspondence
julia = openxlsx::read.xlsx(paste0(path, "data/ICS_HS_mapping_table.xlsx"))[,1:2]
julia$id = paste(as.numeric(julia$hs4), julia$ics, sep = "-")

complete$hs4 = gsub('.{2}$', '', complete$hs.code)
complete$ics5 = substr(complete$ics.code,1,6)
complete$id = paste(complete$hs4, complete$ics5, sep = "-")
complete$hs4 = NULL
complete$ics5 = NULL

complete$compatible = as.numeric(complete$id %in% julia$id)
complete$id = NULL

paste("Only", length(unique(subset(complete, compatible==1)$ics.code)), "out of", length(unique(subset(complete)$ics.code)), "ICS codes have corresponding HS codes that are in line with Schmidt et al.")
paste("Out of", nrow(complete), "HS-ICS correspondences, only", nrow(subset(complete, compatible==1)), "are in line with Schmidt et al.")
complete$compatible = NULL
rm(julia)


##############################################################################################
## 4: Check if all important HS codes assigned

top.threshold=500 ## Top X HS codes for CS commonwealth trade need to have a correspondence to at least one ICS code
countries = openxlsx::read.xlsx("0 projects/40 Commonwealth/help files/Commonwealth members.xlsx")
countries = countries$un_code

gtalibrary::gta_trade_value_bilateral(exporting.country = countries, keep.exporter = T, trade.data = c(2017))
exports = trade.base.bilateral
gtalibrary::gta_trade_value_bilateral(importing.country = countries, keep.importer = T, trade.data = c(2017))
trade.data = rbind(exports, trade.base.bilateral); rm(exports, trade.base.bilateral)
# subset(country.correspondence, un_code %in% countries[!(countries %in% c(unique(trade.data$i.un)))])$name #no imports for multiple countries, mentionable only Bangladesh
# subset(country.correspondence, un_code %in% countries[!(countries %in% c(unique(trade.data$a.un)))])$name #no exports for Pitcairn

main.products = aggregate(trade.value ~ hs6, trade.data, sum)
main.products = main.products[order(-main.products$trade.value),]
main.products = main.products$hs6[1:top.threshold]
rm(trade.data, parameter.choice.trade.base)


## HS codes without assigned ICS codes:
unassigned.hs.codes = main.products[!(main.products %in% complete$hs.code)]
unassigned.hs.codes = data.frame(hs.code=unassigned.hs.codes)
openxlsx::write.xlsx(unassigned.hs.codes, file=paste0(path, "help files/Unassigned HS codes.xlsx"))

added.hs.codes = openxlsx::read.xlsx(paste0(path, "help files/Unassigned HS codes.checked.xlsx"))
added.hs.codes=cSplit(added.hs.codes, which(colnames(added.hs.codes)=="ics.code"), direction="long", sep=",")
complete = rbind(complete, data.frame(ics.code=subset(added.hs.codes, is.na(ics.code)==F)$ics.code, hs.code=subset(added.hs.codes, is.na(ics.code)==F)$hs.code, source="manual"))


complete = aggregate(source ~ ics.code + hs.code, complete, function(x) paste(x, collapse = ";"))

##############################################################################################
## 5. Save ICS-HS correspondence
save(complete, file = paste0(path, "results/ics.hs.correspondence.Rdata"))
