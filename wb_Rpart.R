library(RMySQL)
library(ggplot2)
library(GGally)
library(dplyr)
library(stringr)
library(party)
library(caret)

# connect ro mysql database
mydb = dbConnect(MySQL(), user='root', password='root',dbname = 'wb_homicide',host='localhost')

dbListTables(mydb)

# create dataframes from tables
wb = dbReadTable(mydb,'wb')
imf = dbReadTable(mydb,'imf')
region_dat = dbReadTable(mydb,"region_dat")

#check differences between first 2 tables
setdiff(wb$country,imf$country)
setdiff(imf$country,wb$country)

# make all common countries the same 
change = list("Hong Kong SAR" = "Hong Kong SAR, China", "Papua New\xa0Guinea" = "Papua New Guinea",
"S\xe3o Tom\xe9 and Pr\xedncipe" = "Sao Tome and Principe","Korea" = "Korea, Rep.",
"Syria2" = "Syrian Arab Republic","Sudan1" = "Sudan", "The Bahamas" = "Bahamas, The",
"Democratic Republic of the Congo" = "Congo, Dem. Rep.", "Republic of Congo" = "Congo, Rep.",
"C\xf4te d'Ivoire" = "Cote d'Ivoire", "Egypt" = "Egypt, Arab Rep.", "The Gambia" = "Gambia, The",
"Iran" = "Iran, Islamic Rep.", "Lao P.D.R." = "Lao PDR", "FYR Macedonia" = "Macedonia, FYR",
"Micronesia" = "Micronesia, Fed. Sts.", "Russia" = "Russian Federation",
"Venezuela" = "Venezuela, RB", "Yemen" = "Yemen, Rep.")

for(i in 1:length(change)){
  imf$country = str_replace(imf$country,names(change)[i],change[i])
}

setdiff(imf$country,wb$country)

# combine imf and world bank data
combi <- inner_join(imf,wb,"country")

# repeat for the region data
set2 <- list ("Bahamas" = "Bahamas, The", "Cape Verde" = "Cabo Verde",
"Congo, The Democratic Republic of the" = "Congo, Dem. Rep.",
"C\xf4te D'Ivoire" = "Cote d'Ivoire",
"Cura?ao" = "Curacao", "Faroe Islands" = "Faeroe Islands", "Gambia" = "Gambia, The",
"Hong Kong" = "Hong Kong SAR, China", "Iran, Islamic Republic of" = "Iran, Islamic Rep.",
"Korea, Democratic People's Republic of"  = "Korea, Dem. Rep.",
"Korea, Republic of"  = "Korea, Rep.", "Kyrgyzstan" = "Kyrgyz Republic",
"Lao People's Democratic Republic" = "Lao PDR","Macau" = "Macao SAR, China", "Macedonia" ="Macedonia, FYR",
"Saint Martin" = "St. Martin (French part)", "Slovakia" = "Slovak Republic",
"Saint Kitts and Nevis" = "St. Kitts and Nevis", "Saint Lucia" = "St. Lucia",
"Russian Federationn Federation" = "Russian Federation", "Tanzania, United Republic of" = "Tanzania",
"Virgin Islands, U.S." = "Virgin Islands (U.S.)",
"Saint Vincent and the Grenadines" ="St. Vincent and the Grenadines","Egypt" ="Egypt, Arab Rep."
)

for(i in 1:length(set2)){
  region_dat$country <- str_replace(region_dat$country,names(set2)[i],set2[i])
}

region_dat$country[40] <- "Congo, Rep."

setdiff(combi$country,region_dat$country)

#join dataframes the remove index and other region column
wb_combo <- inner_join(combi,region_dat,"country")
wb_combo <- wb_combo[-c(10,11)]

# make imf_grade and region factor variables 
wb_combo$imf_grade <- as.factor(wb_combo$imf_grade)
wb_combo$region <- as.factor(wb_combo$region)

#see correlation to homicides
cor(wb_combo[-c(1,2,10)],use ="pairwise.complete.obs")[,1]

# Do Some Plots
qplot(homicides, data = wb_combo, geom = "bar", main = "Homicides")
qplot(homicides, data = wb_combo, geom = "density", main = "Homicides")

qplot(region,homicides, data = wb_combo, fill = region, geom = "boxplot","jitter",
      main = "Homicides by Region")

qplot(imf_grade, homicides, data = wb_combo,fill = imf_grade, geom = "boxplot","jitter",
      main = "Homicide by Economy (IMF)")


ggpairs(wb_combo, columns = c(3,4,5,6,7))
ggpairs(wb_combo, columns = c(3,8,9))

qplot(x = net_migration,homicides, data = wb_combo,log = "x")
qplot(x = population_total,homicides, data = wb_combo,log = "x")
qplot(x = water_urban,homicides, data = wb_combo,log = "x")
qplot(x = land_area,homicides, data = wb_combo,log = "x")
qplot(x = internet_users,homicides, data = wb_combo)
qplot(x = income_highest_10,homicides, data = wb_combo,log = "x")

# Try a Decision Tree (Regression)
country_tree <- ctree(homicides ~imf_grade + region + net_migration + water_urban + land_area + internet_users + income_highest_10, data = wb_combo)
print(country_tree)
plot(country_tree, type = "simple")

# Make homicides a factor from its quantiles  
x <- (wb_combo$homicides)
qnt <- quantile(x,seq(0,1,.25))


homi_quantiles <- cut(x,unique(qnt),include.lowest=TRUE)
levs <- levels(homi_quantiles)

homi_quantiles <- cut(x,unique(qnt),include.lowest=TRUE,labels = c("low","med_low","med_high","high"))

wb_combo$homi_quantiles <- homi_quantiles

# Plot Again (Classification)
country_tree2 <- ctree(homi_quantiles ~ imf_grade + region + net_migration + water_urban + land_area + internet_users + income_highest_10, data = wb_combo)
print(country_tree2)
plot(country_tree2)


# Try Again --- this time making predictions!
# Remove USA
wb_combo1 <- wb_combo[wb_combo$country != "United States",]

# Create training data --- 80%
trainIndex <- createDataPartition(wb_combo1$homi_quantiles, p = .8,
                                  list = FALSE,
                                  times = 1)

wbTrain <- wb_combo1[ trainIndex,]
wbTest  <- wb_combo1[-trainIndex,]


# Create and Plot Tree
country_tree3 <- ctree(homi_quantiles ~ imf_grade + region + net_migration + water_urban + land_area + internet_users + income_highest_10, data = wbTrain)
print(country_tree3)
plot(country_tree3)

# Make predictions on test data 
preds <- predict(country_tree3,wbTest)

# Look at real to predicted
data.frame(wbTest[c("country","homi_quantiles")],preds)

table(preds,wbTest$homi_quantiles)

# Predict for United States
predict(country_tree3,wb_combo[wb_combo$country == "United States",-11])
wb_combo[wb_combo$country == "United States",]

