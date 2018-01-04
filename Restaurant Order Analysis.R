install.packages('sas7bdat')
install.packages('arules')
install.packages('arulesViz')
library(sas7bdat)
library(arules)
library(arulesViz)
#read in data file from appropriate location on your computer
menu  = read.sas7bdat("C:\\Users\\Kimbe\\OneDrive\\Documents\\MSA\\Data Mining\\HW1\\restaurant2.sas7bdat")
summary(menu)
menu2 = subset(menu, select = -(orderid))

#have to transform into transactions in order to use apriori calls
transmenu <- as(menu2, "transactions")
summary(transmenu)
itemLabels(transmenu)

#generating the rules for the market basket / association analysis for each order type
meatrules <- apriori(data=transmenu, parameter=list(supp=0.001,conf = 0.05, minlen=2),
               appearance = list(lhs = c("meatorder=Filet Mignon", "meatorder=Duck", 
                                        "meatorder=Roast Chicken", "meatorder=Pork Tenderloin"),
                                             default="rhs"))
plot(meatrules, method="graph", control=list(type="items"))

#meat rules sorted by confidence level and displayed top 5 (then the rest)
meatrules <-  sort(meatrules, decreasing=TRUE,by="confidence")
inspect(meatrules[1:5])
inspect(meatrules)

meatrules <-  sort(meatrules, decreasing=TRUE,by="support")
inspect(meatrules[1:5])
inspect(meatrules)
#rules list shows individual recommendations with each meat order

#look at popular combinations of the duck entree
duckrules <- apriori(data=transmenu, parameter=list(supp=0.001,conf = 0.05, minlen=2),
                     appearance = list(lhs = "meatorder=Duck",
                                       default="rhs"))
duckrules <-  sort(duckrules, decreasing=TRUE,by="confidence")
inspect(duckrules)
plot(duckrules, method="graph", control=list(type="items"))


#people like the duck with roasted root veg, and duckhorn chardonnay

#look at popular combinations of the filet mignon entree
steakrules <- apriori(data=transmenu, parameter=list(supp=0.001,conf = 0.05, minlen=2),
                     appearance = list(lhs = "meatorder=Filet Mignon",
                                       default="rhs"))
steakrules <-  sort(steakrules, decreasing=TRUE,by="confidence")
inspect(steakrules)
plot(steakrules, method="graph", control=list(type="items"))
#people like the filet with roasted root veg and balckstone merlot 

#look at the roasted chicken pairings 
chickrules <- apriori(data=transmenu, parameter=list(supp=0.001,conf = 0.05, minlen=2),
                      appearance = list(lhs = "meatorder=Roast Chicken",
                                        default="rhs"))
chickrules <-  sort(chickrules, decreasing=TRUE,by="confidence")
inspect(chickrules)
plot(chickrules, method="graph", control=list(type="items"))
#people like the chicken with steamed seasonal veg and the chardonnay (lots of variety in the sides)

#look at the pork tenderloin pairings 
porkrules <- apriori(data=transmenu, parameter=list(supp=0.001,conf = 0.05, minlen=2),
                      appearance = list(lhs = "meatorder=Pork Tenderloin",
                                        default="rhs"))
porkrules <-  sort(porkrules, decreasing=TRUE,by="confidence")
inspect(porkrules)
plot(porkrules, method="graph", control=list(type="items"))
#people like the pork with the Pinot Bianco and roasted root veg  
