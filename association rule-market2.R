# read file
 
market <-read.csv("Online Retail.csv",header=T,
                   colClasses="factor")

names(market)
head(market)
tail(market)
summary(market)
str(market)

install.packages("arules") # install "arules" package.
library(arules) # activate "arules" package 

sapply(market, function(x) sum(is.na(x)))


#choose column we need
market2 <- subset(market, select = c(InvoiceNo,Description))  
market2 
head(market2)  
tail(market2)
str(market2)

# convert dataframe to transaction format using ddply and
install.packages("plyr")
library(plyr)

market2_itemList <- ddply(market2,'InvoiceNo', function(market2)paste
                          (market2$Description, collapse = ","))
head(market2_itemList)

#remove the column 'Invoice'.
market2_itemList$InvoiceNo <- NULL


#rename the only column head left in the data set.
colnames(market2_itemList) <- c("Description")
head(market2_itemList)



#export the data set to be worked upon to a csv format file, for a back up.
write.csv(market2_itemList,"Online Retail.csv", quote = FALSE, row.names = TRUE)

#association rule mining algorithm: 'apriori'
install.packages("arules")
library(arules)

#convert the csv file to basket format and inspect it.
txn = read.transactions(file="Online Retail.csv", 
                        rm.duplicates= FALSE, format="basket",sep=",",cols=1);
inspect(head(txn))
itemFrequencyPlot(txn, topN=10)
#remove the quotes from transactions.
txn@itemInfo$labels <- gsub("","",txn@itemInfo$labels)

# run the apriori algorithm.
basket_rules1 <- apriori(txn,parameter = list
                         (sup = 0.01, conf = 0.8, target="rules"))

basket_rules2 <- apriori(txn,parameter = list
                         (sup = 0.01, conf = 0.25, target="rules"))

#Note: Here we have created 2 different
#basket rules. One with high confidence and low 
#support and the other with high support and low confidence.
#Now we view the rules that we have created.

summary(basket_rules1)
summary(basket_rules2)
install.packages("arulesViz")

#We now convert to the basket rules to dataframe and view them.
#Also, we give suitable transformations to the 'confidence' and 
#'support' parameters.


market_basket1 <-  as(basket_rules1,"data.frame")
head(market_basket1)
tail(market_basket1)

market_basket1$confidence <- market_basket1$confidence * 100
market_basket1$support <- market_basket1$support * nrow(market_basket1)
head(market_basket1)


market_basket2 <- as(basket_rules2,"data.frame")
head(market_basket2)
tail(market_basket2)


#split lhs and rhs into two columns.
install.packages("reshape2")
library(reshape2)

market_basket_split1 <- transform(market_basket1, rules = colsplit
                                  (rules, pattern = " => ", names = c("lhs","rhs")))

market_basket_split1

head(market_basket_split1)
tail(market_basket_split1)

market_basket_split2 <- transform(market_basket2, rules = colsplit
                                  (rules, pattern = " => ", names = c("lhs","rhs")))
head(market_basket_split2)
tail(market_basket_split2)

install.packages("arulesViz")
install.packages("grid")
library(arulesViz)
library(arules)
plot(basket_rules1)
plot(basket_rules2)


# code to Plot the rules in groups
plot(basket_rules1, method="grouped") 
plot(basket_rules2, method="grouped") 
#scatterplot matrix to compare the support, 
plot(basket_rules1@quality) 
plot(basket_rules2@quality) 

plotly_arules(basket_rules1, measure = c("support", "lift"), shading = "confidence")
plotly_arules(basket_rules2, measure = c("support", "lift"), shading = "confidence")

## using interactive manipulations and visualization using shiny.
plot(basket_rules1, method="paracoord")
plot(basket_rules2, method="paracoord")

rules_ex <-apriori(market_basket1,
                   parameter =list(minlen=1,maxlen=4,conf=0.8))
ruleExplorer(rules_ex)

rules_ex <-apriori(market_basket2,
                   parameter =list(minlen=2,maxlen=4,conf=0.25))
ruleExplorer(rules_ex)








