# Possible dependencies for arulesViz (run once )
# install . packages (c(" igraph " , " tidygraph " , " ggraph "))
# Install packages if you haven ’t already (run once )
# install . packages (c(" arules " , " arulesViz "))

library ( arules )
library ( arulesViz )

# Load the famous Groceries dataset (1 month of real - world point -of - sale data )
data ("Groceries")
 
# 1. Check the dimensions and density of the dataset
summary(Groceries)
 
# 2. Peek at the first 5 receipts ( transactions )
inspect(Groceries [1:5])
 
# 3. Check the exact support ( frequency ) of the first 3 items
itemFrequency (Groceries [ , 1:3])
 
# 4. Plot the Top 15 most popular items in the store
itemFrequencyPlot (Groceries, topN = 15 , type = "relative",
                        col = "steelblue",
                        main = "Top 15 Most Purchased Items",
                        ylab = "Support ( Proportion of Receipts )")

# Mine frequent itemsets with min _ support = 0.01 and at least 2 items
freq_items <- eclat ( Groceries ,parameter = list ( supp = 0.01 , minlen = 2 , maxlen = 5) )

# Sort by support and inspect the top 10 pairs / groups
inspect( sort ( freq_items , by = "support") [1:10])

# Mine association rules
rules <- apriori ( Groceries,
                     parameter = list ( supp = 0.005 , conf = 0.50 , minlen = 2) )

# How many rules did we find ?
length ( rules )

# Sort by CONFIDENCE and look at the top 5
inspect ( sort( rules , by = "confidence") [1:5])

# Sort by LIFT and inspect the top 5
inspect ( sort ( rules , by = "lift") [1:5])

# Prune redundant rules ( rules that add extra items without improving confidence )
clean_rules <- rules [!is.redundant ( rules ) ]
length ( clean_rules )

# Question 1: What drives customers to buy berries ? ( berries on the RHS )
berry_rules <- subset ( Groceries_rules <- apriori ( Groceries ,
                        parameter = list ( supp = 0.002 , conf = 0.20) ) ,
                        subset = rhs %in% "berries" & lift > 2)
inspect ( sort ( berry_rules , by = "lift") )

# Question 2: If someone grabs root vegetables (LHS ), what else do they buy ?
veggie_rules <- subset ( clean_rules , subset = lhs %in% "root vegetables")
inspect ( sort ( veggie_rules , by = "lift") [1:5])


# 1. Scatter plot of all clean rules ( Support vs. Confidence , shaded by Lift )
plot( clean_rules , measure = c("support", "confidence") , shading = "lift", jitter = 0)

# 2. Grab the top 15 rules by Lift for a clean network graph
top15_lift <- head ( sort ( clean_rules , by = "lift") , 15)

# 3. Plot as a directed network graph ( Circles = Lift / Support , Arrows = Rules )
plot( top15_lift , method = "graph", engine = "htmlwidget")

# 4. Grouped Matrix plot ( great for spotting clusters of related rules )
plot ( clean_rules , method = "grouped")

## Challenge 1: The Beer & Bottled Water Showdown

# 1. Mine with the lower thresholds

drink_rules <- apriori(Groceries,
                       parameter = list(supp = 0.001, conf = 0.15, minlen = 2))

# 2. Make the two subsets

beer_rules <- subset(drink_rules, subset = lhs %in% "bottled beer")
soda_rules <- subset(drink_rules, subset = lhs %in% "soda")


# 3. Compare the top 5 by lift

inspect(head(sort(beer_rules, by = "lift"), 5))
inspect(head(sort(soda_rules, by = "lift"), 5))


## Challenge 2: Hunting for Negative Correlations (Lift < 1)

#1. Mine with low confidence

neg_rules <- apriori(Groceries,
                     parameter = list(supp = 0.01, conf = 0.05, minlen = 2))

#2. Filter, show the empty result, then loosen it

length(subset(neg_rules, subset = lift < 0.75))   # 0
low_lift <- subset(neg_rules, subset = lift < 1)  # 20 rules
inspect(head(sort(low_lift, by = "lift", decreasing = FALSE), 6))

#3. Verify by hand

supp <- itemFrequency(Groceries)
supp_A  <- supp["rolls/buns"]    # 0.1839  (1809 receipts)
supp_B  <- supp["canned beer"]   # 0.0777  (764 receipts)
supp_AB <- 111 / length(Groceries)  # 0.0113  (111 of 9835 receipts)
supp_AB / (supp_A * supp_B)      # 0.7899

# Challenge 3: The “Adult” Census Investigation (Beyond Groceries!)

#1–2. Load and mine

data("Adult")
income_rules <- apriori(Adult,
                        parameter = list(supp = 0.02, conf = 0.50),
                        appearance = list(rhs = c("income=large"), default = "lhs"))

# 3. Prune, sort, plot

clean_income <- income_rules[!is.redundant(income_rules)]
top10_income <- head(sort(clean_income, by = "lift"), 10)
inspect(top10_income)
plot(top10_income, method = "graph")


