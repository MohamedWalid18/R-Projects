data1= read.csv('D:/feps  fourth year curriculum/CDA/mini project/Thailand resources/recoded_data.csv')
dim(data1)

library('epitools') # for odds ratio
library(DescTools)   # for gamma coefficient & Uncertainty coefficient
library("vcd") # for pearson chi-square 3-way conditional independence 

# 2-way contingency table for Q48 & Q46
crosstable1 = xtabs(~ data1$Q48..How.much.freedom.of.choice.and.control +data1$Q46..Feeling.of.happiness , data = data1)

GoodmanKruskalGamma(crosstable1, conf.level = 0.95)  # Gamma measure of ordinal association

#######

crosstable2 = xtabs(~ data1$Q50..Satisfaction.with.financial.situation.of.household +data1$Q46..Feeling.of.happiness , data = data1)

GoodmanKruskalGamma(crosstable2, conf.level = 0.95)

#######

crosstable3 = xtabs(~ data1$Q54..Frequency.you.family..last.12.month...Gone.without.a.cash.income +data1$Q46..Feeling.of.happiness , data = data1)

UncertCoef(crosstable3) # since one of the variables is nominal with multiple categories

#######

crosstable4 = xtabs(~ data1$Q158..Science.and.technology.are.making.our.lives.healthier..easier..and.more.comfortable +data1$Q46..Feeling.of.happiness , data = data1)

GoodmanKruskalGamma(crosstable4, conf.level = 0.95)

#######


crosstable5 = xtabs(~ data1$Q260..Sex +data1$Q46..Feeling.of.happiness , data = data1)

GoodmanKruskalGamma(crosstable5, conf.level = 0.95)

# Pearson chi-square for a 3-way table 

three_way = xtabs(~ data1$Q48..How.much.freedom.of.choice.and.control +data1$Q46..Feeling.of.happiness +data1$Q50..Satisfaction.with.financial.situation.of.household  , data = data1)

mantelhaen.test(three_way)
oddsratio(three_way)

assocstats(three_way)


