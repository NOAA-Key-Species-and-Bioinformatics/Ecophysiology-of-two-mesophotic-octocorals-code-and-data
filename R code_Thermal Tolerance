#loading in libs for thermal tolerance
```{r}
install.packages(c('tidyverse','ggpubr','rstatix'))
```


```{r}
library(xlsx)
library(rstatix)
library(reshape)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(plyr)
library(datarium)
```

#Loading in data for respirometry analyses
```{r}
#swiftiadata

swiftresp <- read_csv("Swiftiaresp.csv")
sq10.1<- read_csv("SwiftiaQ10all.csv")


#muriceadata
muricearesp <- read_csv("muricearesp.csv")
mq10.1 <- read_csv("MuriceaQ10all.csv")

```

#approach 1: repeated measurements analysis (ANOVA) with temperature as a covariate

```{r}
#testing swiftresp data for assumptions and running anova

#respiration rates: testing for outliers

#swiftia
summary<-swiftresp %>%
group_by(temp) %>%
get_summary_stats(rate, type = "mean_sd")
data.frame(summary)

outliers<-swiftresp %>%
group_by(temp) %>%
identify_outliers(rate)
data.frame(outliers) #no outlier detected in dataset

#muricea
summarym<-muricearesp %>%
group_by(temp) %>%
get_summary_stats(rate, type = "mean_sd")
data.frame(summarym)

outlierm<-muricearesp %>%
group_by(temp) %>%
identify_outliers(rate)
data.frame(outlierm) #outlier detected in dataset (22 m2)
```

```{r}
#testing normality in swiftresp

#swiftia
normality<-swiftresp %>%
group_by(temp) %>%
shapiro_test(rate)
data.frame(normality) #22 degrees is not normal (p=0.01)

#muricea
normalitym<-muricearesp %>%
group_by(temp) %>%
shapiro_test(rate)
data.frame(normalitym) #all normal
```

```{r}
#testing sphericity and running anova

#swiftia
res<-anova_test(data=swiftresp,dv=rate,wid=id,within=temp) 
get_anova_table(res) #cannot use because 22 dataset was not normal- have to use friedman test (nonparametric alternative)

#muricea
resm<-anova_test(data=muricearesp,dv=rate,wid=id,within=temp) 
get_anova_table(resm) #cannot use because 22 dataset contained outlier- have to use friedman test (nonparametric alternative)

```

```{r}
#Freidman test

#swiftia
swift.freid <- swiftresp %>% friedman_test(rate ~ temp |id)
swift.freid #no significance in swifta

#muricea
mur.freid <- muricearesp %>% friedman_test(rate ~ temp |id)
mur.freid #no significance in muricea
```

#approach 2: transform Q10 values and test to see if mean is significantly different from 1
```{r}
#testing swiftia data for normality
q10normality<-sq10.1 %>%
shapiro_test(q10)
data.frame(q10normality) #normal

#testing muricea data for normality
q10normalitym<-mq10.1 %>%
shapiro_test(q10)
data.frame(q10normalitym) #not normal- needs to be transformed

#transform both datasets the same since one is normal and one is not. cube root:

#swiftia
cuberoot_swift <- sq10.1$q10^(1/3)
q10normality2<-cuberoot_swift %>%
shapiro_test(q10)
data.frame(q10normality2) #way better (0.97)

cuberoot_mur <- mq10.1$q10^(1/3)
q10normalitym2<-cuberoot_mur %>%
shapiro_test(q10)
data.frame(q10normalitym2) #also way better (0.76)

#will use this transformation
```

```{r}
#testing if the transformed values are significantly different from 1 using a t.test

cubem <- murcube
t.test(cq10 ~ group, data = cubem) #not significantly different from 1

cubes <- swiftcube
t.test(q10 ~ group, data = cubes) #not significantly different from 1
```

#running analyses for polyp activity

#loading in the data for the analyses

library(dplyr)

# load data
polyp <- read.csv("PolypData.csv", header = TRUE, check.names = TRUE, stringsAsFactors = FALSE)
colnames(polyp)
head(polyp)

```

```{r}
#testing assumptions for parametric pearson correlation test

#normality checks (shapiro-wilk test)
shapiro.test(polyp$polyp_activity) #not normal (p=0.037)

#visually assess linearity
plot(polyp$temperature, polyp$polyp_activity)
abline(lm(polyp_activity ~ temperature, data = polyp), col = "red") #not linear

#data does not meet assumptions, so will use nonparametric alternative (spearman)
```

```{r}
# Spearman correlation by species
species_list <- unique(polyp$species)

for (sp in species_list) {
  cat("\nSpecies:", sp, "\n")
  sub_data <- filter(polyp, species == sp)
  result <- cor.test(sub_data$temperature, sub_data$polyp_activity, method = "spearman")
  cat("  Spearman correlation:", round(result$estimate, 4), "\n")
  cat("  p-value:", signif(result$p.value, 4), "\n")
}

```
