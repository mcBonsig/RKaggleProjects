# Mike Bonsignore
# CIS 468
# Assignment 2
# 3/12/23

# imports
library(ggplot2)
library(gridExtra)
library(stringr)
supcomp_data <- read.csv(file = "supecomputer 2021.csv", stringsAsFactors = T)

# learning about data
names(supcomp_data)
summary(supcomp_data)
View(supcomp_data)

fulldata1 <- ggplot(supcomp_data, aes(x=Total.Cores)) + geom_density()
fulldata2 <- ggplot(supcomp_data, aes(x=Power.Efficiency..GFlops.Watts.)) + geom_density()
fulldata3 <- ggplot(supcomp_data, aes(x=Cores.per.Socket)) + geom_density()
fulldata4 <- ggplot(supcomp_data, aes(x=Processor.Speed..MHz.)) + geom_density()
fulldata5 <- ggplot(supcomp_data, aes(x=Year)) + geom_bar()
fulldata6 <- ggplot(supcomp_data, aes(x=Continent)) + geom_bar() + coord_flip()

grid.arrange(fulldata1, fulldata2, fulldata3, fulldata4, fulldata5, fulldata6, ncol = 2)

##### DATA TRANSFORMATIONS #####

### pt 1 Creating subsets based on cores per socket
sort(unique(supcomp_data$Cores.per.Socket))
ggplot(supcomp_data, aes(x=Cores.per.Socket)) + geom_density()

CPS_12 <- subset(supcomp_data, Cores.per.Socket<=12)

CPS_12_22 <- subset(supcomp_data, Cores.per.Socket>12 & Cores.per.Socket<=22)

CPS_22_36 <- subset(supcomp_data, Cores.per.Socket>22 & Cores.per.Socket<=36)

CPS_38_68 <- subset(supcomp_data, Cores.per.Socket>36 & Cores.per.Socket<=68)
#removed machine with 260 cores per socket because outlier status

#Visualizations
CPSdensity1 <- ggplot(CPS_12, aes(x=Rank)) + 
  geom_density()

CPSdensity2 <- ggplot(CPS_12_22, aes(x=Rank)) + 
  geom_density()

CPSdensity3 <- ggplot(CPS_22_36, aes(x=Rank)) + 
  geom_density()

CPSdensity4 <- ggplot(CPS_38_68, aes(x=Rank)) + 
  geom_density()

grid.arrange(CPSdensity1,CPSdensity2,CPSdensity3,CPSdensity4,ncol=2)

ggplot(supcomp_data, aes(x=Power.Efficiency..GFlops.Watts.)) + geom_density()
ggplot(CPS_12, aes(x=Power.Efficiency..GFlops.Watts.)) + geom_density()
ggplot(CPS_38_68, aes(x=Power.Efficiency..GFlops.Watts.)) + geom_density()

# Since this works like a game of golf, Technically, machines in subset 4 (38-68 cores per socket) rank the highest! makes sense...

# Descriptive statistics

summary(CPS_12)
summary(CPS_12_22)
summary(CPS_22_36)
summary(CPS_38_68)

CPS_Year1 <- ggplot(CPS_12, aes(x=Year)) + geom_density()
CPS_Year2 <- ggplot(CPS_38_68, aes(x=Year)) + geom_density()
CPS_PE1 <- ggplot(CPS_38_68, aes(x=Power.Efficiency..GFlops.Watts.)) + geom_density()
CPS_PE1 <- ggplot(CPS_12, aes(x=Power.Efficiency..GFlops.Watts.)) + geom_density()
CPS_PE2 <- ggplot(CPS_38_68, aes(x=Power.Efficiency..GFlops.Watts.)) + geom_density()


grid.arrange(CPS_Year1, CPS_Year2, ncol=2)

CPS_Year_Skew1 <- (3*(mean(CPS_12$Year, na.rm = T)
                           - median(CPS_12$Year, na.rm = T)
                           ))/sd(CPS_12$Year, na.rm = T)

CPS_Year_Skew2 <- (3*(mean(CPS_38_68$Year, na.rm = T)
                             - median(CPS_38_68$Year, na.rm = T)
                             ))/sd(CPS_38_68$Year, na.rm = T)

CPS_PE_Skew1 <- (3*(mean(CPS_12$Power.Efficiency..GFlops.Watts., na.rm = T)
                   - median(CPS_12$Power.Efficiency..GFlops.Watts., na.rm = T)
                   ))/sd(CPS_12$Power.Efficiency..GFlops.Watts., na.rm = T)

CPS_PE_Skew2 <- (3*(mean(CPS_38_68$Power.Efficiency..GFlops.Watts., na.rm = T)
                    - median(CPS_38_68$Power.Efficiency..GFlops.Watts., na.rm = T)
                    ))/sd(CPS_38_68$Power.Efficiency..GFlops.Watts., na.rm = T)


median(CPS_12$Year)
median(CPS_38_68$Year)
mean(CPS_12$Year)
mean(CPS_38_68$Year)
median(CPS_12$Power.Efficiency..GFlops.Watts., na.rm = T)
median(CPS_38_68$Power.Efficiency..GFlops.Watts., na.rm = T)
mean(CPS_12$Power.Efficiency..GFlops.Watts., na.rm = T)
mean(CPS_38_68$Power.Efficiency..GFlops.Watts., na.rm = T)
CPS_Year_Skew1
CPS_Year_Skew2
CPS_PE_Skew1
CPS_PE_Skew2
# Follows Moore's Law!

### pt 2 re-scaling variables using min max normalization
supcomp_data$Total.Cores_mmnorm <- (supcomp_data$Total.Cores - min(supcomp_data$Total.Cores))/(max(supcomp_data$Total.Cores) - min(supcomp_data$Total.Cores))
TotalCores_Norm <- ggplot(supcomp_data, aes(Total.Cores_mmnorm))+ 
  geom_density()

supcomp_data$Rpeak..TFlop.s._mmnorm <- (supcomp_data$Rpeak..TFlop.s. - min(supcomp_data$Rpeak..TFlop.s.))/(max(supcomp_data$Rpeak..TFlop.s.) - min(supcomp_data$Rpeak..TFlop.s.))
PeakTFlops_Norm <- ggplot(supcomp_data, aes(Rpeak..TFlop.s._mmnorm))+ 
  geom_density()

supcomp_data$Processor.Speed..MHz._mmnorm <- (supcomp_data$Processor.Speed..MHz. - min(supcomp_data$Processor.Speed..MHz.))/(max(supcomp_data$Processor.Speed..MHz.) - min(supcomp_data$Processor.Speed..MHz.))
ProcessorSpeed_Norm <- ggplot(supcomp_data, aes(Processor.Speed..MHz._mmnorm))+
  geom_density()

grid.arrange(TotalCores_Norm, PeakTFlops_Norm, ProcessorSpeed_Norm, ncol = 1)

### pt 3 equal width binning of year of creation
sort(unique(supcomp_data$Year))
supcomp_data$Year_bin <- as.factor(cut(supcomp_data$Year,5, labels=F))
ggplot(supcomp_data, aes(x=Year_bin)) + 
  geom_bar()


TC_Yearbin <- ggplot(supcomp_data, aes(x=Year_bin, y=Total.Cores, fill = Continent)) + 
  geom_col() + scale_fill_brewer(palette="Set1")

TFlops_Yearbin <- ggplot(supcomp_data, aes(x=Year_bin, y=Rpeak..TFlop.s., fill = Continent)) + 
  geom_col() + scale_fill_brewer(palette="Set1")

PE_Yearbin <- ggplot(supcomp_data, aes(x=Year_bin, y=Power.Efficiency..GFlops.Watts., fill = Continent)) + 
  geom_col() + scale_fill_brewer(palette="Set1")

grid.arrange(TC_Yearbin, TFlops_Yearbin, PE_Yearbin, ncol = 3)
