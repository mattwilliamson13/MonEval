require(MASS)
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)


infolder <- "C:/Users/Tyler/Google Drive/MonumentData/Generated Data"  # set folder holding input data

# load in dataframe with output variables for PAs
load(paste(infolder,"/PA_biological_variables.RData", sep=""))

# remove two NMs that were designated by inter-agency agreement
PA.df <- filter(PA.df, DesigAuth %in% c("Congress","President"))

# drop variables that are no longer needed or inaccurate
drops <- c("OrigAntiq", "AntiqYear", "GapStatus", "IucnCat", "OrigDesigAuth")
PA.df <- PA.df[ , !(names(PA.df) %in% drops)]

# Create a new Designation Mode variable to distinguish PAs that started as presidential NMs but are now congressional designations (call this variable DesigMode)
select.units <- c("Petrified Forest National Park", "Lassen National Park", "Grand Canyon National Park", "Pinnacles National Park", "Olympic National Park", 
                  "Zion National Park", "Acadia National Park", "Great Basin National Park", "Bryce Canyon National Park", "Carlsbad Caverns National Park", 
                  "Arches National Park", "Great Sand Dunes National Park", "Death Valley National Park", "Saguaro National Park", 
                  "Black Canyon of the Gunnison National Park", "Dry Tortugas National Park", "Joshua Tree National Park", "Capitol Reef National Park", 
                  "Channel Islands National Park", "Gulf Islands National Seashore", "Grand Teton National Park")
PA.df$DesigMode <- PA.df$DesigAuth
PA.df$DesigMode[which(PA.df$UnitName %in% select.units)] <- "President then Congress"


#############################################################################################################################


counts <- as.vector(with(PA.df, table(DesigMode, bailey.majority)))   # get count of observations for each combination of division and DesigMode
truehist(counts, h=1) 
print(prop.missing <- length(which(counts==0))/length(counts))  # proportion of unrepresented combinations

# function to add correlation values to matrix of pairwise scatterplots
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, use="pairwise.complete.obs"))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
mat <- PA.df[,10:26]  # get biological variables
pairs(mat, upper.panel=panel.cor)  # pairwise scatterplots

# If doing regression, should reduce to variables with r<0.7; from each highly correlated pair, keep the one with the greater correlation with response variable (DesigMode)




# Standard Anova on these data
response <- PA.df$mean.rich.mammal
mod1 <- lm(response ~ PA.df$DesigMode + PA.df$bailey.majority + PA.df$DesigMode:PA.df$bailey.majority)
ANOVA <- summary(aov(mod1))
cat( " The standard ANOVA for these data follows ","\n")
Fdesig <-  ANOVA[[1]]$"F value"[1]   # Saving F values for future use
Fbailey <-  ANOVA[[1]]$"F value"[2]
Finteract <-  ANOVA[[1]]$"F value"[3]
print(ANOVA)


print( "Resampling as in Manly with unrestricted sampling of observations. ")
# Now start resampling
nreps <- 5000 
FD <- numeric(nreps)    #Set up space to store F values as calculated.
FB <- numeric(nreps)  
FDB <- numeric(nreps)
FD[1] <- Fdesig          # The first F of our 5000 
FB[1] <- Fbailey
FDB[1] <- Finteract
pres.index <- which(PA.df$DesigMode=="President")
cong.index <- which(PA.df$DesigMode=="Congress")
presthencong.index <- which(PA.df$DesigMode=="President then Congress")
sampsize <- min(c(length(pres.index), length(cong.index), length(presthencong.index)))

for (i in 2:nreps) {
  pres.sample.index <- sample(pres.index, sampsize)
  cong.sample.index <- sample(cong.index, sampsize)
  presthencong.sample.index <- sample(presthencong.index, sampsize)
  all.sample.index <- c(pres.sample.index,cong.sample.index,presthencong.sample.index)
  new.response <- sample(response[all.sample.index], size=length(all.sample.index))
  mod2 <- lm(new.response ~ PA.df$DesigMode[all.sample.index] + PA.df$bailey.majority[all.sample.index] + PA.df$DesigMode[all.sample.index]:PA.df$bailey.majority[all.sample.index])
  b <- summary(aov(mod2))
  FD[i] <- b[[1]]$"F value"[1]
  FB[i] <- b[[1]]$"F value"[2]
  FDB[i] <- b[[1]]$"F value"[3]
}
probD <- length(FD[FD >= Fdesig + .Machine$double.eps ^0.5])/nreps
probB <- length(FB[FB >= Fbailey + .Machine$double.eps ^0.5])/nreps       
probDB  <-  length(FDB[FDB >= Finteract + .Machine$double.eps ^0.5])/nreps
### The addition of "+ .Machine$double.eps" is an aid against two numbers that differ only by
### floating point computer calculations at the extreme.

cat(" The probability value for the interaction is ",probDB, "\n")
cat(" The probability value for DesigMode is ", probD, "\n")
cat(" The probability value for bailey.majority is ", probB, "\n")








# MULTINOMIAL LOGISTIC REGRESSION

# should make a correlation matrix to pick out set of variables

# standardize predictor variables first? So that they can be compared more easily?

PA.df$DesigMode2 <- relevel(as.factor(PA.df$DesigMode), ref="Congress")  # set Congress as the reference level
test <- multinom(DesigMode2 ~ mean.rich.mammal + system.richness.rare, data=PA.df)
summary(test)


# Note that the (M)ANOVA and multinomial regression approaches are really asking different questions:
 # Does ecological value differ between Congressionally and presidentially designated protected areas? (ANOVA)
 # versus: is the designation mode of PAs affected by ecological values? (Multinomial regression)





