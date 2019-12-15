

################################################################
#
# R script for paper "The Fentanyl Crisis in Indianapolis: Estimating trends using multilevel Bayesian models"
#
# July 2017
#
#
################################################################



#download needed packages you don't have 
wants <- c("chron", "rethinking", "plyr", "doBy", "rgdal", "scales", "magrittr", "geojsonio", "leaflet", "htmlwidgets")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])

#load needed packages
sapply(wants, require, character.only = TRUE)




##########################
# load and prep data

d <- read.csv("/Users/PeterPhalen/Dropbox/Manuscripts/BRADRAY/coroner/MarionCountyOverdoseDeathDataset2010-2017thruApril.csv")

# Get dates of death as vectors of months, days, and years
DOD <- dates(as.character(d$DOD), format= "m/d/y")
DOD <- month.day.year(DOD)

# Drop DODs from May 2017-onwards as the data is incomplete
post.april.2017 <- (DOD$year == 2017) & (DOD$month > 4)
sum(post.april.2017) # this eliminates 20 people
d <- d[!post.april.2017,]


# Get column indexes for all the toxicology screen columns
drugs.index <- which(colnames(d)=="ALCOHOLS"):ncol(d)
# The tox data came to us as 1s and NAs. Set "NA" values in these columns to 0
d[drugs.index] <- lapply(d[drugs.index] , function(x) ifelse(is.na(x), 0, x) )

# Set unknown or incomplete zip codes to 0. We have the option of treating 
# these cases as a separate zip code for the purposes of the hierarchical model 
levels(d$Injury_Zip)[levels(d$Injury_Zip) == "#NULL!"] <- "0"
# but there are only 4 of them so
sum(d$Injury_Zip == 0) 
# we drop them
d <- d[!d$Injury_Zip == 0,] 

#  Zip codes are loaded as factors, convert them to numbers
d$Injury_Zip <- strtoi(d$Injury_Zip) 

# Race: 1 = White, 2 = Black, 3 = Other
d$Race <- ifelse(d$Race == 1, 1, ifelse(d$Race == 2, 2, 3))
d$Race <- as.integer(d$Race) # set to integer


# create index for interactions between race and sex
d <- transform( d,
                race_sex = ifelse(Sex == 0 & Race == 1, 1, # white female = 1
                                  ifelse( Sex == 0 & Race == 2, 2, # black female = 2  
                                          ifelse( Sex == 0 & Race == 3, 3, # other female = 3
                                                  ifelse( Sex == 1 & Race == 1, 4, # white male = 4
                                                          ifelse( Sex == 1 & Race == 2, 5, # black male = 5
                                                                  ifelse( Sex == 1 & Race == 3, 6, NA))))))) # other male = 6
d$race_sex <- as.integer(d$race_sex) # set it to integer

# This is a list of all the zip codes that intersect Marion county / Indianapolis
marion.county.zips <- c(46298,
                        46107,
                        46113,
                        46183,
                        46202,
                        46201,
                        46204,
                        46203,
                        46206,
                        46205,
                        46208,
                        46211,
                        46216,
                        46214,
                        46218,
                        46217,
                        46220,
                        46219,
                        46222,
                        46221,
                        46224,
                        46226,
                        46225,
                        46228,
                        46227,
                        46229,
                        46234,
                        46231,
                        46236,
                        46262,
                        46235,
                        46239,
                        46237,
                        46241,
                        46240,
                        46242,
                        46249,
                        46250,
                        46254,
                        46256,
                        46260,
                        46259,
                        46268,
                        46274,
                        46278,
                        46277,
                        46285)

# 107 zip codes in the dataset don't intersect Marion County
sum(!(d$Injury_Zip %in% marion.county.zips)) 

# We checked and these non-Marion County zip codes are
# often very far away, frequently nearer to other states
# than to the city of Indianapolis. 
# Restrict dataset to zip codes within Marion county
d <- d[d$Injury_Zip %in% marion.county.zips,]


# Four people were under 5 years old at death
sum(d$Age < 5)
# Drop these, as they presumably occurred under very different
# probabilistic processes. (The next youngest person was 16 at death)
d <- d[d$Age > 5,]


# Give every zip code a unique integer index, ordered from
# "lowest" to "highest"
zip.key <- unique(d$Injury_Zip)
zip.key <- sort(zip.key, decreasing= FALSE)
d$zip_code <- rep(NA, nrow(d))
for (i in 1:length(zip.key)){ 
  d$zip_code[which(d$Injury_Zip == zip.key[[i]])] <- i
}



# Calculate z-scored julian date of death (DOD)
DOD <- dates(as.character(d$DOD), format= "m/d/y")
DOD <- month.day.year(DOD)
d$julian <- julian(x=DOD$month, d= DOD$day, y=DOD$year)
d$julianZ <- (d$julian - mean(d$julian)) / sd(d$julian)


# Calculate z-scored age
d$ageZ <- (d$Age - mean(d$Age)) / sd(d$Age) 



# **** END DATA PREP



# DEFINE AND FIT MODEL

fentanyl.od.rate.model <- map2stan(
  alist(
    
    # outcome is a binary variable: 1 for positive Fentanyl tox screen else 0
    Fentanyl_tox_screen ~ dbinom(1, p ),
    
    logit(p) <- A + #intercept (defined below)
      ba * age + # coefficient for z-scored age
      ba2 * age^2 + # z-scored age ^2
      jul * age * bja + # interaction between z-scored age and date of death
      BJU * jul + # z-scored julian date of death, slope varying by race/gender (defined below)
      BJU2 * jul^2 + # z-scored julian date of death^2, slope varying by race/gender
      BJU3 * jul^3 , # z-scored julian date of death^3, slope varying by race/gender
    #   higher-order polynomials for age and date don't improve WAIC
    
    A <- a + a_ZIP[Injury_Zip] + # varying intercepts for zip codes (K=38)
      a_RACE_SEX[race_sex_ints], # varying intercepts for race and gender combinations (J=6)
    BJU <- ( bju + bju_RACE_SEX[race_sex_ints] ) , # varying slopes all DOD coefficients by race and gender combinations (J=6)
    BJU2 <- ( bju2 + bju2_RACE_SEX[race_sex_ints] ) ,
    BJU3 <- ( bju3 + bju3_RACE_SEX[race_sex_ints] ) ,
    
    
    # non-centered multivariate normal prior on intercept and slope terms for race/sex combinations
    # calculating Rho here allows us to check for correlations between intercepts and slopes
    c(a_RACE_SEX, bju_RACE_SEX, bju2_RACE_SEX, bju3_RACE_SEX)[race_sex_ints] ~ dmvnormNC(sigma_RACE_SEX, Rho_RACE_SEX),
    Rho_RACE_SEX ~ dlkjcorr(2), #LKJ prior set to 2 gives weak preference for weaker correlations
    sigma_RACE_SEX ~ dcauchy(0,2.5), # Estimate sigma for race/sex parameters
    
    
    a ~ dnorm(0,5), # Normal(0,5) prior on intercept
    
    a_ZIP[Injury_Zip] ~ dnorm(0,sigma_ZIP), # Estimate sigma for zip code parameters
    sigma_ZIP ~ dcauchy(0,2.5),
    
    # Set Normal(0,5) priors for all slope parameters. This may seem lazy but I checked wide ranges for the priors and we have enough data that it doesn’t really matter
    c(ba, ba2, bju, bju2, bju3, bja) ~ dnorm(0,5) 
    
  ),
  data= list(
    Fentanyl_tox_screen = d$Fentanyl_TOTAL, #positive Fentanyl tox screen (0s and 1s)
    jul = d$julianZ, # julian date z-scored
    Injury_Zip = d$zip_code, # zip code where the person overdosed
    age = d$ageZ,
    race_sex_ints = d$race_sex
    
  ),
  warmup=5000, iter=2e4, chains=4, cores=4, WAIC=F,
  control = list(adapt_delta = 0.99, max_treedepth = 15)
)



# Here is the Stan code produced by the above map2stan function:
# data{
#   int<lower=1> N;
#   int<lower=1> N_race_sex_ints;
#   int<lower=1> N_Injury_Zip;
#   int Fentanyl_tox_screen[N];
#   real jul[N];
#   real age[N];
#   int Injury_Zip[N];
#   int race_sex_ints[N];
# }
# parameters{
#   matrix[4,N_race_sex_ints] z_N_race_sex_ints;
#   cholesky_factor_corr[4] L_Rho_RACE_SEX;
#   vector<lower=0>[4] sigma_RACE_SEX;
#   real a;
#   vector[N_Injury_Zip] a_ZIP;
#   real<lower=0> sigma_ZIP;
#   real ba;
#   real ba2;
#   real bju;
#   real bju2;
#   real bju3;
#   real bja;
# }
# transformed parameters{
#   matrix[N_race_sex_ints,4] v_N_race_sex_ints;
#   vector[N_race_sex_ints] a_RACE_SEX;
#   vector[N_race_sex_ints] bju_RACE_SEX;
#   vector[N_race_sex_ints] bju2_RACE_SEX;
#   vector[N_race_sex_ints] bju3_RACE_SEX;
#   matrix[4,4] Rho_RACE_SEX;
#   v_N_race_sex_ints = (diag_pre_multiply(sigma_RACE_SEX,L_Rho_RACE_SEX)*z_N_race_sex_ints)';
#   a_RACE_SEX = col(v_N_race_sex_ints,1);
#   bju_RACE_SEX = col(v_N_race_sex_ints,2);
#   bju2_RACE_SEX = col(v_N_race_sex_ints,3);
#   bju3_RACE_SEX = col(v_N_race_sex_ints,4);
#   Rho_RACE_SEX = L_Rho_RACE_SEX * L_Rho_RACE_SEX';
# }
# model{
#   vector[N] BJU3;
#   vector[N] BJU2;
#   vector[N] BJU;
#   vector[N] A;
#   vector[N] p;
#   bja ~ normal( 0 , 5 );
#   bju3 ~ normal( 0 , 5 );
#   bju2 ~ normal( 0 , 5 );
#   bju ~ normal( 0 , 5 );
#   ba2 ~ normal( 0 , 5 );
#   ba ~ normal( 0 , 5 );
#   sigma_ZIP ~ cauchy( 0 , 2.5 );
#   a_ZIP ~ normal( 0 , sigma_ZIP );
#   a ~ normal( 0 , 5 );
#   sigma_RACE_SEX ~ cauchy( 0 , 2.5 );
#   L_Rho_RACE_SEX ~ lkj_corr_cholesky( 2 );
#   to_vector(z_N_race_sex_ints) ~ normal( 0 , 1 );
#   for ( i in 1:N ) {
#     BJU3[i] = (bju3 + bju3_RACE_SEX[race_sex_ints[i]]);
#   }
#   for ( i in 1:N ) {
#     BJU2[i] = (bju2 + bju2_RACE_SEX[race_sex_ints[i]]);
#   }
#   for ( i in 1:N ) {
#     BJU[i] = (bju + bju_RACE_SEX[race_sex_ints[i]]);
#   }
#   for ( i in 1:N ) {
#     A[i] = a + a_ZIP[Injury_Zip[i]] + a_RACE_SEX[race_sex_ints[i]];
#   }
#   for ( i in 1:N ) {
#     p[i] = A[i] + BJU[i] * jul[i] + BJU2[i] * jul[i]^2 + BJU3[i] * jul[i]^3 + ba * age[i] + ba2 *      age[i]^2 + jul[i] * age[i] * bja;
#   }
#   Fentanyl_tox_screen ~ binomial_logit( 1 , p );
# }
# generated quantities{
#   vector[N] BJU3;
#   vector[N] BJU2;
#   vector[N] BJU;
#   vector[N] A;
#   vector[N] p;
#   real dev;
#   dev = 0;
#   for ( i in 1:N ) {
#     BJU3[i] = (bju3 + bju3_RACE_SEX[race_sex_ints[i]]);
#   }
#   for ( i in 1:N ) {
#     BJU2[i] = (bju2 + bju2_RACE_SEX[race_sex_ints[i]]);
#   }
#   for ( i in 1:N ) {
#     BJU[i] = (bju + bju_RACE_SEX[race_sex_ints[i]]);
#   }
#   for ( i in 1:N ) {
#     A[i] = a + a_ZIP[Injury_Zip[i]] + a_RACE_SEX[race_sex_ints[i]];
#   }
#   for ( i in 1:N ) {
#     p[i] = A[i] + BJU[i] * jul[i] + BJU2[i] * jul[i]^2 + BJU3[i] * jul[i]^3 + ba * age[i] + ba2 *      age[i]^2 + jul[i] * age[i] * bja;
#   }
#   dev = dev + (-2)*binomial_logit_lpmf( Fentanyl_tox_screen | 1 , p );
# }








# **** GENERATE FIGURES


# Extract samples  
post <- extract.samples(fentanyl.od.rate.model)



#### First define function for getting model estimates from samples
estimate <- function(ZIP = NULL, # zip code of death
                     MONTH = NULL, # month of death
                     YEAR = NULL, # year of death
                     JULIANZ = NULL, # z-scored julian date of death if you want to specify directly
                     RACE = NULL, # 1 = White ; 2 = Black ; 3 = Other
                     SEX = NULL, # 1 = Female ; 2 = Male
                     AGE = mean(d$Age), # Unstandardized age (default to sample mean)
                     ZIP_SIMS = NULL) # if zip code is null, simulate zip codes from estimated population sigma
{
  
  # start with intercept
  estimated.OD.rate <- post$a
  
  # Look up zip code index
  if (!is.null(ZIP)){ZIP_CODE_LOOKUP <-  which(zip.key == ZIP)}
  
  if ( !is.null(YEAR) & !is.null(MONTH)){
    jul <- julian(x=MONTH, d=1, y=YEAR)
    JULIANZ <- (jul - mean(d$julian)) / sd(d$julian)
  }
  
  # calculate z-scored age
  ageZ <- ((AGE - mean(d$Age)) / sd(d$Age))
  
  # add age information (will do ~nothing if age isn't specified or is average)
  estimated.OD.rate <- estimated.OD.rate + post$ba * ageZ + post$ba2 * ageZ^2 +  post$bja * ageZ * JULIANZ
  
  
  # if race/sex is specified
  if (!is.null(RACE) & !is.null(SEX)){
    
    # 1 = white female; 2 = black female; 3 = other female; 
    # 4 = white male; 5 = black male; 6 = other male
    race_sex_index <- RACE
    if (SEX == 2){ # if Male
      race_sex_index <- RACE + 3
    }
    
    # Adjust slopes for date by race/sex 
    B1 <- (post$bju + post$bju_RACE_SEX[,race_sex_index]) * (JULIANZ)
    B2 <- (post$bju2 + post$bju2_RACE_SEX[,race_sex_index]) * (JULIANZ )^2
    B3 <- (post$bju3 + post$bju3_RACE_SEX[,race_sex_index]) * (JULIANZ)^3
    
    estimated.OD.rate <- estimated.OD.rate + post$a_RACE_SEX[,race_sex_index] + B1 + B2 + B3
      
  }
  
  if ( !is.null(ZIP)){  # If zip code is specified
    
    # make sure our model has an estimate for it
    if ( ZIP %in% zip.key){
    
        # and vary intercept by it
        estimated.OD.rate <- estimated.OD.rate + post$a_ZIP[,ZIP_CODE_LOOKUP]
        
    }else{
      
      # otherwise simulate variation by zip code
      estimated.OD.rate <- estimated.OD.rate + ZIP_SIMS
      
      }
  }
  

  
  # take the logistic of the estimate to get a rate
  estimated.OD.rate <- logistic(estimated.OD.rate) 
  
  return(estimated.OD.rate)
  
}


# Figure 3
# Plot rates across time for full sample

# z-scored year sequence
year.seq <- seq(-5,5, .25)

# Jitter binary fentanyl data vertically
jittered.fent <- jitter(d$Fentanyl_TOTAL, factor=.3)
# this next step is only for aesthetics, it makes the points line up better with
# the border of the graph
jittered.fent <- ifelse(jittered.fent > 0.5, 
                        jittered.fent[jittered.fent > .5] - (1 - min(jittered.fent[jittered.fent>.5]  )), 
                        jittered.fent[jittered.fent < .5] + max(jittered.fent[jittered.fent<.5]) )

par(oma=c(4,1,1,1))

# plot all the real observations
plot(d$julianZ, jittered.fent,
     xlab="Exact date of death", 
     ylab="Fentanyl tox screens", 
     xaxt="n", yaxt="n", xaxs="i", yaxs="i",
     col=col.alpha("black",.2),
     ylim=c(0,1), pch=19)

# This function gets the first of each year expressed in julian z-scores
get.first.year <- function(YEAR) (julian(x=1, d=1, y=YEAR) - mean(d$julian)) / sd(d$julian)

# Create axes
axis ( side = 1, at = seq(get.first.year(2010),get.first.year(2017), length.out = 8), labels = paste("Jan",seq(2010,2017, 1)))
axis ( side = 2, at = c(.1, .9), labels = c("negative (-)","positive (+)"), tick=FALSE)
axis ( side = 4, at = seq(0, 1, .1), labels = percent(seq(0,1,.1)))


# z-scored julian date sequence 
julianz.seq <- seq(-3.5,2, .1)


# get sample counts by zip code / race / sex
demo.counts <- table(d$zip_code, d$race_sex)


# initialize empty variable to hold estimates by z-scored julian date
julianz.mu <- NULL

# for every julian date in the sequence
for (i in julianz.seq){
  
  # start with the intercept
  est <- post$a 
  
  # post-stratify by zip code
  for (zip in 1:38){
    # and race/sex
    for (race_sex in 1:6){
      
      # get the count for the current demographic subgroup
      demo.size <- demo.counts[zip,race_sex]
      
      # get the model estimate for that subgroup
      demo.est <- post$a_RACE_SEX[,race_sex] +  (post$bju + post$bju_RACE_SEX[,race_sex]) * i + 
        (post$bju2 + post$bju2_RACE_SEX[,race_sex]) * i^2 + 
        (post$bju3 + post$bju3_RACE_SEX[,race_sex]) * i^3 + 
        post$a_ZIP[,zip]
      
      
      # weighted avg. for this estimate by sample (Gelman and Hill, 2007, p. 181)
      est <- est + (demo.est * (demo.size / sum(demo.counts)))
      
    }
    
  }
  
  # take the logistic of the estimate...
  est <- logistic(est)
  
  # add it to our sequence of dates
  julianz.mu <- cbind(julianz.mu, est )
}


# plot 1 thousand randomly selected regression lines to get a sense of likelihdood 
for (i in sample(1:60000, size=1e3)){
  lines(julianz.seq, julianz.mu[i,], col = col.alpha("black", .025))
}

# Draw a vertical line for where fentanyl ODs appear to take off
abline(v=(julian(x=4, d=1, y=2014) - mean(d$julian)) / sd(d$julian), lty=2, col="red")

# plot the mean model estimate
grand.mu <- apply(julianz.mu,2,mean)
lines(julianz.seq,grand.mu, col = "black", lwd=2)





# FIGURE 4
# We want a matrix of graphs arranged in 4 rows (for years) and 6 columns (for race/gender combos)
par(mfrow=c(4,6), mar=c(.75,.75,.75,.75), omi=c(rep(.5,4)))

# Simulate varying race and zip code parameters for when these are left unspecified
n.sims <- nrow(post[[1]])
ZIP_SIMS <- rnorm(n.sims, 0, post$sigma_ZIP)

# loop through years    
for (year.to.plot in c(2011,2013,2015,2017)) {
  for (RACE in 1:3){ # loop through races: white, black, then other
    
    for (SEX in 2:1){ # loop through sexes: male, then female
      
      # These four lines calculate an index for race/gender 
      # according to how these were entered into our particular model  
      race_sex_index <- RACE
      if (SEX == 2){ # if Male
        race_sex_index <- RACE + 3
      }
      
      
      
      # we plot data in 2-year intervals. So for example the first iteration of this loop
      # gets us data subsetted down to people who died in 2010 and 2011 
      years.data <- subset(d, Year %in% c(year.to.plot - 1, year.to.plot) & race_sex == race_sex_index)
      
      
      # if there are any data points in this race/gender/year iteration, plot them
      if (nrow(years.data) > 0){
        
        # jitter the data vertically
        jittered.fent <- jitter(years.data$Fentanyl_TOTAL, factor=.1)
        # this step is only for aesthetics, it makes the points line up better with
        # the border of the graph
        jittered.fent <- ifelse(jittered.fent > 0.5, 
                                jittered.fent[jittered.fent > .5] - (1 - min(jittered.fent[jittered.fent>.5] - .04 )), 
                                jittered.fent[jittered.fent < .5] + max(jittered.fent[jittered.fent<.5]) + .04)
        plot(years.data$Age, # Age on x-axis
             jittered.fent, # Fentanyl ODs (0s and 1s) on y-axis
             pch=19, # plot points as filled circles
             xlim=c(15,80), # observed ages range from 16-77
             ylim=c(0,1),
             yaxt="n", xaxt="n", # Don't generate the axes yet. We'll add those in manually later
             xaxs="i", yaxs="i", # No extra padding around x and y-lims
             col=col.alpha(grey(.15), .2) # make points somewhat transparent
        )
      }
      
      
      # We have no observations for some race-sex-year combinations (viz., other female 2012/2013)
      # and in those cases we just put down an empty plot with otherwise identical specifications
      if (nrow(years.data) == 0){
        plot(50, .5, # arbitrary data-point
             type="n", # type='n' prevents anything from being plotted visually
             # all the other specifications are identical
             xlim=c(15,80), ylim=c(0,1), yaxt="n",xaxt="n",xaxs="i", yaxs="i"
        )
        
      }
      
      # This code is for axes, titles, etc. 
      # It'll be easier to follow what this is doing
      # by running it
      
      # Give the graphs x-axis labels
      if ( year.to.plot == 2017){ 
        # only provide labels at the very bottom of the 6x4 matrix of graphs
        axis ( side = 1, at = seq(20, 80, 10))
      }else{
        # for the other plots, add small tick-marks for reference but leave out labels
        axis ( side = 1, at = seq(20, 80, 10), labels=F, tck=-.02)
      }
      
      # On the far left side of the 6x4 matrix
      if (race_sex_index == 4){
        
        # Add y-axes indicating rate of positive fentanyl toxicology screen
        # with major tick-marks (and labels) at 0, 50, and 100%
        axis ( side = 2, at = seq(0, 1, .5), labels =percent(seq(0,1,.5)), cex.axis=.9, las = 1,  mgp=c(3,.75,0))
        # at 0,10,20...,100% only add minor tick-marks
        axis ( side = 2, at = seq(0, 1, .1), labels =F, tck=-.02)
      }else{
        
        # for the other graphs add the same tick-marks but leave labels out to avoid clutter
        axis ( side = 2, at = seq(0, 1, .5), labels =F)
        axis ( side = 2, at = seq(0, 1, .1), labels =F, tck=-.02)
      }
      
      
      
      # Here we add the model estimates
      
      # initialize a variable to hold model estimates
      age.est <- NULL
      # We're going to get a model estimate for each year from 10-90 years old
      age.seq <- seq(10,90,2)
      
      for (i in age.seq){
        # get a sequence of model estimates for 10-90 year olds at the mid-points of the two years 
        # represented in each graph (e.g., for the 2010/2011 graph, get the model estimate for January 1st, 2011 )
        age.est <- cbind(age.est,estimate(YEAR = year.to.plot , ZIP_SIMS= ZIP_SIMS, MONTH=1, AGE =i, RACE = RACE, SEX = SEX ))
      }
      
      # get the mean of the samples for each age
      grand.mu <- apply(age.est,2,mean)
      # plot the mean as a solid black line
      lines(age.seq,grand.mu, col = "black", lwd=1.5)
      
      
      # plot 95% credible intervals, but fade them beginning at the 90% HPDI
      
      for ( i in seq(.9,.95,.01)){
        grand.PI <- apply(age.est,2,function(x) HPDI(x, prob=i))
        
        shade(grand.PI, age.seq, col=col.alpha('black', .03 ))
        # 
      }
      
    }
    
  }
}






# FIGURE 5
# A map

# download polygon information to map zip code outlines
# http://data.indy.gov/datasets/bba987ced0cf4b7086650e3656b30d69_7.geojson
zips <- geojsonio::geojson_read("http://data.indy.gov/datasets/bba987ced0cf4b7086650e3656b30d69_7.geojson",
                                what = "sp")

# create a quick function that takes zip codes from Marion county and gets fentanyl OD rates
zip.adj.year <- function(ZIP) mean(estimate(ZIP, ZIP_SIMS=ZIP_SIMS, YEAR=mean(d$Year), MONTH=7))


# Construct estimate for Fentanyl-related lethal overdose rates for each zip code
# note that a couple zip codes will have to be simulated. Essentially they'll end up looking "average"
zips$ZIPCODEestimate <- sapply(zips$ZIPCODE, zip.adj.year)
# Subtract the mean from each estimate so we get a sense of which zip codes have higher
# or lower rates than the average
zips$ZIPCODEestimate <- (zips$ZIPCODEestimate - mean(zips$ZIPCODEestimate, na.rm=T))

# Define a grayscale color palette
pal = colorNumeric(colorRamp(c(grey(.8),grey(0))), zips$ZIPCODEestimate, na.color = col.alpha("white",0))

# Create a legend for the color palette
pal.legend = colorNumeric(colorRamp(c(grey(.8),grey(0))), zips$ZIPCODEestimate)

# Use the leaflet htmlwidget to create a map
(m <- leaflet(zips, options = leafletOptions(zoomControl = FALSE)) %>% 
    addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
    setView(lng = -86.15, lat = 39.78, zoom = 10) %>% # set center and extent of map
    addPolygons(stroke = TRUE, weight=1, fillOpacity = .5, color= ~pal(ZIPCODEestimate)) %>%
    addLegend("bottomright",pal=pal.legend, values=zips$ZIPCODEestimate, labFormat = labelFormat(suffix="%", transform=function(x) 100 * x))
)









