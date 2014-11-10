dd <- '/data/ref/global/cru'
cms <- read.csv(file.path(dd,'crutem-4.2-station-metadata.csv'),header=TRUE)
dim(cms)

head(cms)

## wmoid: Met Station ID
## name:  Met Station Name
## co:    Country Code
## lat:   Latitude (Decimal Degrees)
## lon:   Longitude (Decimal Degrees)
## z:     Elevation (m)
## y0:    Temperature Record Start Year
## y1:    Temperature Record End Year
## fgy:   "First Good Year" (not always coincident with y0)
## sid:   Source ID

sapply(cms, class)

levels(cms$co) ## 242

## target countries
tcs <- c('ARGENTINA','AUSTRALIA','CHILE','NEW ZEALAND','URUGUAY')

## "Southern Hemisphere Met Station Extraction"
sex <- subset(cms, cms$co %in% tcs)
sex$co <- factor(sex$co)
dim(sex)

sex[478,] ## Clare Post Office should be SH
sex[478,'lat'] <- -1 * sex[478,'lat'] 

head(sex, 20)
plot(sex$lon, sex$lat)

## binary matrix for station operation years
y00 <- min(sex$fgy)
y11 <- max(sex$y1)
ys  <- t(sapply(seq(nrow(sex)), function (i) {
    zs <- rep(0,y11-y00+1)
    zs[(sex$fgy[i]-y00+1):(sex$y1[i]-y00+1)] <- 1
    zs
} ))
rownames(ys) <- sex$wmoid

tc <- apply(ys,2,sum)
plot(tc ~ I(y00:y11), type='l',
     main='Selected SH Country CRUTEM-4.2 Station Count by Year',
     ylab='Station Count', xlab='Year')
abline(v=seq(1860,2010,by=10),lty=3,col='gray')

## counts of operational stations by year and country
cs <- lapply(tcs, function (c) {
    apply(ys[sex$co==c,],2,sum)
})
names(cs) <- tcs

## cs expressed as proportions of country mamxima
csp <- sapply(cs, function (c) {
    round(c/max(c),3)
})



pdf('ProportionalCRUTEMStationCountBySelectedSHCountries.pdf')
xs <- I(y00:y11)
plot(I(tc/max(tc)) ~ xs, type='l', lwd=2,
     main='Proportional CRUTEM Station Count',
     ylab='Proportion', xlab='Year')
abline(v=seq(1860,2010,by=10),lty=3,col='gray')
for (i in seq(ncol(csp)))
    lines(csp[,i] ~ xs, col=i, lty=2)
lab <- paste(tcs,' (',lapply(cs, max),')',sep='')
legend(1850,1,lab,title='Country (Max #)',lty=2,col=seq(5))
dev.off()

## ----- eof ------------------------------------------------------------------
