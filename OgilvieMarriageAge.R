setwd('C:/Users/Nathan/Downloads/Twitter Pictures/DennisonOgilvieMap')

library(data.table)
library(rgdal)
library(spdplyr)
library(magrittr)
library(dplyr)
library(pdftools)
library(rworldmap)
library(eurostat)
library(foreach)
library(viridis)
library(cowplot)
library(ggplot2)
theme_set(theme_cowplot())
rbind_and_fill=function(...) rbind(...,fill=T)


load('NUTS_2013_01M_SH_SPDF.RData')
NUTS_shapefiles = NUTS_2013_01M_SH_SPDF

pdftext = pdftools::pdf_text('Dennison-Ogilvie-2014.pdf')
pdftext[11] %>%
  strsplit('\\r\\n') %>%
  as.data.table() -> marriage_dt
marriage_dt = marriage_dt[7:nrow(marriage_dt)]
setnames(marriage_dt,1,'bigstring')
marriage_dt[,c('country','coef','stderr') := tstrsplit(bigstring, "  [ ]*",perl=T)][,bigstring := NULL]
marriage_dt[,country := gsub(' (all)','',country,fixed = T)]
marriage_dt = rbind(marriage_dt,data.table(country = c('Estonia','Lithuania','Latvia'),coef = marriage_dt[country == 'Baltics',coef]),fill=T)
marriage_dt[country == 'Bohemia',country := 'Czech Republic']
marriage_dt[,coef := gsub(substr(marriage_dt$coef[42],1,1),'-',coef)]
marriage_dt[,coef := as.numeric(coef)]

geo_dt = data.table(map_data('world'))
geo_dt[marriage_dt,on = c(region = 'country'),`:=`(coef=i.coef,country=i.country)]
marriage_dt[data.table(country=unique(geo_dt$country),merged2 = 1),on='country',merged2 := i.merged2]
geo_dt[subregion == 'Northern Ireland',coef := marriage_dt[country == 'Ireland',coef]]
geo_dt[subregion == 'Scotland',coef := marriage_dt[country == 'Scotland',coef]]
geo_dt[subregion == 'Wales',coef := 0]
geo_dt[subregion == 'Isle of Wight',coef := 0]



maxgroup = max(geo_dt$group)
scotland = foreach(iter = seq_along(NUTS_shapefiles@polygons[[which(NUTS_shapefiles$NUTS_ID == 'UKM')]]@Polygons),.combine=rbind_and_fill)%do%{
  tryCatch({test2 = NUTS_shapefiles@polygons[[which(NUTS_shapefiles$NUTS_ID == 'UKM')]]@Polygons[[iter]]@coords
  out <<- data.table(long=test2[,1],lat=test2[,2],group=maxgroup+iter+1,region='Scotland')
  },error = function(e) out <<- data.table())
  out
}

geo_dt = rbind(geo_dt,scotland,fill=T)
geo_dt[region == 'Scotland',coef := marriage_dt[country == 'Scotland',coef]]

englandandwalesNUTS = c('UKC','UKD','UKE','UKF','UKG','UKH','UKI','UKJ','UKK','UKL')
maxgroup = max(geo_dt$group)
englandandwales = foreach(regnum = seq_along(englandandwalesNUTS),.combine=rbind_and_fill)%do%{
  reg = englandandwalesNUTS[regnum]
  foreach(iter = seq_along(NUTS_shapefiles@polygons[[which(NUTS_shapefiles$NUTS_ID == reg)]]@Polygons),.combine=rbind_and_fill)%do%{
    tryCatch({test2 = NUTS_shapefiles@polygons[[which(NUTS_shapefiles$NUTS_ID == reg)]]@Polygons[[iter]]@coords
    out <<- data.table(long=test2[,1],lat=test2[,2],group=maxgroup+iter+1+regnum*10000,region='England')
    },error = function(e) out <<- data.table())
    out
  }
}


geo_dt = rbind(geo_dt,englandandwales,fill=T)
geo_dt[region == 'England',coef := 0]



iterateover = rbind(data.table(c('FR1','FR3','FR4','FR21','FR22','FR23','FR25'),'France (northern)'),
data.table(c('FR5','FR63','FR72','FR24','FR26'),'France (central)'),
data.table(c('FR8','FR61','FR62','FR71'),'France (southern)'),
data.table(c('ES6','ES53'),'Spain (southern)'),
data.table(c('ES3','ES42','ES43','ES52'),'Spain (central)'),
data.table(c('ES1','ES2','ES41','ES51'),'Spain (northern)'),
data.table(c('ITC','ITH','ITI'),'Italy (northern)'),
data.table(c('ITF','ITG'),'Italy (southern)'))
setnames(iterateover,c('NUTS','region'))

maxgroup = max(geo_dt$group)
FranceSpainItaly = foreach(regnum = seq_along(iterateover$NUTS),.combine=rbind_and_fill)%do%{
  reg = iterateover$NUTS[regnum]
  foreach(iter = seq_along(NUTS_shapefiles@polygons[[which(NUTS_shapefiles$NUTS_ID == reg)]]@Polygons),.combine=rbind_and_fill)%do%{
    tryCatch({test2 = NUTS_shapefiles@polygons[[which(NUTS_shapefiles$NUTS_ID == reg)]]@Polygons[[iter]]@coords
    out <<- data.table(long=test2[,1],lat=test2[,2],group=maxgroup+iter+1+regnum*10000,region=iterateover$region[regnum])
    },error = function(e) out <<- data.table())
    out
  }
}


geo_dt = rbind(geo_dt,FranceSpainItaly,fill=T)




maxgroup = max(geo_dt$group)
norway = foreach(iter = seq_along(NUTS_shapefiles@polygons[[which(NUTS_shapefiles$NUTS_ID == 'NO0')]]@Polygons),.combine=rbind_and_fill)%do%{
  tryCatch({test2 = NUTS_shapefiles@polygons[[which(NUTS_shapefiles$NUTS_ID == 'NO0')]]@Polygons[[iter]]@coords
  out <<- data.table(long=test2[,1],lat=test2[,2],group=maxgroup+iter+1,region='Norway')
  },error = function(e) out <<- data.table())
  out
}

setkey(geo_dt, region)
geo_dt = geo_dt[!'Norway']
geo_dt = rbind(geo_dt,norway,fill=T)
geo_dt[region == 'Norway',coef := marriage_dt[country == 'Norway',coef]]

crosswalk = rbind(data.table(efta_countries),data.table(eu_candidate_countries),setnames(data.table(eu_countries),names(efta_countries)))

setnames(crosswalk,c('NUTS','country','label'))
crosswalk[country == 'Czechia',country := 'Czech Republic']
crosswalk[country == "The former Yugoslav Republic of Macedonia",`:=`(country='Macedonia',NUTS='MK')]
crosswalk = crosswalk[!country%in%anti_join(data.table(country=crosswalk$country,NUTS=crosswalk$NUTS),data.table(NUTS=unique(substr(NUTS_shapefiles$NUTS_ID,1,2))),'NUTS')$country]
crosswalk = crosswalk[NUTS!='UK']
crosswalk = crosswalk[country!='Poland'] #the border with Kaliningrad looked bad
crosswalk = crosswalk[!country%in%c('France','Spain','Italy')]


maxgroup = max(geo_dt$group)
EUcountries = foreach(regnum = seq_along(crosswalk$NUTS),.combine=rbind_and_fill)%do%{
  reg = crosswalk$NUTS[regnum]
  foreach(iter = seq_along(NUTS_shapefiles@polygons[[which(NUTS_shapefiles$NUTS_ID == reg)]]@Polygons),.combine=rbind_and_fill)%do%{
    tryCatch({test2 = NUTS_shapefiles@polygons[[which(NUTS_shapefiles$NUTS_ID == reg)]]@Polygons[[iter]]@coords
    out <<- data.table(long=test2[,1],lat=test2[,2],group=maxgroup+iter+1+regnum*10000,region=crosswalk$country[regnum])
    },error = function(e) out <<- data.table())
    out
  }
}

geo_dt = geo_dt[!region%in%crosswalk$country]
geo_dt = rbind(geo_dt,EUcountries,fill=T)

geo_dt = geo_dt[!region%in%c('France','Spain','Italy')]


geo_dt[marriage_dt,on=c(region='country'),coef := i.coef]


geo_dt[country=='Russia'&long<23.5&lat>53&lat<67,coef:=NA_real_] #Kaliningrad
geo_dt[(long < -12 &lat < 33.4)|(long < -23 & lat < 40.5),coef:=NA_real_] #Azores
geo_dt = geo_dt[is.na(subregion) | subregion != 'Great Britain']


#48*-2.65 + 257*-.9 + 970*-.33 + 1347*0 + 274 + 500 + 1476 + 372
cons = 25.26 + (1/3)*-.9 + (1/3)*-.33 + (1/3)*0 +
  (274*0 + 500*.14 + 1476*.43 + 372*.55)/(274 + 500 + 1476 + 372) +
  (233*0 + 337*-.52 + 1499*.05 + 553*.49)/(233 + 337 + 1499 + 553) +
  (652*-.08 + 1145*0 + 53*.17 + 132*-.04 + 640*-.12)/(652 + 1145 + 53 + 132 + 640) +
  (1239*0 + 510*.02 + 296*-.02 + 577*.32)/(1239 + 510 + 296 + 577)
geo_dt[,est_marriage_age:=cons+coef]

plot = ggplot(geo_dt, aes( x = long, y = lat, group = group )) +
  geom_polygon(aes(fill = est_marriage_age)) +
  xlim(-75,75) +
  ylim(-5,90) +
  scale_fill_viridis(option = "magma", direction = 1,'Average Age\nof Women\'s\nFirst Marriage\n(1600-1900)')  +
  coord_fixed()
plot(plot)
ggsave('OgilvieMarriageAge.png', plot, width = 1.25*30,height=1.25*25.22,limitsize = F)