####========================================================
# Merapikan Data Kelelawar Sulawesi dari Database Daring
####========================================================

rm(list=ls())
getwd()
#setwd()

wranglingPackages<-c("dplyr", "forcats", "stringr", "rgbif")
for(lib in wranglingPackages[!wranglingPackages %in% installed.packages()]) 
  {install.packages(lib,dependencies=TRUE)}
sapply(wranglingPackages,require,character=TRUE)

# Wrangling species occurrence data downloaded from VertNet-------------------------------
sulawesi_bat <- read.delim("ChiropteraSpecimenSulawesi.txt",header=TRUE)

# See the data
glimpse(sulawesi_bat) # Lots of columns with NAs and empty data

# Wrangling the entire on with the entire record---------------------------------------
# First, we remove columns that their entire values are NA or empty spaces or 0s
sulbat<-sulawesi_bat[!sapply(sulawesi_bat, function (x) all(is.na(x) | x == "" | x == " " | x==0))]
glimpse(sulbat) # Lots of columns with NAs
nrow(sulbat) # 3397 records

# As we want to map species richness, we need...
# ...the locality and any geographic information
# ...and complete species name.

# Remove data without localities
sulbat<-sulbat %>% filter(locality!="")
sulbat$locality<-factor(sulbat$locality)
levels(sulbat$locality) # 176 locality

# Then, we generalize the remaining localities, repairing typos, etc
# First, save the real localities and the number of species there
originallocality<-sulbat %>% select(scientificname) %>% group_by(sulbat$locality) %>% summarise(n=n())
# making all localities lower case for convenience
# change first to character as cases in factor class cannot be changed
sulbat<-mutate(sulbat,
               county=as.character(county),
               locality=as.character(locality),
               verbatimlocality=as.character(verbatimlocality)
               )
sulbat<-mutate(sulbat,
               county=tolower(county),
               locality=tolower(locality),
               verbatimlocality=tolower(verbatimlocality)
               )
sulbat<-mutate(sulbat,
               county=as.factor(county),
               locality=as.factor(locality),
               verbatimlocality=as.factor(verbatimlocality)
)
levels(sulbat$locality) # reduces the localities into 169
levels(sulbat$county)
levels(sulbat$verbatimlocality)
originallocality<-sulbat %>% select(scientificname) %>% group_by(sulbat$locality) %>% summarise(n=n())

# Repairing typos and redundant locality names...--------------------------
# ...by checking in Google Earth, http://carikodepos.co/, and ancient museum accounts (see remarks on location)
# But we put them on a new column called correctedlocality

sulbat <-  mutate(sulbat, 
                  correctedlocality = fct_recode(locality
                                                 ,"tomado" = "tomado, 1000 m"
                                                 ,"tolitoli" = "toli toli [= tolitoli]"
                                                 ,"temboan" = "temboan, sw from tondano lake"
                                                 ,"pangkajene district; padang lampe" = "pangkajene district; padnag lampe"
                                                 ,"pangkajene district; padang lampe" = "pankajene district; padang lampe"
                                                 ,"pandere" = "panderc"
                                                 ,"talassa (maros)" = "maros talassa"
                                                 ,"lake lindu" = "lindu lake"
                                                 ,"kecamatan: pagimana; desa siuna, kampung siuna" = "kecamatan: bagimana; desa siuna, kampung siuna"
                                                 ,"kecamatan: pagimana; desa siuna, kampung siuna" = "kecamatan: bagrimana; desa siuna, kampung suing"
                                                 ,"hanggira" = "hanngira"
                                                 ,"desa: siuna; 5km east of siuna" = "desa (township) siuna; 5km e siuna"
                                                 ,"desa: siuna; 5km east of siuna" = "desa: suina; 5km e of suina"
                                                 ,"maros borong" = "maros borony"
                                                 ,"bonto sunggu" = "bonto sunngi"
                                                 ,"bonto sunggu" = "bonto sunngu"
                                                 ,"bariri" = "barini"
                                                 ,"gumbasa proj.,gumbasa" = "gumbasa proj.,gumbasa."
                                                 ,"gumbasa proj.,gumbasa" = "gumbasa proj.;gumbasa"
                                                 ,"gumbasa proj.,kalawara" = "gumbasa proj.,kalawara."
                                                 ,"gumbasa proj.,kalawara" = "gumbasa proj.;kalawara"
                                                 ,"gumbasa proj.,omu." = "gumbasa proj.;omu"
                                                 ,"gumbasa proj.,pandere" = "gumbasa proj.,pandere."
                                                 ,"gumbasa proj.,pandere" = "gumbasa proj.;pandere"
                                                 ,"gumbasa proj.,pewanu" = "gumbasa proj.,pewanua."
                                                 ,"gumbasa proj.,pewanu" = "gumbasa proj.;pewanua"
                          )
)

levels(sulbat$correctedlocality) #146 real localities
glimpse(sulbat)

## Remarks on location------------------------------------------------------

# There are 2 toli toli, one in central and one in southeast
sulbat %>% select(verbatimlocality,county,stateprovince,locationaccordingto,locationremarks) %>% filter(sulbat$locality=="toli toli")
sulbat %>% select(verbatimlocality,county,stateprovince,locationaccordingto,locationremarks) %>% filter(sulbat$locality=="toli toli [= tolitoli]")
# "toli toli [= tolitoli]" is in Central, the other one is uncertain

# panderc is a typo of pandere
sulbat %>% select(verbatimlocality,county,stateprovince,locationaccordingto,locationremarks) %>% filter(sulbat$locality=="panderc")

# maros talassa and talassa (maros) are the same area
sulbat %>% select(decimallongitude, decimallatitude, verbatimlocality,county,stateprovince,locationaccordingto,locationremarks) %>% filter(sulbat$locality=="talassa (maros)")
sulbat %>% select(decimallongitude, decimallatitude, verbatimlocality,county,stateprovince,locationaccordingto,locationremarks) %>% filter(sulbat$locality=="maros talassa")

# there are typos on rantekarua, but these places has coordinates, so we let them be
sulbat %>% select(recordedby, decimallatitude, verbatimlocality,county,stateprovince,locationaccordingto,locationremarks) %>% filter(sulbat$locality=="kecamatan:rindingallo;desa:awan;dusun:rantekarva")

# bagrimana and bagimana are not in the carikodepos
# the nearest one is Pagimana in Central Sulawesi
sulbat %>% select(decimallongitude, decimallatitude, verbatimlocality,county,stateprovince,locationaccordingto,locationremarks) %>% filter(sulbat$locality=="kecamatan: bagrimana; desa siuna, kampung suing")
# ^ this has verbatimlocality on tana toraja, south sulawesi
# but tana toraja does not have desa siuna
sulbat %>% select(decimallongitude, decimallatitude, verbatimlocality,county,stateprovince,locationaccordingto,locationremarks) %>% filter(sulbat$locality=="kecamatan: bagimana; desa siuna, kampung siuna")
# ^ this has verbatimlocality on Banggai, Central Sulawesi
# so I correct both into pagimana

# barini is not on carikodepos but bariri is
sulbat %>% select(decimallongitude, decimallatitude, verbatimlocality,county,stateprovince,locationaccordingto,locationremarks) %>% filter(sulbat$locality=="bariri")

# pewanua is not in gumbasa but in manado so i change it into pewanu, which is near gumbasa
sulbat %>% select(dataset_citation, verbatimlocality,county,stateprovince,locationaccordingto,locationremarks) %>% filter(sulbat$locality=="gumbasa proj.;pewanua")

# Repairing coordinates for localities with NAs in the coordinates----------
# Checking any NAs in locality, specificepithet, scientificname, and coordinates
NAcoords<-sulbat %>% filter(is.na(sulbat$decimallongitude)) %>% group_by(correctedlocality) %>% summarise(n=n())
NAcoords$correctedlocality<-factor(NAcoords$correctedlocality)
levels(NAcoords$correctedlocality) # there are 112 localities with NAs

# code for checking locality coordinates
sulbat %>% select(decimallatitude,
                  decimallongitude,
                  verbatimlocality,
                  county,
                  stateprovince,
                  locationaccordingto,
                  locationremarks) %>% filter(sulbat$locality=="utara")
levels(NAcoords$correctedlocality)

# Search geocode from here: http://www.latlong.net/convert-address-to-lat-long.html
sulbat <- sulbat %>%
  mutate(decimallongitude = case_when(.$locality == "kecamatan: rindingallo; desa awan, dusun: rantekarua; tana toraja." ~ 119.6973
                                      ,TRUE ~ .$decimallongitude
  )
  ,decimallatitude = case_when(.$locality == "kecamatan: rindingallo; desa awan, dusun: rantekarua; tana toraja." ~ -2.9022 )
  )
                               
sulbat <- sulbat %>%
  mutate(decimallongitude = case_when(.$locality == "bantimoeroeng" ~ 119.684681
                                      ,.$correctedlocality == "bariri" ~ 120.412716
                                      ,.$correctedlocality == "above lake tonado" ~ 124.895519
                                      ,.$correctedlocality == "bonto sunggu" ~ 119.435344
                                      ,.$correctedlocality == "buka island, tomini gulf, sulawesi" ~ 121.762287
                                      ,.$correctedlocality == "donggala, lake lindu" ~ 119.83523
                                      ,.$correctedlocality == "galang; malangga selatan; mt. dako" ~ 120.941737
                                      ,.$correctedlocality == "gumbasa" ~ 119.997177
                                      ,.$correctedlocality == "gunung balease base camp, kecamatan sukamaju, kabupaten luwu utara, sulawesi" ~ 120.5425
                                      ,.$correctedlocality == "kalimbaung" ~ 119.960305
                                      ,.$correctedlocality == "kecamatan: bittuang; desa: tiroan; dusun: tiroan, kampung: rinbun; tana toraja." ~ 119.6838166667
                                      ,.$correctedlocality == "kecamatan: bittuang;desa:tiroan;dusun:bolokan; tana toraja." ~ 119.6979
                                      ,.$correctedlocality == "kecamatan: rindingallo; desa awan, dusun: rantekarua, kampung to' ranni; tana toraja." ~ 119.7123
                                      ,.$correctedlocality == "kecamatan: rindingallo; desa awan, dusun: rantekarua; cave on maulu river; tana toraja." ~ 119.7354
                                      ,.$correctedlocality == "kecamatan: rindingallo; desa: awan; dusun: rantekarua; tana toraja." ~ 119.723
                                      ,.$correctedlocality == "kecamatan: rindingallo; desa:awan, dusun: rantekarua; kampung: to' ranni'; tana toraja." ~ 119.7123
                                      ,.$correctedlocality == "likoepang" ~ 125.055298
                                      ,.$correctedlocality == "loka" ~ 119.907274
                                      ,.$correctedlocality == "maros district" ~ 119.696268
                                      ,.$correctedlocality == "maros district; forest in karaenta" ~  119.79521694
                                      ,.$correctedlocality == "mato angin" ~ 119.407049
                                      ,.$correctedlocality == "near bitung" ~ 125.148879
                                      ,.$correctedlocality == "padang lampe" ~ 119.609346
                                      ,.$correctedlocality == "palopo, soroako" ~ 121.357
                                      ,.$correctedlocality == "pangkajene district; padang lampe" ~ 119.609346
                                      ,.$correctedlocality == "salua" ~ 119.904659
                                      ,.$correctedlocality == "sigi" ~ 119.88152
                                      ,.$correctedlocality == "soemalata" ~ 122.492009
                                      ,.$correctedlocality == "tapanjen" ~ 119.983333
                                      ,.$correctedlocality == "toli toli" ~ 120.798203
                                      ,.$locality == "bumbulan" ~ 122.092266
                                      ,.$correctedlocality == "desa: siuna; 5km east of siuna" ~ 123.011357
                                      ,.$locality == "doluduo" ~ 123.9527
                                      ,.$locality == "g. niotokan" ~ 124.44478
                                      ,.$locality == "gimpoe [= gimpu]" ~ 120.124285
                                      ,.$locality == "gumbasa forest" ~ 119.997177
                                      ,.$locality == "gumbasa forest, 1 12's, 119 57e" ~ 119.9500
                                      ,.$locality == "gumbasa proj.,bobo(palolo)." ~ 120.135835
                                      ,.$locality == "gumbasa proj.,bunga." ~ 120.026076
                                      ,.$correctedlocality == "gumbasa proj.,gumbasa" ~ 119.997177
                                      ,.$correctedlocality == "gumbasa proj.,kalawara" ~ 119.968272
                                      ,.$locality == "gumbasa proj.,lonja" ~ 119.952106
                                      ,.$correctedlocality == "gumbasa proj.,omu." ~ 120.008737
                                      ,.$locality == "gumbasa proj.,pakuli." ~ 120.008737
                                      ,.$correctedlocality == "gumbasa proj.,pandere" ~ 119.96249
                                      ,.$locality == "gumbasa proj.,petimbe." ~ 120.04919
                                      ,.$correctedlocality == "gumbasa proj.,pewanu" ~ 119.826549
                                      ,.$locality == "gumbasa proj.,sibowi." ~ 119.945084
                                      ,.$locality == "gumbasa proj.,tatura" ~ 119.880074
                                      ,.$locality == "gunung kanino" ~ 120.1333
                                      ,.$locality == "gunung nokilalaki" ~ 120.1333
                                      ,.$correctedlocality == "hanggira" ~ 120.193566
                                      ,.$locality == "kalabat" ~ 125.030833
                                      ,.$locality == "kalamanta" ~ 119.904659
                                      ,.$locality == "kalawara" ~ 119.93
                                      ,.$locality == "kanawu" ~ 120.1138
                                      ,.$locality == "kantewu" ~ 119.89309
                                      ,.$locality == "karowa" ~ 124.497441
                                      ,.$locality == "katu" ~ 120.193566
                                      ,.$locality == "kecamatan ulubongka; marowo" ~ 121.50274
                                      ,.$correctedlocality == "kecamatan: pagimana; desa siuna, kampung siuna" ~ 123.011357
                                      ,.$locality == "kecamatan: bittuang; desa: tiroan; dusun: bolokan; tana toraja; 2*56.145's, 119*41.872'e" ~ 119.6979
                                      ,.$locality == "kecamatan: rindingallo; desa awan, dusun: rantekarua; tana toraja; 2* 54.130's, 119* 41.839'e" ~ 119.6973
                                      ,.$locality == "kecamatan: rindingallo; desa awan, dusun: rantekarua; tana toraja; 2* 54.130's, 119*41.841'e" ~ 119.6974
                                      ,.$locality == "kecamatan: rindingallo; desa:awan, dusun: rantekarua; kampung: to' ranni'; tana toraja; 2* 55.871's, 119*  42.737'e" ~ 119.7123
                                      ,.$locality == "kecamatan: ulubongka: marowo" ~ 121.50274
                                      ,.$locality == "kecamatan:bittuang;desa tiroan;dusun:bolokan" ~ 119.7204
                                      ,.$locality == "kecamatan:bittuang;desa tiroan;dusun:ranteharua" ~ 119.723
                                      ,.$locality == "kecamatan:rindingallo;desa:awan;dusun:rantekarua c" ~ 119.723
                                      ,.$locality == "kecamatan:rindingallo;desa:awan;dusun:rantekarua k" ~ 119.723
                                      ,.$locality == "kecamatan:rindingallo;desa:awan;dusun:rantekarva" ~ 119.723
                                      ,.$locality == "kecamatan:tompo bulu; cikoro: desa; dusun: lembang bune;kampung: parang bintolo" ~ 119.898875
                                      ,.$locality == "kecamatang:bittuany;desa:teroan;dusun:tiroan;kampu" ~ 119.702061
                                      ,.$locality == "koelawi [= kulawi]" ~ 119.9833
                                      ,.$locality == "koemersot" ~ 125.066346
                                      ,.$locality == "kosio" ~ 124.017242
                                      ,.$locality == "kuala navusu" ~ 120.45
                                      ,.$locality == "lake ilalai" ~ 124.408009
                                      ,.$locality == "lake iloloi" ~ 124.408009
                                      ,.$correctedlocality == "lake lindu" ~ 120.0500
                                      ,.$locality == "lalolis" ~ 122.0500
                                      ,.$locality == "lambara" ~ 119.872842
                                      ,.$locality == "lambasang" ~ 119.851452
                                      ,.$locality == "lampesue" ~ 121.5327
                                      ,.$locality == "lampobatang" ~ 119.931667
                                      ,.$locality == "langko" ~ 120.5279
                                      ,.$locality == "latimodjong geb." ~ 120.073889
                                      ,.$locality == "lembeh" ~  125.237343
                                      ,.$locality == "likoepang [= likupang]" ~ 125.055298
                                      ,.$locality == "lombasang" ~ 119.851452
                                      ,.$locality == "lonka" ~ 120.303187
                                      ,.$locality == "luwu, tohalosi" ~ 120.251273
                                      ,.$locality == "luwu, wawondula" ~ 120.251273
                                      ,.$locality == "majane island" ~ 119.432731
                                      ,.$locality == "malenge" ~ 122.043969
                                      ,.$locality == "malakasa" ~ 120.45
                                      ,.$locality == "mamu" ~ 119.997177
                                      ,.$locality == "mengkoka mts." ~ 121.297778
                                      ,.$county == "menkoka mts." ~ 121.297778
                                      ,.$county == "mengkoka mts." ~ 121.297778
                                      ,.$locality == "mopuya" ~ 123.991722
                                      ,.$locality == "near lindu lake" ~ 120.0500
                                      ,.$locality == "oena - oena" ~ 121.605489
                                      ,.$locality == "paku ure" ~ 124.430916
                                      ,.$locality == "pakuli" ~ 119.9333
                                      ,.$locality == "palmas island [= pulau miangas], 75 mi se of mindanao, philippines" ~ 126.585095
                                      ,.$correctedlocality == "pandere" ~ 119.95 
                                      ,.$locality == "parahaleang" ~ 119.806291
                                      ,.$locality == "peleng island [= pulau peling], near liang" ~ 123.225088
                                      ,.$locality == "pinedapa" ~ 120.583300
                                      ,.$locality == "roeroekan" ~ 124.8500
                                      ,.$locality == "rompo" ~ 120.401191
                                      ,.$locality == "sadaunta" ~ 119.966700
                                      ,.$locality == "sedoa" ~ 120.29742
                                      ,.$locality == "singkalong" ~ 119.683333
                                      ,.$locality == "soroako" ~ 121.388467
                                      ,.$locality == "sungai miu" ~ 119.966700
                                      ,.$locality == "sungai oha kecil" ~ 119.950000
                                      ,.$locality == "sungai paleleh" ~ 121.816700
                                      ,.$locality == "sungai sadaunta" ~ 119.966700
                                      ,.$locality == "sungai tokararu" ~ 120.116700
                                      ,.$locality == "sungai tolewonu" ~ 120.450000
                                      ,.$correctedlocality == "talassa (maros)" ~ 119.7333
                                      ,.$locality == "tamadue" ~ 120.42424
                                      ,.$locality == "tanke salokko" ~ 121.2500
                                      ,.$correctedlocality == "temboan" ~ 124.912066
                                      ,.$locality == "tenga" ~ 124.479988
                                      ,.$locality == "tj.pulisan" ~ 125.165556
                                      ,.$locality == "toeare [= tuare], bada" ~ 120.147383
                                      ,.$locality == "tolai" ~ 120.308954
                                      ,.$correctedlocality == "tolitoli" ~ 120.757983
                                      ,.$correctedlocality == "tomado" ~ 120.041397
                                      ,.$locality == "ulubongka county, marowo" ~ 121.50274
                                      ,.$county == "pulisan" ~ 125.154378
                                      ,.$county == "lembeh island" ~ 125.225837
                                      ,.$county == "tenga" ~ 124.488212
                                      ,.$locality == "wasuponda" ~ 120.941737
                                      ,.$locality == "wawondula" ~ 121.36973
                                      ,.$locality == "wori" ~ 124.856898
                                      ,.$locality == "wowondula" ~ 121.36973
                                      ,.$correctedlocality == "maros borong" ~ 119.461458
                                      ,TRUE ~ .$decimallongitude
  )
  ,decimallatitude = case_when(.$locality == "bantimoeroeng" ~ -4.972899
                                ,.$correctedlocality == "bariri" ~ -1.731652
                               ,.$correctedlocality == "above lake tonado" ~ 1.223679
                               ,.$correctedlocality == "bonto sunggu" ~ -5.257028
                               ,.$correctedlocality == "buka island, tomini gulf, sulawesi" ~ -0.743690
                               ,.$correctedlocality == "donggala, lake lindu" ~ -0.423316
                               ,.$correctedlocality == "galang; malangga selatan; mt. dako" ~ 1.138760
                               ,.$correctedlocality == "gumbasa" ~ -1.266930
                               ,.$correctedlocality == "gunung balease base camp, kecamatan sukamaju, kabupaten luwu utara, sulawesi" ~ -2.405833
                               ,.$correctedlocality == "kalimbaung" ~ -5.542271
                               ,.$correctedlocality == "kecamatan: bittuang; desa: tiroan; dusun: tiroan, kampung: rinbun; tana toraja." ~ -2.9675
                               ,.$correctedlocality == "kecamatan: bittuang;desa:tiroan;dusun:bolokan; tana toraja." ~ -2.9358
                               ,.$correctedlocality == "kecamatan: rindingallo; desa awan, dusun: rantekarua, kampung to' ranni; tana toraja." ~ -2.9312
                               ,.$correctedlocality == "kecamatan: rindingallo; desa awan, dusun: rantekarua; cave on maulu river; tana toraja." ~ -2.9111
                               ,.$correctedlocality == "kecamatan: rindingallo; desa: awan; dusun: rantekarua; tana toraja." ~ -2.9241
                               ,.$correctedlocality == "kecamatan: rindingallo; desa:awan, dusun: rantekarua; kampung: to' ranni'; tana toraja." ~ -2.9312
                               ,.$correctedlocality == "likoepang" ~ 1.672033
                               ,.$correctedlocality == "loka" ~ -5.454335
                               ,.$correctedlocality == "maros district" ~ -5.054914
                               ,.$correctedlocality == "maros district; forest in karaenta" ~ -5.17318380
                               ,.$correctedlocality == "mato angin" ~ -5.162086
                               ,.$correctedlocality == "near bitung" ~ 1.482034
                               ,.$correctedlocality == "padang lampe" ~ -4.724904
                               ,.$correctedlocality == "palopo, soroako" ~ -2.5213
                               ,.$correctedlocality == "pangkajene district; padang lampe" ~ -4.724904
                               ,.$correctedlocality == "salua" ~ -1.402494
                               ,.$correctedlocality == "sigi" ~ -1.385990
                               ,.$correctedlocality == "soemalata" ~ 0.925265
                               ,.$correctedlocality == "tapanjen" ~ -5.533333
                               ,.$correctedlocality == "toli toli" ~ 1.020594
                                ,.$locality == "bumbulan" ~ 0.487511
                                ,.$correctedlocality == "desa: siuna; 5km east of siuna" ~ -0.743473
                                ,.$locality == "doluduo" ~ 0.5257
                                ,.$locality == "g. niotokan" ~ 0.938269
                                ,.$locality == "gimpoe [= gimpu]" ~ -1.643669
                                ,.$locality == "gumbasa forest" ~ -1.266930
                                ,.$locality == "gumbasa forest, 1 12's, 119 57e" ~ -1.2000
                                ,.$locality == "gumbasa proj.,bobo(palolo)." ~ -1.131576
                                ,.$locality == "gumbasa proj.,bunga." ~ -1.103644
                                ,.$correctedlocality == "gumbasa proj.,gumbasa" ~ -1.266930
                                ,.$correctedlocality == "gumbasa proj.,kalawara" ~ -1.171075
                                ,.$locality == "gumbasa proj.,lonja" ~ -1.127056
                                ,.$correctedlocality == "gumbasa proj.,omu." ~ -1.275159
                                ,.$locality == "gumbasa proj.,pakuli." ~ -1.224932
                                ,.$correctedlocality == "gumbasa proj.,pandere" ~ -1.192041
                                ,.$locality == "gumbasa proj.,petimbe." ~ -1.120059
                                ,.$correctedlocality == "gumbasa proj.,pewanu" ~ -1.028678
                                ,.$locality == "gumbasa proj.,sibowi." ~ -1.125760
                                ,.$locality == "gumbasa proj.,tatura" ~ -0.921032
                                ,.$locality == "gunung kanino" ~ -1.2833
                                ,.$locality == "gunung nokilalaki" ~ -1.2167
                                ,.$correctedlocality == "hanggira" ~ -1.727060
                                ,.$locality == "kalabat" ~ 1.453333
                                ,.$locality == "kalamanta" ~ -1.979066
                                ,.$locality == "kalawara" ~  -1.17
                                ,.$locality == "kanawu" ~ -1.323
                                ,.$locality == "kantewu" ~ -1.732303
                                ,.$locality == "karowa" ~ 0.927792
                                ,.$locality == "katu" ~ -1.575017
                                ,.$locality == "kecamatan ulubongka; marowo" ~ -0.959830
                                ,.$correctedlocality == "kecamatan: pagimana; desa siuna, kampung siuna" ~ -0.743473
                                ,.$locality == "kecamatan: bittuang; desa: tiroan; dusun: bolokan; tana toraja; 2*56.145's, 119*41.872'e" ~ -2.9358
                                ,.$locality == "kecamatan: rindingallo; desa awan, dusun: rantekarua; tana toraja; 2* 54.130's, 119* 41.839'e" ~ -2.9022
                                ,.$locality == "kecamatan: rindingallo; desa awan, dusun: rantekarua; tana toraja; 2* 54.130's, 119*41.841'e" ~ -2.9022
                                ,.$locality == "kecamatan: rindingallo; desa:awan, dusun: rantekarua; kampung: to' ranni'; tana toraja; 2* 55.871's, 119*  42.737'e" ~ -2.9312
                                ,.$locality == "kecamatan: ulubongka: marowo" ~ -0.959830
                                ,.$locality == "kecamatan:bittuang;desa tiroan;dusun:bolokan" ~ -3.0054
                                ,.$locality == "kecamatan:bittuang;desa tiroan;dusun:ranteharua" ~ -2.9241
                                ,.$locality == "kecamatan:rindingallo;desa:awan;dusun:rantekarua c" ~ -2.9241
                                ,.$locality == "kecamatan:rindingallo;desa:awan;dusun:rantekarua k" ~ -2.9241
                                ,.$locality == "kecamatan:rindingallo;desa:awan;dusun:rantekarva" ~ -2.9241
                                ,.$locality == "kecamatan:tompo bulu; cikoro: desa; dusun: lembang bune;kampung: parang bintolo" ~ -5.408100
                                ,.$locality == "kecamatang:bittuany;desa:teroan;dusun:tiroan;kampu" ~ -2.949210
                                ,.$locality == "koelawi [= kulawi]" ~ -1.4500
                                ,.$locality == "koemersot" ~ 1.471564
                                ,.$locality == "kosio" ~ 0.511980
                                ,.$locality == "kuala navusu" ~ -0.9667
                                ,.$locality == "lake ilalai" ~ 0.857625
                                ,.$locality == "lake iloloi" ~ 0.857625
                                ,.$correctedlocality == "lake lindu" ~ -1.3167
                                ,.$locality == "lalolis" ~ -3.9500
                                ,.$locality == "lambara" ~ -0.737800
                                ,.$locality == "lambasang" ~ -5.262888
                                ,.$locality == "lampesue" ~ -2.6257
                                ,.$locality == "lampobatang" ~ -5.346667
                                ,.$locality == "langko" ~ -1.847900
                                ,.$locality == "latimodjong geb." ~ -3.445556
                                ,.$locality == "lembeh" ~ 1.420592
                                ,.$locality == "likoepang [= likupang]" ~ 1.672033
                                ,.$locality == "lombasang" ~ -5.262888
                                ,.$locality == "lonka" ~ -3.856514
                                ,.$locality == "luwu, tohalosi" ~ -3.305221
                                ,.$locality == "luwu, wawondula" ~ -3.305221
                                ,.$locality == "majane island" ~ -5.14766
                                ,.$locality == "malenge" ~ -0.26588
                                ,.$locality == "malakasa" ~ -0.9667
                                ,.$locality == "mamu" ~ -1.909004
                                ,.$locality == "mengkoka mts." ~ -3.690833
                               ,.$county == "menkoka mts." ~ -3.690833
                               ,.$county == "mengkoka mts." ~ -3.690833
                               ,.$locality == "mopuya" ~ 0.565508
                                ,.$locality == "near lindu lake" ~ -1.3167
                                ,.$locality == "oena - oena" ~ -0.175199
                                ,.$locality == "paku ure" ~ 1.134895
                                ,.$locality == "pakuli" ~ -1.2333
                                ,.$locality == "palmas island [= pulau miangas], 75 mi se of mindanao, philippines" ~ 5.556079
                                ,.$correctedlocality == "pandere" ~ -1.18
                                ,.$locality == "parahaleang" ~ -0.939380
                                ,.$locality == "peleng island [= pulau peling], near liang" ~ -1.573357
                                ,.$locality == "pinedapa" ~ -1.416700
                                ,.$locality == "roeroekan" ~ 1.345355
                                ,.$locality == "rompo" ~ -1.655901
                                ,.$locality == "sadaunta" ~ -1.383300
                                ,.$locality == "sedoa" ~ -1.279786
                                ,.$locality == "singkalong" ~ -2.316667
                                ,.$locality == "soroako" ~ -2.550972
                                ,.$locality == "sungai miu" ~ -1.383300
                                ,.$locality == "sungai oha kecil" ~ -1.366700
                                ,.$locality == "sungai paleleh" ~ 0.9833333
                                ,.$locality == "sungai sadaunta" ~ -1.383333
                                ,.$locality == "sungai tokararu" ~ -1.283333
                                ,.$locality == "sungai tolewonu" ~ -1.066667
                                ,.$correctedlocality == "talassa (maros)" ~ -5.000000
                                ,.$locality == "tamadue" ~ -1.487311
                                ,.$locality == "tanke salokko" ~ -3.5833
                                ,.$correctedlocality == "temboan" ~ 1.069014
                                ,.$locality == "tenga" ~ 1.184082
                                ,.$locality == "tj.pulisan" ~ 1.690000
                                ,.$locality == "toeare [= tuare], bada" ~ -1.914619
                                ,.$locality == "tolai" ~ -1.088016
                                ,.$correctedlocality == "tolitoli" ~ 0.876823
                                ,.$correctedlocality == "tomado" ~ -1.346752
                                ,.$locality == "ulubongka county, marowo" ~ -0.959830
                                ,.$county == "pulisan" ~ 1.669793
                                ,.$county == "lembeh island" ~ 1.424914
                               ,.$county == "tenga" ~ 1.181150
                               ,.$locality == "wasuponda" ~ -2.280723
                                ,.$locality == "wawondula" ~ -2.635680
                                ,.$locality == "wori" ~ 1.579471
                                ,.$locality == "wowondula" ~ -2.635680
                               ,.$locality == "panaikang" ~ -5.15
                               ,.$correctedlocality == "maros borong" ~ -5.165676
                               ,TRUE ~ .$decimallatitude
  )
  )

# Checking unusual range for the ones with coordinates
sulbat %>% filter(decimallongitude<=0) %>% summarise(n=n()) 
sulbat %>% filter(decimallongitude<119.070) 
sulbat<-sulbat %>% filter(decimallongitude!=(-67.97))
sulbat %>% filter(decimallongitude>150)
sulbat %>% filter(decimallatitude>2.5)

# Now checking for NAs in latitudes
NAcoords<-sulbat %>% filter(is.na(sulbat$decimallatitude)) %>% group_by(correctedlocality) %>% summarise(n=n())
NAcoords$correctedlocality<-factor(NAcoords$correctedlocality)
levels(NAcoords$correctedlocality) # there are 112 localities with NAs

# As these locality cannot be saved (see location remarks) we delete it along with coarse localities
levels(sulbat$correctedlocality)
# potential candidates to be deleted are
# "dodolo", "north",  "north sulawesi is.", "south sulawesi is.", and "south sulawesi"
# But we first check whether this seemingly coarse locality has remarks
sulbat %>% 
  select(recordedby,
         recordnumber,
         collectioncode,
         collectionid,
         institutioncode,
         dateidentified,
         identifiedby,
         decimallatitude,
         decimallongitude,
         verbatimlocality,
         county,
         stateprovince,
         locationaccordingto,
         locality,
         locationremarks,
         scientificname) %>% filter(sulbat$correctedlocality=="utara")
# utara has correct county but other has no detailed locality although coordinates are clear
# they need verification though, so we are free to delete this coarse locality
sulbat %>% 
  select(recordedby,
         recordnumber,
         collectioncode,
         collectionid,
         institutioncode,
         dateidentified,
         identifiedby,
         decimallatitude,
         decimallongitude,
         verbatimlocality,
         county,
         stateprovince,
         locationaccordingto,
         locality,
         locationremarks,
         scientificname) %>% filter(sulbat$georeferenceverificationstatus=="verified by collector")

# Let's see what kind of bats are there
sulbat %>% filter(locality=="NORTH") %>% group_by(scientificname) %>% summarise(n=n()) # 8 species, most without epithet
sulbat %>% filter(locality=="NORTH SULAWESI IS.") %>% group_by(scientificname) %>% summarise(n=n()) # 9 species
sulbat %>% filter(locality=="SOUTH SULAWESI") %>% group_by(scientificname) %>% summarise(n=n()) # 12 species, 2 without epithet

# How vary are their coordinates?
sulbat %>% select(decimallatitude,decimallongitude) %>% filter(sulbat$locality=="NORTH")
## NORTH coordinates are quite vary to 0.1, but unfortunately most will require verification
sulbat %>% select(decimallatitude,decimallongitude) %>% filter(sulbat$locality=="NORTH SULAWESI IS.")
## NORTH SULAWESI IS. coordinates are quite vary to 0.1-1, but unfortunately most will require verification
sulbat %>% select(decimallatitude,decimallongitude) %>% filter(sulbat$locality=="SOUTH SULAWESI")
## SOUTH SULAWESI vary 0.1-1 also, but unfortunately will require verification also

# Just delete them
sulbat<-sulbat %>% filter(locality!="north")
sulbat<-sulbat %>% filter(locality!="south sulawesi")
sulbat<-sulbat %>% filter(locality!="south sulawesi is.")
sulbat<-sulbat %>% filter(locality!="north sulawesi is.")
sulbat<-sulbat %>% filter(locality!="dodolo")

# Check again for locality
sulbat$locality<-factor(sulbat$correctedlocality)
levels(sulbat$locality)

# As we want to measure species richness, we take records that are complete in species level-------------
sulbat %>% group_by(specificepithet) %>% summarise(n=n()) 
levels(sulbat$specificepithet)
sulbat %>% filter(is.na(sulbat$scientificname)) %>% group_by(specificepithet) %>% summarise(n=n())
sulbat<-sulbat %>% filter(specificepithet!="")
sulbat$specificepithet<-factor(sulbat$specificepithet)
sulbat$scientificname<-factor(sulbat$scientificname)
levels(sulbat$specificepithet)

# also, recode species name to only include genus and specific epithet, without subspecies
sulbat<-mutate(sulbat,
               speciesname = fct_recode(scientificname
                                        ,"Emballonura alecto" = "Emballonura alecto alecto"
                                        ,"Cynopterus brachyotis" = "Cynopterus brachyotis brachyotis"
                                        ,"Emballonura nigrescens" = "Emballonura nigrescens papuanus"
                                        ,"Hipposideros ater" = "Hipposideros ater saevus"
                                        ,"Hipposideros cervinus" = "Hipposideros cervinus cervinus"
                                        ,"Hipposideros diadema" = "Hipposideros diadema speculator"
                                        ,"Hipposideros dinops" = "Hipposideros dinops speculator"
                                        ,"Hipposideros galeritus" = "Hipposideros galeritus celebensis"
                                        ,"Macroglossus minimus" = "Macroglossus minimus lagochilus"
                                        ,"Megaderma spasma" = "Megaderma spasma celebensis"
                                        ,"Miniopterus schreibersii" = "Miniopterus schreibersii blepotis"
                                        ,"Miniopterus tristis" = "Miniopterus tristis celebensis"
                                        ,"Mosia nigrescens" = "Mosia nigrescens papuana"
                                        ,"Myotis ater" = "Myotis ater ater"
                                        ,"Myotis formosus" = "Myotis formosus weberi"
                                        ,"Myotis moluccarum" = "Myotis moluccarum moluccarum" 
                                        ,"Nyctimene cephalotes" = "Nyctimene cephalotes cephalotes"
                                        ,"Nyctimene cephalotes" = "Nyctimene cephalotes aplini"
                                        ,"Pipistrellus tenuis" = "Pipistrellus tenuis sewalanus"
                                        ,"Pipistrellus tenuis" = "Pipistrellus tenuis sewelanus"
                                        ,"Pteropus alecto" = "Pteropus alecto alecto"
                                        ,"Rhinolophus borneensis" = "Rhinolophus borneensis celebensis"
                                        ,"Rhinolophus celebensis" = "Rhinolophus celebensis celebensis"
                                        ,"Rhinolophus euryotis" = "Rhinolophus euryotis tatar"
                                        ,"Rhinolophus philippinensis" = "Rhinolophus philippinensis maros"
                                        ,"Rousettus amplexicaudatus" = "Rousettus amplexicaudatus amplexicaudatus")
)
levels(sulbat$speciesname)
glimpse(sulbat)

# write new clean data set
setwd("D:/TAMBORA/SkrpDat/14 Event - Warung Kopi Biodiverksripsi/warkopBiodiverskripsi")
write.csv(sulbat,"SulawesiBat_cleaned.csv")

