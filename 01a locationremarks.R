detach("package:raster", unload=TRUE)

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
         eventdate,
         scientificname) %>% filter(sulbat$correctedlocality=="toli toli")

sulbat  %>% filter(sulbat$institutioncode=="AMNH") %>% group_by(correctedlocality) %>% summarise (n=n())

# Remarks for "palopo, soroako"
# soroako from http://mapcarta.com/15602850

# Remarks for "toli toli"
# it is the central one, accorfing to Raven notes  Toli Toli (Naloe), North Celebes, coll. H . C. Raven,
# December 30, 1914. U.S.N.M. no. 199754. 

# Remarks for "mato angin"
# = matto angin

# Remarks for "tapanjen"
# http://cdn.gbif.org/occurrence/731986117

# Remarks for "maros district; forest in karaenta"
# http://www.indonesiatravelingguide.com/sulawesi-natural-resources/south-sulawesi-nature-reserves/south-sulawesi-karaenta-nature-reserve/

# Remarks for "donggala, lake lindu"
# donggala is quite far east of lake lindu but they are in the same province
# so i just use donggala

# Remarks for "buka island, tomini gulf, sulawesi"
# There is no "buka island" in North Sulawesi, so it must be "bukabuka island"

# Remarks for "above lake tonado"
# There is no "lake tonado" in North Sulawesi, so it must be "lake tondano"

# "kalabat" = "klabat"
#  "pewanu" = "pewunu"

# "doluduo" from map carta

# Remarks for "g. niotokan"
# I got the coordinate by inserting village location of air panas niotokan: http://cawanapeka.blogspot.de/2014/12/air-panas-niotokan.html
# It is in pinaesaan, tompaso baru

# Remarks for "utara"
# there are pulisan and lembeh island in the county
sulbat %>% select(recordedby, decimallatitude,decimallongitude,verbatimlocality,county,stateprovince,locationaccordingto,locality,locationremarks) %>% filter(sulbat$locality=="roeroekan")

# Remarks for "wowondula"
# it is on central sulawesi, but the state province is south sulawesi, so it is a typo of "wawondula"

# Remarks for "talassa (maros)" "roeroekan"
# coordinates were found from Bergmans and Rozendaal publication as talassa is nowhere to be found
# there is only patalassang in gowa which is near maros but not so near
# those localities are recorded by Heinrich.

# Remarks for "sungai toluwenu" "sungai tokararu" "sungai sadaunta" "sungai paleleh" "sungai miu" 
# "pinedapa" "malakasa" "gunung nokilalaki" "gunung kanino"
# coordinates are found from Guy G Musser publication

# Remarks for "near lindu lake"
# it is a collection of NAMRU-2 near lindu, according to Musser they have station in Tomado...:
# Valley of Danau Lindu, forest near Tomado, 01u199S, 120u039E (estimated from Sheet SA 50-8)
# "lake lindu" also georeferenced here as they came from the same collector and county

# Remarks for "majane island"
# I have checked in http://biocache.ala.org.au/occurrences/search?q=collection_uid%3Aco123&fq=species_subgroup%3A%22Bats%22&fq=%28identified_by%3A%22Kitchener%2C+D.J.%22+OR+identified_by%3A%22Kitchener%2CD.J.%22%29&fq=country%3A%22Indonesia%22&fq=collectors%3A%22Kitchener%2C+D.J.+Et%22&fq=species%3A%22Pteropus%20alecto%22
# There are the records but without coordinates. Another locality note is "Ujung Pandang"
# But Ujung pandang does not have majane island
# It is a Pteropus anyway so I will take Ujung Pandang coordinates for this locality

# Remarks for "luwu, wawondula"
# I go for luwu regency as wawondula is a different place
# same for "luwu, tohalosi"

# Remarks for "lampesue"
# The coordinate is from http://mapcarta.com/16310732 "Salo Lampesue"

# Remarks for "kecamatan:bittuang;desa tiroan;dusun:bolokan"
# The coordinate is from http://mapcarta.com/16310732 "Salu Bolokan"

# Remarks for "kanawu"
# The coordinate is from http://mapcarta.com/16310732 "Kanawu"

# Remarks for "lambasang"
# It is an area in west sulawesi, but county said "lampo-batang", which is in south sulawesi
# so I give the same coordinate as lombasang in south sulawesi

# Remarks for "lalolis" from Musser
# Lalolei (spelled ''Lalolis'' on specimen tags and some maps)

# Remarks for "lake ilalai"
# There is no lake ilalai in north sulawesi so it is a typo of "lake iloloi"
# I put "lake iloloi" coordinates there

# Need to check again for "mengkoka mts." "tanka salokko" and "wawo"

# have to delete "dodolo" as there are 2 place named dodolo, one in central and one in south
# and there is no clue this dodolo is which one

# have to delete "garden" in south sulawesi as it is unclear