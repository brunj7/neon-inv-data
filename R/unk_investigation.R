# reclassifying unknowns. meant to by sourced from diverisity_data_prep.R
# There is a flora specifically for the Jornada
# there is probably a plant list for the santa rita experimental range on seinet
# but that site is down at the moment

full_on_cover[full_on_cover$taxonID == "ABUTI","nativeStatusCode"] <-"N"
full_on_cover[full_on_cover$taxonID == "ASTRA","nativeStatusCode"] <-"N"
full_on_cover[full_on_cover$taxonID == "CORYP","nativeStatusCode"] <-"N"        # Coryphantha is a native genus 
full_on_cover[full_on_cover$taxonID == "CRYPT" &
                full_on_cover$site == "JORN","nativeStatusCode"] <-"N"          # All Cryptantha are native in the Jornada
full_on_cover[full_on_cover$taxonID == "CRYPTSPP"&
                full_on_cover$site == "JORN","nativeStatusCode"] <-"N"
full_on_cover[full_on_cover$taxonID == "CUSCU","nativeStatusCode"] <-"N"        # Cuscuta is native
full_on_cover[full_on_cover$taxonID == "ERIOG","nativeStatusCode"] <-"N"        # Eriogonums are all native
full_on_cover[full_on_cover$taxonID == "EUPHO"&
                full_on_cover$site == "JORN","nativeStatusCode"] <-"N"          # all Euphorbia (and Chamasyceae) are native in Jorn 

# investigating erodium unk
full_on_cover[full_on_cover$taxonID == "ERODI",]
full_on_cover[full_on_cover$plotID == "SRER_013",] %>%
  filter(str_sub(taxonID,1,2) == "ER" & family == "Geraniaceae")
# need to check the SRER plant list -- these are usually not hard to figure out

full_on_cover[full_on_cover$taxonID == "IPOMO","nativeStatusCode"] <-"N"  
full_on_cover[full_on_cover$taxonID == "LINUM"&
                full_on_cover$site == "JORN","nativeStatusCode"] <-"N"  
full_on_cover[full_on_cover$taxonID == "MALVAC","nativeStatusCode"] <-"N"  
full_on_cover[full_on_cover$taxonID == "OPUNT","nativeStatusCode"] <-"N"  
full_on_cover[full_on_cover$taxonID == "PANIC"&
                full_on_cover$site == "JORN","nativeStatusCode"] <-"N"  
full_on_cover[full_on_cover$taxonID == "PROSO","nativeStatusCode"] <-"N"   
full_on_cover[full_on_cover$taxonID == "SENNA","nativeStatusCode"] <-"N"   
full_on_cover[full_on_cover$taxonID == "SPORO","nativeStatusCode"] <-"N"  
full_on_cover[full_on_cover$taxonID == "SPOROSPP","nativeStatusCode"] <-"N"   
full_on_cover[full_on_cover$taxonID == "ARTEM","nativeStatusCode"] <-"N"   
full_on_cover[full_on_cover$taxonID == "ASTRASPP","nativeStatusCode"] <-"N"     # Astragalus
full_on_cover[full_on_cover$taxonID == "ATRIPSPP","nativeStatusCode"] <-"N"     # Atriplex
full_on_cover[full_on_cover$taxonID == "CACTAC","nativeStatusCode"] <-"N"   
full_on_cover[full_on_cover$taxonID == "CACTACSPP","nativeStatusCode"] <-"N"   
full_on_cover[full_on_cover$taxonID == "DRABA","nativeStatusCode"] <-"N"   
full_on_cover[full_on_cover$taxonID == "DRABASPP","nativeStatusCode"] <-"N"   
full_on_cover[full_on_cover$taxonID == "ERIOGSPP","nativeStatusCode"] <-"N"   
full_on_cover[full_on_cover$taxonID == "ELYMU","nativeStatusCode"] <-"N"   
full_on_cover[full_on_cover$taxonID == "OENOT","nativeStatusCode"] <-"N"   
full_on_cover[full_on_cover$taxonID == "OENOTSPP","nativeStatusCode"] <-"N"   
full_on_cover[full_on_cover$taxonID == "OPUNTSPP","nativeStatusCode"] <-"N"   
full_on_cover[full_on_cover$taxonID == "PEDIO","nativeStatusCode"] <-"N"        # cactus
full_on_cover[full_on_cover$taxonID == "PEDIOSPP","nativeStatusCode"] <-"N"
full_on_cover[full_on_cover$taxonID == "PHYSA2","nativeStatusCode"] <-"N"
full_on_cover[full_on_cover$taxonID == "PHYSASPP","nativeStatusCode"] <-"N"     #Physaria spp
full_on_cover[full_on_cover$taxonID == "POLEMO","nativeStatusCode"] <-"N"
full_on_cover[full_on_cover$taxonID == "POLEMOSPP","nativeStatusCode"] <-"N"
full_on_cover[full_on_cover$taxonID == "SCLER10","nativeStatusCode"] <-"N"
full_on_cover[full_on_cover$taxonID == "TOWNS","nativeStatusCode"] <-"N"
full_on_cover[full_on_cover$taxonID == "TOWNSSPP","nativeStatusCode"] <-"N"     # Townsendia
full_on_cover[full_on_cover$taxonID == "ACHNA","nativeStatusCode"] <-"N"        # Achnatherum
full_on_cover[full_on_cover$taxonID == "ALLIU","nativeStatusCode"] <-"N"        # double check if there are non-native alliums in GB
full_on_cover[full_on_cover$taxonID == "ARENA","nativeStatusCode"] <-"N"
full_on_cover[full_on_cover$taxonID == "ATRIP","nativeStatusCode"] <-"N"
full_on_cover[full_on_cover$taxonID == "BROMU","nativeStatusCode"] <-"I"        # botanist speculated it might be young Bromus tectorum, so that means it's probably not perennial thus not native
full_on_cover[full_on_cover$taxonID == "CASTI2","nativeStatusCode"] <-"N"       # Castilleja
full_on_cover[full_on_cover$taxonID == "CASTI2SPP","nativeStatusCode"] <-"N"
full_on_cover[full_on_cover$taxonID == "CREPI","nativeStatusCode"] <-"N"        # Crepis
full_on_cover[full_on_cover$taxonID == "CREPISPP","nativeStatusCode"] <-"N"
full_on_cover[full_on_cover$taxonID == "EPILO","nativeStatusCode"] <-"N"        # Epilobium
full_on_cover[full_on_cover$taxonID == "ERIGE2","nativeStatusCode"] <-"N"       # Erigeron
full_on_cover[full_on_cover$taxonID == "ERIGE2SPP","nativeStatusCode"] <-"N"
full_on_cover[full_on_cover$taxonID == "OROBA","nativeStatusCode"] <-"N"
full_on_cover[full_on_cover$taxonID == "OROBASPP","nativeStatusCode"] <-"N"
full_on_cover[full_on_cover$taxonID == "SCROPH","nativeStatusCode"] <-"N"       # Making assumptions... but the morpho comments don't sount like mullein
full_on_cover[full_on_cover$taxonID == "SCROPHSPP","nativeStatusCode"] <-"N"
full_on_cover[full_on_cover$taxonID == "TRAGOSPP","nativeStatusCode"] <-"I"
full_on_cover[full_on_cover$taxonID == "AMBRO","nativeStatusCode"] <-"N"        # Ambrosias in SRER are probs native
full_on_cover[full_on_cover$taxonID == "AYENI","nativeStatusCode"] <-"N"
full_on_cover[full_on_cover$taxonID == "BIDEN","nativeStatusCode"] <-"N"
full_on_cover[full_on_cover$taxonID == "ECHIN3","nativeStatusCode"] <-"N"       
full_on_cover[full_on_cover$taxonID == "IPOMOSPP","nativeStatusCode"] <-"N"
full_on_cover[full_on_cover$taxonID == "LYCIU","nativeStatusCode"] <-"N"        # Lycium
full_on_cover[full_on_cover$taxonID == "SIDA","nativeStatusCode"] <-"N"
full_on_cover[full_on_cover$taxonID == "SIDASPP","nativeStatusCode"] <-"N"
full_on_cover[full_on_cover$taxonID == "SISYM","nativeStatusCode"] <-"I"
full_on_cover[full_on_cover$taxonID == "THYMO","nativeStatusCode"] <-"N"


# looking at the unknown lepidiums in srer
x$div_1m2Data[str_sub(x$div_1m2Data$scientificName,1,4) == "Lepi",] %>%
  filter(str_sub(plotID, 1,4) == "SRER") %>%
  dplyr::select(scientificName) %>%
  unique()
# so, there's densiflorum and lasiocarpum to choose from. lasiocarpum has hair,
# densiflorum can be glabrous (according to FNA)
