# Compare NERRs at different spatial scales

# what is '2PLANT'?
# are dates being appropriately grouped?
# Different species identified in different bouts?
# Are empty locations removed form data sets (particularly subplot data)
# have unknowns been figured out?
# Why fewer potential species in subplots dataset



names(vegan_friendly_div)
vegan_friendly_div$plotID
species_cols=5:816
sp_codes=names(vegan_friendly_div)[species_cols]
plot_level_presence=vegan_friendly_div
plot_level_presence[,species_cols]=as.numeric(plot_level_presence[,species_cols]>0)

plot_level_presence$site=gsub(x = plot_level_presence$plotID,pattern = "(.+)_.+",replacement = "\\1")
sites=unique(plot_level_presence$site)

unique(plot_level_presence$year)


# Get invasion status for each species
invasive_status=data.frame(sp_codes=unique(full_on_cover$taxonID),nativeStatusCode=full_on_cover$nativeStatusCode[match(unique(full_on_cover$taxonID),table = full_on_cover$taxonID)])



# Site Level analysis
site_year_data=data.frame(matrix(ncol=length(species_cols)+6,nrow=0))

colnames(site_year_data)=c("site","date","Tot_richness","Nat_richness","Inv_richness","invaded",names(plot_level_presence)[species_cols])

species_cols_site=species_cols
  
curr_line=0
for (ss in 1:length(sites)){
  curr_site=sites[ss]
  curr_site_dat=plot_level_presence[which(plot_level_presence$site==curr_site),]
  timepoints=unique(curr_site_dat$year)


  for (tt in 1:length(timepoints)){

    curr_date=timepoints[tt]
    curr_date_dat=curr_site_dat[which(curr_site_dat$year==curr_date),]

    curr_line=curr_line+1

    site_year_data[curr_line,]=NA
    site_year_data$site[curr_line]=curr_site
    site_year_data$date[curr_line]=paste(curr_date)
    site_year_data[curr_line,7:ncol(site_year_data)]=colSums(curr_date_dat[,species_cols])
    site_year_data$invaded[curr_line]=any(invasive_status$nativeStatusCode[match(x = sp_codes,table = invasive_status$sp_codes)][which(colSums(curr_date_dat[,species_cols])>0)]=="I")


      }

}

site_year_data$date=as.numeric(site_year_data$date)

site_year_data$Tot_richness=rowSums(site_year_data[,which(names(site_year_data)%in%sp_codes)]>0)

site_year_data[,1:10]




sp_codes_Nat_status=invasive_status$nativeStatusCode[match(x = sp_codes,table = invasive_status$sp_codes)]
Inv_sp_codes=which(sp_codes_Nat_status=="I")
Inv_sp_codes_mask=rep(FALSE, length(sp_codes))
Inv_sp_codes_mask[which(sp_codes_Nat_status=="I")]=TRUE
Nat_sp_codes_mask=rep(FALSE, length(sp_codes))
Nat_sp_codes_mask[which(sp_codes_Nat_status!="I")]=TRUE# Also includes Unknowns!!!


site_year_data$Nat_richness=rowSums(site_year_data[,which(names(site_year_data)%in%sp_codes)[Nat_sp_codes_mask]]>0)
site_year_data$Inv_richness=rowSums(site_year_data[,which(names(site_year_data)%in%sp_codes)[Inv_sp_codes_mask]]>0)

site_year_data[,1:10]

# names(site_year_data)[which(names(site_year_data)%in%sp_codes)[Inv_sp_codes_mask]]
# which(names(site_year_data)%in%sp_codes)
# sp_codes[Inv_sp_codes_mask]

#[site_year_data$date==2019],
ggplot(data=site_year_data,aes(x=Nat_richness, y=Inv_richness,color=site)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "poisson"),show.legend = F) +
  ggtitle("Native vs. Exotic Species Richness site scale")+
  xlab("Native Species") +
  ylab("Exotic Species") +
  theme_pubr()
  # facet_wrap(~site, scales = "free")




# Plot level analysis
plot_year_data=data.frame(matrix(ncol=length(species_cols)+7,nrow=0))

plot_ids=unique(plot_level_presence$plotID)

colnames(plot_year_data)=c("site","plotID","date","Tot_richness","Nat_richness","Inv_richness","invaded",names(plot_level_presence)[species_cols])

species_cols_plot=species_cols+1

curr_line=0
for (pp in 1:length(plot_ids)){
  curr_plot=plot_ids[pp]
  curr_plot_dat=plot_level_presence[which(plot_level_presence$plotID==curr_plot),]
  timepoints=unique(curr_plot_dat$year)


  for (tt in 1:length(timepoints)){

    curr_date=timepoints[tt]
    curr_date_dat=curr_plot_dat[which(curr_plot_dat$year==curr_date),]

    curr_line=curr_line+1

    plot_year_data[curr_line,]=NA
    plot_year_data$site[curr_line]=gsub(curr_plot,pattern = "(\\w+)_.+",replacement = "\\1")
    plot_year_data$plotID[curr_line]=curr_plot
    plot_year_data$date[curr_line]=paste(curr_date)
    plot_year_data[curr_line,8:ncol(plot_year_data)]=colSums(curr_date_dat[,species_cols])
    plot_year_data$invaded[curr_line]=any(invasive_status$nativeStatusCode[match(x = sp_codes,table = invasive_status$sp_codes)][which(colSums(curr_date_dat[,species_cols])>0)]=="I")


  }

}

plot_year_data$date=as.numeric(plot_year_data$date)

plot_year_data$Tot_richness=rowSums(plot_year_data[,which(names(plot_year_data)%in%sp_codes)]>0)

plot_year_data[,1:10]

plot_year_data$Nat_richness=rowSums(plot_year_data[,which(names(plot_year_data)%in%sp_codes)[Nat_sp_codes_mask]]>0)
plot_year_data$Inv_richness=rowSums(plot_year_data[,which(names(plot_year_data)%in%sp_codes)[Inv_sp_codes_mask]]>0)

plot_year_data[,1:10]


#[plot_year_data$date==2019],
ggplot(data=plot_year_data,aes(x=Nat_richness, y=Inv_richness,color=site)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "poisson"),show.legend = F) +
  ggtitle("Native vs. Exotic Species Richness plot scale")+
  xlab("Native Species") +
  ylab("Exotic Species") +
  theme_pubr()
  # facet_wrap(~plotID, scales = "free")




#####
# Subplot level
cover_subplots 
print(cover_subplots[,1])

subplot_year_data=data.frame(matrix(ncol=length(species_cols)+8,nrow=0))

plot_ids=unique(cover_subplots$plotID)
subplot_ids=unique(cover_subplots$subplotID)

colnames(subplot_year_data)=c("site","plotID","subplotID","date","Tot_richness","Nat_richness","Inv_richness","invaded",names(plot_level_presence)[species_cols])

species_cols_subplot=species_cols+4

curr_line=0
for (pp in 1:length(plot_ids)){
  print(pp)
  curr_plot=plot_ids[pp]
  for (bb in 1:length(subplot_ids)){
    
  curr_subplot=subplot_ids[bb]
  curr_subplot_dat=cover_subplots[which(cover_subplots$plotID==curr_plot&cover_subplots$subplotID==subplot_ids[bb]),]
  
  curr_subplot_dat$year=gsub(x = curr_subplot_dat$bout_year,pattern = "(.+)_.+",replacement = "\\1")
  timepoints=unique(curr_subplot_dat$year)
  if(length(timepoints)>0){

  for (tt in 1:length(timepoints)){
    
    curr_date=timepoints[tt]
    curr_date_dat=curr_subplot_dat[which(curr_subplot_dat$year==curr_date),]
    
    curr_line=curr_line+1
    
    subplot_year_data[curr_line,]=NA
    subplot_year_data$site[curr_line]=gsub(curr_plot,pattern = "(\\w+)_.+",replacement = "\\1")
    subplot_year_data$plotID[curr_line]=curr_plot
    subplot_year_data$subplotID[curr_line]=curr_subplot
    subplot_year_data$date[curr_line]=paste(curr_date)
    for(sp in 1:length(sp_codes)){
      if (sp_codes[sp]%in%names(curr_date_dat)){
    subplot_year_data[curr_line,sp_codes[sp]]=colSums(curr_date_dat[,sp_codes[sp]])
      }else{
        subplot_year_data[curr_line,sp_codes[sp]]=0
      }
    }
    subplot_year_data$invaded[curr_line]=any(invasive_status$nativeStatusCode[match(x = sp_codes,table = invasive_status$sp_codes)][which(subplot_year_data[curr_line,species_cols_subplot]>0)]=="I")
  }
    
  }
  
  }
  
}

subplot_year_data$date=as.numeric(subplot_year_data$date)
subplot_year_data$Tot_richness=rowSums(subplot_year_data[,which(names(subplot_year_data)%in%sp_codes)]>0)

subplot_year_data[,1:10]
# uniq_ids=paste(subplot_year_data$plotID,subplot_year_data$subplotID,sep = "_")

subplot_year_data$Nat_richness=rowSums(subplot_year_data[,which(names(subplot_year_data)%in%sp_codes)[Nat_sp_codes_mask]]>0)
subplot_year_data$Inv_richness=rowSums(subplot_year_data[,which(names(subplot_year_data)%in%sp_codes)[Inv_sp_codes_mask]]>0)

subplot_year_data[,1:10]

ggplot(data=subplot_year_data[subplot_year_data$date==2018,],aes(x=Nat_richness, y=Inv_richness,color=site)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "poisson"),show.legend = F) +
  ggtitle("Native vs. Exotic Species Richness plot scale")+
  xlab("Native Species") +
  ylab("Exotic Species") +
  theme_pubr()
  # facet_wrap(~plotID, scales = "free")


# 
# ###############################
# All plots together
par(mfrow=c(1,3))
# Subplot level
# pdf("draft_figures/NERRsubplot.pdf")
ggplot(data=subplot_year_data,aes(x=Nat_richness, y=Inv_richness,color=site)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "poisson"),show.legend = F) +
  ggtitle("Native vs. Exotic Species Richness plot scale")+
  xlab("Native Species") +
  ylab("Exotic Species") +
  theme_pubr()
# facet_wrap(~plotID, scales = "free")
# dev.off()

# Plot level
# pdf("draft_figures/NERRplot.pdf")
ggplot(data=plot_year_data,aes(x=Nat_richness, y=Inv_richness,color=site)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "poisson"),show.legend = F) +
  ggtitle("Native vs. Exotic Species Richness plot scale")+
  xlab("Native Species") +
  ylab("Exotic Species") +
  theme_pubr()
# facet_wrap(~plotID, scales = "free")
# dev.off()


# Site level
# pdf("draft_figures/NERRsite.pdf")

ggplot(data=site_year_data,aes(x=Nat_richness, y=Inv_richness,color=site)) +
  geom_point() +
  geom_smooth(mapping = aes(x=Nat_richness, y=Inv_richness),method = "glm", method.args = list(family = "poisson"),show.legend = F) +
  ggtitle("Native vs. Exotic Species Richness site scale")+
  xlab("Native Species") +
  ylab("Exotic Species") +
  theme_pubr()
# facet_wrap(~site, scales = "free")
# dev.off()
