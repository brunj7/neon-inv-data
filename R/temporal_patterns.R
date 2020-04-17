library(lme4)
library(lmerTest)


# Competitive exclusion analysis
subplot_comp_ex=data.frame(subplot_uniq_id=unique(uniq_ids),uninv_reg_coef=NA,inv_reg_coef=NA)

  for (pp in 1:length(plot_ids)){
    print(pp)
    curr_plot=plot_ids[pp]
    for (bb in 1:length(subplot_ids)){
      
      curr_subplot=subplot_ids[bb]
      subplot_uniq_id=paste(curr_plot,curr_subplot,sep="_")
      rep_years=subplot_year_data[which(subplot_year_data$plotID==curr_plot&subplot_year_data$subplotID==curr_subplot),"date"]
      # for (tt in 1:length(rep_years)){
      if(length(rep_years)>0){
        curr_dat=subplot_year_data[which(subplot_year_data$plotID==curr_plot&subplot_year_data$subplotID==curr_subplot),]
        curr_dat=curr_dat[order(curr_dat$date),]
        curr_dat[,1:10]
        
        
        if(length(which(curr_dat$invaded==TRUE))==0)
        {
          uninv_dates=1:length(curr_dat$date)
        }else{
        uninv_dates=which(curr_dat$date<min(curr_dat$date[which(curr_dat$invaded==TRUE)]))
        }
      if(length(uninv_dates)>1){
        uninv_reg=lm(curr_dat$Nat_richness[uninv_dates]~curr_dat$date[uninv_dates])  
        subplot_comp_ex$uninv_reg_coef[match(subplot_uniq_id,subplot_comp_ex$subplot_uniq_id)]=summary(uninv_reg)$coefficients[2,1]
      }
        
        if(length(which(curr_dat$invaded==TRUE))==0){
          inv_dates=vector()
    }else{
      inv_dates=which(curr_dat$date>=min(curr_dat$date[which(curr_dat$invaded==TRUE)]))
    }
      if(length(inv_dates)>1){
        inv_reg=lm(curr_dat$Nat_richness[inv_dates]~curr_dat$date[inv_dates])  
        subplot_comp_ex$inv_reg_coef[match(subplot_uniq_id,subplot_comp_ex$subplot_uniq_id)]=summary(inv_reg)$coefficients[2,1]
      }
      
      
    # }
  }
    }
  
  }

boxplot(subplot_comp_ex$uninv_reg_coef,subplot_comp_ex$inv_reg_coef)
plot(x=jitter(c(rep(1,length(subplot_comp_ex$uninv_reg_coef)),rep(2,length(subplot_comp_ex$inv_reg_coef)))), y=jitter(c(subplot_comp_ex$uninv_reg_coef,subplot_comp_ex$inv_reg_coef)))
t.test(subplot_comp_ex$uninv_reg_coef,subplot_comp_ex$inv_reg_coef)




############################################################
# Biotic resistance

# rich_mat=matrix(NA,nrow=length(unique(uniq_ids)),ncol=length(unique(subplot_year_data$date))) #Matrix for native species richness at each subplot across all timepoints
# inv_mat=matrix(NA,nrow=length(unique(uniq_ids)),ncol=length(unique(subplot_year_data$date))) #Matrix for invasive species richness at each subplot across all timepoints
# 
# inv_pres_vec=rep(0,samp_sizes) #Vector for invader presence at each subplot, will indicate how many timepoints had invaders
# # lake_vec=rep(multi_surveys[multisetid[ii]],samp_sizes)

subplot_bio_res=data.frame(subplot_uniq_id=vector(),nat_rich_pre=vector(),inv_status_post=vector())



for (pp in 1:length(plot_ids)){
  print(pp)
  curr_plot=plot_ids[pp]
  for (bb in 1:length(subplot_ids)){
    
    curr_subplot=subplot_ids[bb]
    subplot_uniq_id=paste(curr_plot,curr_subplot,sep="_")
    rep_years=subplot_year_data[which(subplot_year_data$plotID==curr_plot&subplot_year_data$subplotID==curr_subplot),"date"]
    if(length(rep_years)>1){
      
      pre_years=1:(length(rep_years)-1)
      post_years=2:length(rep_years)
      curr_dat=subplot_year_data[which(subplot_year_data$plotID==curr_plot&subplot_year_data$subplotID==curr_subplot),]
      curr_dat=curr_dat[order(curr_dat$date),]
      curr_dat[,1:10]
      

      # identify if there are uninvaded timepoints to look for changes
      if(length(which(curr_dat$invaded==FALSE))==0)
      {
        uninv_dates=vector()
      }else{
        uninv_dates=which(curr_dat$date<min(curr_dat$date[which(curr_dat$invaded==TRUE)]))
      }
      
      if(length(uninv_dates)>0){
        nat_rich_pre=curr_dat$Nat_richness[pre_years[which(pre_years%in%uninv_dates)]] #native richness of subplot
        inv_status_post=curr_dat$invaded[post_years[which(pre_years%in%uninv_dates)]] #Indicates invasion status in subsuquent time point
        subplot_rep=rep(subplot_uniq_id,length(pre_years[which(pre_years%in%uninv_dates)])) #subplot id
        
        add_df=data.frame(subplot_uniq_id=subplot_rep,nat_rich_pre=nat_rich_pre,inv_status_post=inv_status_post) # make data for current subplot into data frame
        
        subplot_bio_res=rbind(subplot_bio_res,add_df) #add data for current subplot to full dataset
        }
      
      
      
            
    }
    
    
  }
}


summary(glmer(subplot_bio_res$inv_status_post~subplot_bio_res$nat_rich_pre+(1|subplot_bio_res$subplot_uniq_id),family="binomial"))
# summary(glm(subplot_bio_res$inv_status_post~subplot_bio_res$nat_rich_pre,family="binomial"))

plot(jitter(as.numeric(subplot_bio_res$inv_status_post))~subplot_bio_res$nat_rich_pre)
# plot(x=jitter(c(rep(1,length(subplot_comp_ex$uninv_reg_coef)),rep(2,length(subplot_comp_ex$inv_reg_coef)))), y=jitter(c(subplot_comp_ex$uninv_reg_coef,subplot_comp_ex$inv_reg_coef)))