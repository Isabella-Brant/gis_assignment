library(dplyr)
library(tidyverse)
library(janitor)
library(sf)
library(tmap)
library(tmaptools)
library(ggplot2)
library(ggthemes)
library(corrr)

############### code ###############

data <- st_read("data/msoas/msoas_final.shp")

#Renaming some fields that were abbreviated
#tot = total
#p/pct = percentage
#mn = mean
data <- data%>%
  rename(code = MSOA11C,
         totcases = ttl_css,
         p_0_15 = pc_0_15,
         hh_tot = hhlds_t,
         p_hhwdc = pct_hh_,
         p_lphh = pct_lp_,
         p_1phh = pct_1p_,
         pct_oth = pct_thr,
         pct_oo = pct_wnt,
         pct_det = pct_dtc,
         pct_ql4 = pct_q4_,
         mn_hh_i = mn_hh_n,
         pct_obe = pct_16_,
         le_f = le_feml,
         le_m = le_male,
         gs_m2 = gs_r_m2,
         gs_ha = gs_ar_h)


#Transforming the total cases and greenspace area fields 
#pr_cases = proportion of covid-19 cases out of the msoa population
#pr_gsm2/ha = proportion of defined greenspace accessibility area (>2ha with 5 min proximity) to the
#...total area of the msoa in m2 and ha
data <- data%>%
  mutate(pr_cases = totcases / pop * 100,
         pr_gsm2 = gs_m2 / area_m2 * 100,
         pr_gsha = gs_ha / area_ha * 100)


#Distribution plot of greenspace proportion
#Pretty normal distribution
data%>%
  ggplot(aes(x=pr_gsha)) +
  geom_histogram(position="identity", 
                 alpha=0.5, 
                 bins=15, 
                 fill="lightblue2", col="lightblue3")+
  geom_vline(aes(xintercept=mean(pr_gsha)),
             color="blue",
             linetype="dashed")+
  labs(title="Distribution of the proportion of proximate public green spaces defined by green spaces
  that are atleast 2 hectares within a 5 minute walking distance to the total land area of London MSOAS.",
       x="Proportion of proximate public green space",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))


#Distribution of proportion of total covid-19 cases to the population size of
#each msoa collected between 22/09/2020 to 08/12/2020
#Appears positively skewed
data%>%
  ggplot(aes(x=pr_cases)) +
  geom_histogram(position="identity", 
                 alpha=0.5, 
                 bins=15, 
                 fill="lightblue2", col="lightblue3")+
  geom_vline(aes(xintercept=mean(pr_cases)),
             color="blue",
             linetype="dashed")+
  labs(title="Distribution of the proportion of total COVID-19 cases to the population
       of each MSOA.",
       x="Proportion of COVID-19 cases (%)",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))




######K-means cluster of covid proportion and greenspace proportion
data_for_km <- data%>%
  st_drop_geometry(.)%>%
  dplyr::select(c(pr_cases, pr_gsha))

fit <- data_for_km %>%
  kmeans(., 3, nstart=25)

library(tidymodels)
centroid <- tidy(fit)%>%
  #print the results of the cluster groupings
  print()%>%
  dplyr::select(pr_cases, pr_gsha)

p <- ggplot(data_for_km,aes(pr_cases, pr_gsha))+
  geom_point(aes(colour=factor(fit$cluster)))+
  geom_point(data=centroid,aes(pr_cases, pr_gsha), size=7, shape=18)+ theme(legend.position="none")

cluster_test <- fit %>% 
  augment(., data)%>%
  dplyr::select(code, .cluster)%>%
  #make sure the .cluster column is numeric
  mutate(across(.cluster, as.numeric))%>%
  # join the .cluster to our sf layer
  left_join(data, 
            .,
            by = c("code" = "code"))

#Map but colour needs changing
map <- ggplot(cluster_test) + 
  geom_sf(mapping = aes(fill=.cluster))+
  scale_fill_continuous(breaks=c(1,2,3))
map

#Alternative map
tmap_mode("view")

tm_shape(cluster_test)+
  tm_polygons(".cluster",
              palette="Set3",
              n=3,
              popup.vars=c("name", ".cluster"),
              title="K-Means Clustering of COVID-19 and Greenspace Area Variables")

###### Spatial auto-correlation using Moran's I
##This is for exploratory purposes
#We are doing this on the greenspace variable to see if any spatial
#autocorrelation exists. We suspect it does but want to concretely prove this
tmap_mode("view")

tm_shape(data) +
  tm_polygons("pr_gsha",
              style="jenks",
              palette="BuGn",
              n=4,
              midpoint=NA,
              popup.vars=c("name", "pr_gsha"),
              title="Proportion of proximate green space to total land area (%)")

#Some evidence of clustering of greenspace proportions but will confirm with 
#Moran's I statistics etc
library(sp)
library(spData)
library(spdep)

#First calculate the centroids of all msoas in London
coordsMSOA <- data%>%
  st_centroid()%>%
  st_geometry()

plot(coordsMSOA,axes=TRUE)

#create a neighbours list
#Will be using Queens Contiguity 
#Snap value - recommended including this argument from this article...
#https://gis.stackexchange.com/questions/355300/poly2nb-spdep-not-identifying-neighbours-correctly
msoa_nb <- data%>%
  poly2nb(., queen=T, snap = 0.0001) #snap value needed

#plot them
plot(msoa_nb, st_geometry(coordsMSOA), col="red")
#add a map underneath
plot(data$geometry, add=T)

#create a spatial weights object from these weights
msoa.lw <- msoa_nb %>%
  nb2listw(., style="C")

##Weight matrix has been defined
#Pulling the proportion of greenspace in ha to see if spatial clustering exists
I_MSOA_Global_prgs <- data%>%
  pull(pr_gsha) %>%
  as.vector()%>%
  moran.test(., msoa.lw)

#Moran's I statistic (score shows moderate spatial autocorrelation 0.212011)
I_MSOA_Global_prgs

##Geary's C - this tells us if similar or disimilar values are clustering
#Alternative hypothesis: expectation greater than statistic
#GC = 0.78959.... | Expectation is > statistic
#Can accept the alternative hypothesis
#<1 - positive spatial autocorrelation or similar values clustering,
C_MSOA_Global_prgs <- 
  data %>%
  pull(pr_gsha) %>%
  as.vector()%>%
  geary.test(., msoa.lw)

C_MSOA_Global_prgs

#Getis Ord - this tells us whether high or low values are clustering
#If GO > Expected = high values clustering
#If GO < Expected = low values clustering
#The G statistic > Expected = High values are clustering
G_MSOA_Global_prgs <- 
  data %>%
  pull(pr_gsha) %>%
  as.vector()%>%
  globalG.test(., msoa.lw)

G_MSOA_Global_prgs

##Now calculating LOCAL Moran's I and Getis Ord for each MSOA
I_MSOA_Local_prgs <- data %>%
  pull(pr_gsha) %>%
  as.vector()%>%
  localmoran(., msoa.lw)%>%
  as_tibble()

slice_head(I_MSOA_Local_prgs, n=5)

data2 <- data%>%
  mutate(prgs_I =as.numeric(I_MSOA_Local_prgs$Ii))%>%
  mutate(prgs_Iz =as.numeric(I_MSOA_Local_prgs$Z.Ii))

##Mapping the outputs
summary(data2$prgs_I)
summary(data2$prgs_Iz)

library(RColorBrewer)
breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)
MoranColours<- brewer.pal(8, "PiYG")

tmap_mode("view")

tm_shape(data2) +
  tm_polygons("prgs_Iz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, Proportion of Greenspaces
              of 2ha or more within a 5 min walking distance 
              in each MSOA")

#Local Getis Ord for Hot and Cold spots
Gi_MSOA_Local_prgs <- data%>%
  pull(pr_gsha) %>%
  as.vector()%>%
  localG(., msoa.lw)

head(Gi_MSOA_Local_prgs)

#Joining
data2 <- data2 %>%
  mutate(prgs_G = as.numeric(Gi_MSOA_Local_prgs))

#bewwer map
GIColours<- rev(brewer.pal(8, "RdBu"))

#now plot on an interactive map
##This map is a lot more informative than the Local Moran's I as it clearly...
#...distinguishes between the hot and cold spatial autocorrelations
#For instance looking at Richmond Park area in South-West London vs City...
#...of London area in Central London
tm_shape(data2) +
  tm_polygons("prgs_G",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, Greenspaces in London")


######### Analysing COVID-19 Prevalence in London MSOAS #########
##This next part of the analysis wil be to investigate C=19 prevalence in 
#London MSOA's and creating a model with and without the greenspace variable in
#it to see what its role is 

#Starting with global regression models
#OLS (bivariate and multivariate)

##Since the covid variable is positively skewed I'm going to log that variable
#This is the distribution plot of the pr_cases variable:
ggplot(data, aes(x=log(pr_cases))) + 
  geom_histogram()

#OLS Plot 1 - log(pr_cases) ~ pr_gsha
q1 <- qplot(x = pr_gsha, 
            y = log(pr_cases), 
            data=data)

#plot with a regression line - note, I've added some jitter here as the x-scale is rounded
#There doesn't seem to be much of a relationship shown using this plot
#Ever so slight negative regression line
q1 + stat_smooth(method="lm", se=FALSE, size=1) + 
  geom_jitter()

#Going to run a regression on this to examine results
Regression_data1 <- data%>%
  dplyr::select(c(pr_cases, pr_gsha))

model1 <- Regression_data1%>%
  lm(log(pr_cases) ~
       pr_gsha,
     data=.)

summary(model1)

##The p-value is about 0.06, pr_gsha is insignificant in a simple bivariate model 
##Coefficient is v small (look after its been run)

##Since we are exploring COVID-19 prevalence and the role of greenspace it makes
##sense to create a multi-variate regression model with AND without the greenspace
##variable. Doing this we can observe the contribution of the greenspace variable
##in the model. First I will create a multi-variate model and observe the significance
#of the variables chosen 

##Stage 1 - after running several bi-variate models with the other variables in
#the msoa data, this is the first Multi-variate model I am making

#Variables selected in for m-variate model 1
mlr_data <- data%>%
  dplyr::select(pop_den, av_hhsz, pct_65, p_hhwdc, p_1phh, pct_bam,
                pct_oo, pct_srn, pct_det, pct_flt, pct_sem, pct_trr,
                pct_ql4, pct_eco_i, pct_limlt, pct_lmltt,pct_ntl,
                pct_h_vb, pct_h_b, pct_h_f, pct_obe, le_m, pr_gsha, pr_cases)

#Multi-variate regression WITH pr_gsha
mlr_1 <- lm(log(pr_cases) ~
              pop_den + av_hhsz + pct_65 + p_hhwdc + p_1phh + pct_bam +
              pct_oo + pct_srn + pct_det + pct_flt + pct_sem + pct_trr +
              pct_ql4 + pct_eco_i + pct_limlt + pct_lmltt + pct_ntl +
              pct_h_vb + pct_h_b + pct_h_f + pct_obe + le_m + pr_gsha,
            data=mlr_data)

summary(mlr_1)

##The results in this model show that in a multi-variate model the pr_gsha 
#variable is significant with a p-value of 0.016312
#Several variables have been flagged as significant in the model so model 2
#will drop the insignifcant variables

#Only selected the significant variables from model_1
mlr_data_2 <- data%>%
  dplyr::select(pop_den, av_hhsz, pct_65, p_hhwdc, pct_bam,
                pct_srn, pct_ql4, pct_eco_i, pct_h_b,le_m, pr_gsha, pr_cases)

#model 2 - with pr_gsha variable
mlr_2 <- lm(log(pr_cases) ~ pop_den + av_hhsz + pct_65 + p_hhwdc + pct_bam +
              pct_srn + pct_ql4 + pct_eco_i + pct_h_b + le_m + pr_gsha, 
            data=mlr_data_2)

#The new model shows that all the variables except the pop_den variable are
#significant in the model
#The model shows that pr_gsha is a negative coefficient, meaning that the
#higher the proportion of greenspace in a MSOA, the decrease in the COVID variable
#The Rsquared is still on the low end at 0.29 and 0.28 (for adj r-sq) but we can
#conclude that the variables in the model are significant 
summary(mlr_2)

#VIF will be run on all the variables in model 2 
#This checks for multi-collinearity 
#av_hhsz has the highest VIF at 6.5 however since it is under the threshold of 
#10 we will keep it in the model.
#install.packages("regclass")
library(regclass)
VIF(mlr_2)

#Now finalising the model by dropping the pop_den variable
#Re-running VIF on it
final_data <- data%>%
  dplyr::select(code, av_hhsz, pct_65, p_hhwdc, pct_bam,
                pct_srn, pct_ql4, pct_eco_i, pct_h_b,le_m, pr_gsha, pr_cases)

mlr_final <- lm(log(pr_cases) ~ av_hhsz + pct_65 + p_hhwdc + pct_bam +
                  pct_srn + pct_ql4 + pct_eco_i + pct_h_b + le_m + pr_gsha, 
                data=final_data)

summary(mlr_final)

#VIF results are acceptable
VIF(mlr_final)

##Creating a final model WITHOUT the pr_gsha variable
#When comparing the results we can see that the pr_gsha has a tiny coefficient
#however adds to the r-squared value. Then again, the r-squared value does increase
#when adding a new variable to the model. When we compare the adjusted R-squ we
#can see that it has ever so slightly improved the model by including the pr_gsha
#increases by ...
mlr_final2 <- lm(log(pr_cases) ~ av_hhsz + pct_65 + p_hhwdc + pct_bam +
                   pct_srn + pct_ql4 + pct_eco_i + pct_h_b + le_m, 
                 data=final_data)

summary(mlr_final2)

#Some final analysis before moving onto SLM/SEM/GWR
#Saving the residual results from the mlr_final model and writing them out
mlr_final_data <- mlr_final %>%
  augment(., final_data)

# add the residuals backs to the data sf 
data <- data %>%
  mutate(final_model_resids = residuals(mlr_final))

#Checking for homo/hetroscedasticity
#printing some model diagnositcs. 
#Looks fine
par(mfrow=c(2,2))    #plot to 2 by 2 array
plot(mlr_final)


#######Exploring spatial autocorrelation of the residuals
##From the Moran's I, Geary's C and Getis-Ords statistics we observed earlier
#We know that the pr_gsha is spatially autocorrelated meaning they aren't 
#independent and would be biasing the model
#We'll plot the residuals of the model to see if what the obvious patterns are

tmap_mode("view")

final_resids_pal <- rev(brewer.pal(5, "RdBu"))

tm_shape(data) +
  tm_polygons("final_model_resids",
              palette = final_resids_pal,
              alpha = 0.5)

#This plot confirms the spatial autocorrelation visually but we can run Moran's
# I to prove that spatial autocorrelation exists between the residuals
#We can use the coordsMSOA, msoa.lw and msoa_nb that we made earlier 
Queen_MI <- data %>%
  st_drop_geometry()%>%
  dplyr::select(final_model_resids)%>%
  pull()%>%
  moran.test(., msoa.lw)%>%
  tidy()

Queen_MI

#Results show that Queen_MI is about 0.460 this shows that there's moderate 
#spatial autocorrelation between our residuals and therefore is likely to be
#biasing our M-V model
#This further suggests that the Multi-variate model is inappropriate for 
#accurately exploring our spatial data and the relationships that exist
#This finding means we can move onto creating a Spatial Lag Model (SLM) and 
#Spatial Error Model (SEM)

#Spatial Lag Model (SLM) with Queens case matrix
install.packages("spatialreg")
library(spatialreg)

slm_final_model_queen <- lagsarlm(log(pr_cases) ~ av_hhsz + pct_65 + p_hhwdc + pct_bam +
                                    pct_srn + pct_ql4 + pct_eco_i + pct_h_b + le_m + pr_gsha,
                                  data = final_data,
                                  nb2listw(msoa_nb, style="C"),
                                  method = "eigen")

summary(slm_final_model_queen)
tidy(slm_final_model_queen)
glance(slm_final_model_queen)
#what do the outputs show?
#The RHO
#The LR
#The LM is significant so we can reject the null hypothesis and accept the
#alternative meaning spatial autocorrelation exists

## Writing out the SLM residuals
data <- data %>%
  mutate(slm_q_resids = residuals(slm_final_model_queen))

#Running the Moran's I on the SLM residuals
QMoran <- data %>%
  st_drop_geometry()%>%
  dplyr::select(slm_q_resids)%>%
  pull()%>%
  moran.test(., msoa.lw)%>%
  tidy()

QMoran

#The Queens matrix no longer shows spatial autocorrelation since moran's I is
#now 0.0577

#Spatial Errors Model (SEM) with Queens case matrix
sem_final_model_q <- errorsarlm(log(pr_cases) ~ av_hhsz + pct_65 + p_hhwdc + pct_bam +
                                  pct_srn + pct_ql4 + pct_eco_i + pct_h_b + le_m + pr_gsha,
                                data = final_data,
                                nb2listw(msoa_nb, style="C"), 
                                method = "eigen")

tidy(sem_final_model_q)
sem_final_model_q

#Interpretation:
#*From looking at the SLM and SEM results it could be that the spatial auto-
#*correlation is causing some problems with the model but it also could be that
#*the global regression models are ultimately unsuitable for the data we are
#*exploring. A GWR may be more suitable to investigate the local story, for example
#*in some areas the slope coefficients between the dependent and independent variables
#*may differ, and this isn't captured in the global models. We can explore this 
#*by carrying out a GWR

##Geographically Weighted Regression (GWR)

#install.packages("spgwr")
library(spgwr)

#First we are just going to print out the final M-V OLS model again
mlr_final <- lm(log(pr_cases) ~ av_hhsz + pct_65 + p_hhwdc + pct_bam +
                  pct_srn + pct_ql4 + pct_eco_i + pct_h_b + le_m + pr_gsha, 
                data=final_data)

summary(mlr_final)

#Quick plot of the final M-V OLS model residuals again
qtm(data, fill = "final_model_resids")

#For GWR we need to create data into sp object instead of sf
st_crs(final_data) = 27700

final_data_SP <- final_data%>%
  as(., "Spatial")

#Do same for centroids of msoas
st_crs(coordsMSOA) = 27700

coordsMSOA_SP <- coordsMSOA%>%
  as(., "Spatial")

coordsMSOA_SP

##Need to calculate the kernel bandwidth for the GWR
GWRbandwidth <- gwr.sel(log(pr_cases) ~ av_hhsz + pct_65 + p_hhwdc + pct_bam +
                          pct_srn + pct_ql4 + pct_eco_i + pct_h_b + le_m + pr_gsha, 
                        data=final_data_SP, 
                        coords=coordsMSOA_SP,
                        adapt=T)

#Running the gwr model
gwr.model = gwr(log(pr_cases) ~ av_hhsz + pct_65 + p_hhwdc + pct_bam +
                  pct_srn + pct_ql4 + pct_eco_i + pct_h_b + le_m + pr_gsha, 
                data=final_data_SP, 
                coords=coordsMSOA_SP, 
                adapt=GWRbandwidth, 
                hatmatrix=TRUE, 
                se.fit=TRUE)

gwr.model

#Interpreting the model in the discussion
#*Difference in coefficients between each MSOAs for certain variables indicate
#*interesting spatial patterning
#*We can add the coefficients to the original dataframe

results <- as.data.frame(gwr.model$SDF)
names(results)

final_data <- final_data%>%
  mutate(coef_av_hhsz = results$av_hhsz,
         coef_pct_65 = results$pct_65,
         coef_p_hhwdc = results$p_hhwdc,
         coef_pct_bam = results$pct_bam,
         coef_pct_srn = results$pct_srn,
         coef_pct_ql4 = results$pct_ql4,
         coef_pct_eco_i = results$pct_eco_i,
         coef_pct_h_b = results$pct_h_b,
         coef_le_m = results$le_m,
         coef_pr_gsha = results$pr_gsha)

##Add the GWR coefficients back to the original data sf object
#Need to drop geometry from final_data first
final_data <- final_data%>%
  st_drop_geometry()%>%
  dplyr::select(c(code,
                  coef_av_hhsz,
                  coef_pct_65,
                  coef_p_hhwdc,
                  coef_pct_bam,
                  coef_pct_srn,
                  coef_pct_ql4,
                  coef_pct_eco_i,
                  coef_pct_h_b,
                  coef_le_m,
                  coef_pr_gsha))

data <- data%>%
  merge(., final_data,
        by.x=1,
        by.y=1)

#Plotting the pr_gsha GWR residuals
tm_mode("plot")

tm_shape(data) +
  tm_polygons(col = "coef_pr_gsha", 
              palette = "RdBu", 
              alpha = 0.5)

#Interpretation
#*Looking at this first map we can see that the coefficients vary over space
#*We can see that there is quite a divide between negative and positive coefficients
#*for pr_gsha. Notably in south east London by the river we can observe that as 
#*the proportion of greenspace increases the overall prevalence of COVID-19 is reduced
#*this fits with the original hypothesis that places which are not deprived of greenspace
#*has lower prevalence of covid.
#*In some areas such as in the north west we can see a positive relationship 
#*meaning that as proportion of greenspace increases, so does the proportion of covid. 
#*This is an interesting observation and suggests that the proportion of greenspace
#*has a more complex influence on covid-19. It seems rather counter-intuitive but
#*perhaps indicates a more complicated dimension of the virus and its relationship
#*with green space 

#What would be helpful is to re-map the original pr_gsha map to see how it compares
#with out residual results
#We can then use these to compare in the discussion 

gs_pal <- brewer.pal(5, "Greens")

tmap_mode("plot")
tm_shape(data) +
  tm_polygons(col = "pr_gsha", 
              palette = gs_pal,
              title="Proportion of green space (%)",
              alpha = 0.5)+
  tm_layout(title = "Proximate, public green space in each London MSOA determined by\nits proportion of the MSOA's total land area.", 
            title.size = 1.0,
            title.position = c("left", "top"),
            inner.margins = c(0.12, 0.17, 0.12, 0.17),
            frame=T) +
  tm_legend(title.size = 0.9,
            text.size = 0.7,
            position = c("left", "bottom"))
  tm_legend(show =T)+
  tm_scale_bar(color.dark = "gray60",
             position = c("right", "bottom"))
  
  
summary(mlr_final2)


