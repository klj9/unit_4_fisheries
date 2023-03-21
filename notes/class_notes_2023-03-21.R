#2023-03-21
# GLM - generalized linear model

source("build_collapse_table.R")#call in script that built collapse data table and removed that outlier
head(collapse)

#LOGISTIC REGRESSION - useful for binary variable
#   glm because equation is a linear combination of x variables (i.e. can be rearranged to look like ordinary lm)

#is a fishery more likely to collapse depending on what type of fishery it is?

model_data = collapse %>%
  group_by(stockid) %>%
  summarize(ever_collapsed = any(current_collapse)) %>% #if current collapse has ever been true for that stock, returns true
  ungroup() %>%
  left_join(stock) %>% #add info back in about fish/fishery type
  left_join(metadata) %>%
  mutate(FisheryType = as.factor(FisheryType)) %>% #need to force character class into factor type
  filter(!is.na(FisheryType))

summary(model_data)
#x var is fishery type, y var is ever collapsed

model_l = glm(ever_collapsed ~ FisheryType, data=model_data, family="binomial")

summary(model_l)
#four types have significant p-values --> fishery type tells us something useful

model_data %>% distinct(FisheryType) %>% arrange(FisheryType)
#intercept is for flatfish

#make a plot of the chance of collapse based on fishery type

FisheryType = model_data %>%
  distinct(FisheryType)

model_l_predict = predict(model_l, newdata=FisheryType, se.fit=TRUE, type="response")
#default for type is link - gives log odds vs probability as percentage

head(model_l_predict)
collapse_fisherytype_predictions = cbind(FisheryType, model_l_predict)

ggplot(data=collapse_fisherytype_predictions, 
       aes(x=FisheryType, y=fit, fill=fit))+
  geom_bar(stat="identity", show.legend=FALSE)+
  geom_errorbar(aes(ymin=fit-se.fit, ymax=fit+se.fit), width=0.2)+
  coord_flip()+
  xlab("Probability of Collapse")

#POISSON MODEL - useful for count data, more appropriate for small data than normal dist
#   zero inflation might be a problem

#predict how long a stock spends in a collapsed state based on UdivUmsypref (harvest/msy) and B/Bmsy (biomass)
#   UdivUmsypref > 1 means overfished
#   BdivBmsypref <1 means low biomass

u_summary = timeseries_values_views %>%
  filter(!is.na(UdivUmsypref),
         !is.na(BdivBmsypref)) %>%
  group_by(stockid) %>%
  summarize(yrs_data=n(),
            ratio_yrs_overfished=sum(UdivUmsypref>1)/yrs_data,#counts years overfished and divides by years we have data for
            ratio_yrs_lowstock=sum(BdivBmsypref<1)/yrs_data) %>%
  select(-yrs_data) %>%

head(u_summary)

collapse_summary = collapse %>%
  group_by(stockid) %>%
  summarize(yrs_data=n(),
            yrs_collapsed = sum(current_collapse)) %>%
  inner_join(u_summary, by="stockid") #only gives rows with both stock and Udiv/Bdiv data

head(collapse_summary)
#already see a lot of zeros - maybe problematic
hist(collapse_summary$yrs_collapsed)
table(collapse_summary$yrs_collapsed)#247 zeros - model will probably be zero inflated

#revise question - for a stock that has collapsed at some point, can time spent overfished/low stock explain collapse duration

collapse_summary_zero_trunc = collapse_summary %>%
  filter(yrs_collapsed>0)

table(collapse_summary_zero_trunc$yrs_collapsed)

#build model
model_p = glm(yrs_collapsed ~ ratio_yrs_overfished + ratio_yrs_lowstock,
              offset(log(yrs_data)),
              data=collapse_summary_zero_trunc,
              family="poisson")
#offset term accounts for difference in monitoring duration (effort)

summary(model_p)
#are the variables significant bc good predictors, or bc of wide variation?
#need to check for overdispersion

library(AER)

AER::dispersiontest(model_p)#p-value<0.001 - we're overdispersed

#try a quasi-poisson model instead
model_qp = glm(yrs_collapsed ~ ratio_yrs_overfished + ratio_yrs_lowstock,
              offset(log(yrs_data)),
              data=collapse_summary_zero_trunc,
              family="quasipoisson")
summary(model_qp)#much less significant results
