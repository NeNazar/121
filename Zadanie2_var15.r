library(factoextra)
library(readr)
library(dplyr)
table = read_csv("https://www.dropbox.com/s/erhs9hoj4vhrz0b/eddypro.csv?dl=1",
                 skip=1, comment = "[")
table = table[-1,]
table[table == -9999] = NA
table <- table %>% filter(daytime == "T")
table <- table %>% filter(year(date) == 2013)
table <- table %>% filter(month(date) %in% c(03, 04, 05))
table = table %>% select(where(is.numeric))
table =-table %>% select(-contains("co2"), co2_flux)
var_data = table %>% summarise_all( ~var(.x,na.rm=T))
var_data[is.na(var_data)] = 0
cpa_data = table[,as.logical(var_data != 0)]
cor_matrix = cor(na.exclude(cpa_data))
heatmap(cor_matrix)
co2_cor = as.numeric(cor_matrix[84,])
names(co2_cor) = names(cor_matrix[84,])
cpa_dataf = cpa_data[,co2_cor > 0.1 | co2_cor < -.1]
data_pca = prcomp(na.exclude(cpa_dataf),scale=TRUE)
fviz_pca_var(data_pca,repel = TRUE, col.var = "steelblue")
model = lm(co2_flux ~ air_heat_capacity+air_pressure+u_unrot+Tdew+air_heat_capacity+h2o_var, table)
summary(model)

formula = paste(c("co2_flux ~ ",
                  paste(names(cpa_dataf)[-36], collapse = "+")),
                collapse = "")
formula = as.formula(formula)


model_first = lm(formula, cpa_dataf)
summary(model_first)
formula2 = formula(co2_flux ~ DOY + wind_dir + u_spikes + h2o_signal_strength_7200 + 
                     h2o...128  + w_spikes + u_rot + air_molar_volume + h2o_var + 
                     ts_spikes + yaw + qc_LE)
model2 = lm(formula2, cpa_dataf)
summary(model2)
formula3 = formula(co2_flux ~ wind_dir + u_spikes + h2o_signal_strength_7200 + 
                     h2o...128  + w_spikes + u_rot + h2o_var + ts_spikes + yaw + qc_LE)
model3 = lm(formula3, cpa_dataf)
summary(model3)
anova(model3)
formula4 = formula(co2_flux ~ wind_dir + u_spikes + h2o_signal_strength_7200 + 
                     h2o...128 + h2o_var + yaw + qc_LE)
model4 = lm(formula4, cpa_dataf)
summary(model4)
anova(model4)
formula5 = formula(co2_flux ~ wind_dir + u_spikes + h2o...128 + h2o_var + yaw + qc_LE)
model5 = lm(formula5, cpa_dataf)
summary(model5)
anova(model5)
