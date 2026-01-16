#install.packages("Rtools")
library(readxl)
library(dplyr)


# Set the file path and name of the .xlsx file -------
data_path <- "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/evidence_paper_data/"

# Read the data into a data frame
data <- read_excel(paste0(data_path,"Meta_data_2024.02.15.xlsx"), sheet = "meta_PCC")
data <- data[-1,]
data <- data[-1,]

length(sort(unique(data$study_id))) # Number of articles 193
sort(unique(data$study_id)) # Number of articles 193

sort(unique(data$y_metric_recla)) #28
sort(unique(data$y_metric_recla_2)) #5
#"adoption" "awareness" "disadoption" "intensity of adoption" "interest"

table(data$y_metric_recla)
table(data$y_metric_recla_2)
table(data$y_metric_recla, data$y_metric_recla_2)

sort(unique(data$x_metric_recla)) #221

key<- read_excel(paste0(data_path,"Meta_data_2024.02.15.xlsx"), sheet = "KEY")%>%
  mutate(factor_category= if_else(
    factor_category =="Political and institutional context",paste("P&I context", "\n(",factor_sub_category,")",sep=""),
      factor_category ))

names(key)
sort(unique(key$x_metric_recla))#222
sort(unique(key$factor_category))#14
sort(unique(key$factor_sub_category))

list<-read_excel(paste0(data_path,"Dataset_1.xlsx"), sheet = "Literature")%>%
  mutate(study_id=as.character(study_id))


data_clean<-data%>%
  filter(!is.na(study_id))%>%
  left_join(list,by="study_id")%>%
  left_join(key %>% select(x_metric_recla, factor, factor_category, factor_sub_category), by = "x_metric_recla") %>%
  mutate(study_model_id= paste(study_id,model_id,sep="_"))%>%
  mutate(factor_category= if_else(is.na(factor_category),"Other",factor_category )) # I need to reclasify these factors

names(data_clean)
sort(unique(data_clean$publication_year)) #1982-2023
sort(unique(data_clean$study_id))
length(unique(data_clean$study_model_id)) #354
sort(unique(data_clean$factor_category))


##### Diversified farming systems ------
## m_dp_recla = by system
data_clean$m_dp_recla<- stringr::str_to_sentence(data_clean$dp_recla)
sort(unique(data_clean$dp_recla))
sort(unique(data_clean$m_dp_recla))

data_clean$m_dp_recla[data_clean$m_dp_recla %in% c("Agroforestry and fallow",
                                                                     "Crop rotation and cover crops",
                                                                     "Crop rotation and intercropping",
                                                                     "Land with temporary fallow and cover crops"
)]<- "Combined practices"
data_clean$m_dp_recla[data_clean$m_dp_recla %in% c("Integrated aquaculture-agriculture")]<- "Agro-aquaculture"
data_clean$m_dp_recla[data_clean$m_dp_recla %in% c("Grazing cut and carry",
                                                                     "Integrated crop-livestock",
                                                                     "Silvopasture")]<- "Agro-silvopasture"
data_clean$m_dp_recla[data_clean$m_dp_recla %in% c("Embedded seminatural infrastructures")]<- "Embedded seminatural habitats"
data_clean$m_dp_recla[data_clean$m_dp_recla %in% c("Land with temporary fallow")]<- "Fallow"

sort(unique(data_clean$m_dp_recla)) #10 systems
table(data_clean$m_dp_recla)


####### Countries -------
sort(unique(data_clean$country)) #49

UN_region <- read_excel("C:/Users/andreasanchez/OneDrive - CGIAR/2_chapter_PhD/meta-analysis/1_chapter_effect_size/UN_region.xlsx", sheet = "UN_subregion")%>%
  mutate(Country_Name= if_else(Country_Name == "United States of America (The)","USA",
                               if_else(Country_Name == "Democratic Rep. of the Congo (The)","Democratic Republic of the Congo",
                                       if_else(Country_Name =="United Republic of Tanzania (The)","Tanzania",
                                               if_else(Country_Name =="Bolivia (Plurinational State of)","Bolivia",
                                                       if_else(Country_Name == "Sudan (The)", "Sudan",
                                                               if_else(Country_Name=="Republic of Moldova (The)", "Moldova",
                                                                       if_else(Country_Name=="Philippines (The)", "Philippines",
                                                                               if_else(Country_Name=="Viet Nam", "Vietnam",
                                                                                       if_else(Country_Name == "Iran (Islamic Republic of)", "Iran",
                                                                                               if_else(Country_Name == "Niger (The)","Niger",
                                                                                                       Country_Name)))))))))))%>%
  dplyr::select(Country_Name, UN_Regions, UN_sub_region,Developed_Developing)%>%
  dplyr::rename("m_un_region"="UN_Regions",
                "m_un_subregion"="UN_sub_region")

sort(unique(UN_region$Country_Name))

data_clean<- data_clean%>%
  left_join(UN_region, by=c("country" ="Country_Name"))
names(data_clean)

data_clean$m_un_region[data_clean$country %in% "Vietnam, Thailand"] <-"Asia"
data_clean$m_un_subregion[data_clean$country %in% "Vietnam, Thailand"] <-"South-eastern Asia"
data_clean$m_un_region[
  data_clean$country %in% c("Ethiopia, Ghana, Kenya, Malawi,  Mozambique, Nigeria, Tanzania, Uganda,  Zambia",
                            "Kenya, Tanzania, Ethiopia")] <-"Africa"
data_clean$m_un_subregion[
  data_clean$country %in% c("Kenya, Tanzania, Ethiopia")] <-"Eastern Africa"
data_clean$country[data_clean$country %in% "Vietnam"] <- "Vietnam"


sort(unique(data_clean$country[is.na(data_clean$m_un_subregion)])) #1
sort(unique(data_clean$country)) #45 countries
sort(unique(data_clean$m_un_region)) #5 regions
sort(unique(data_clean$m_un_subregion)) #15 subregions

table(data_clean$country,data_clean$m_un_region)
length(unique(data_clean$study_id)) #189 studies 
sort(unique(data_clean$country[data_clean$m_un_subregion %in% c("Central America")]))
table(data_clean$country,data_clean$m_un_subregion)

write.csv(data_clean,"C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/evidence_paper_data/evidence_map_data_clean.csv", row.names=FALSE)
