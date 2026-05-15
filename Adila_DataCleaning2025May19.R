
rm(list=ls())

##=======================================================================================
##Install packages and Load libraries
##=======================================================================================

lod.pckgs<- c("haven", "readxl", "openxlsx", "tidyverse", "data.table",
              "reshape2", "arsenal","dplyr","ggplot2", "janitor","viridis", "lubridate")


lod.pckgs
for(x in 1:length(lod.pckgs))
  library(package = lod.pckgs[x], character.only = TRUE)

##=======================================================================================
##Loading data 
##=======================================================================================
print("Please wait, data cleaning in progress!!!")
print("Please wait, data cleaning in progress!!!")
print("Please wait, data cleaning in progress!!!")

##---------------------------------------------------------------------------------------
## (1) Load Ward data
##--------------------------------------------------------------------------------------

ward.df<- read.csv("adila_ward_.csv")
glimpse(ward.df)

ward.df<-ward.df %>% mutate(Date=as.Date(data_date, format="%d/%m/%Y"),
                        ID=paste(wards,Date, sep= "||"))

table(duplicated(ward.df$ID))
(dups1<-ward.df$ID[duplicated(ward.df$ID)])
dups1

#w.df1 %>% filter(ID%in%dups1) %>% View()
ward.df<-ward.df %>% filter(!duplicated(ID))
table(duplicated(ward.df$ID))

##---------------------------------------------------------------------------------------
##(2)Import Patient data
##---------------------------------------------------------------------------------------

patient.df<- read.csv("adila_patient_.csv")
glimpse(patient.df)
patient.df<- patient.df %>% mutate(PID=pid, Date=as.Date(data_date, format="%d%b%Y"),
                         wards=wards_pps,ID=paste(wards,Date, sep= "||")) %>%
  filter(!duplicated(PID))

#Ni duplicates here
check_dups <- patient.df  %>%
  filter(duplicated(PID))

(dups2<-patient.df$PID[duplicated(patient.df$PID)])

table(duplicated(patient.df$PID))

##---------------------------------------------------------------------------------------
## (3) Merge ward and patient data
##---------------------------------------------------------------------------------------
setdiff(patient.df$ID, ward.df$ID)
setdiff(ward.df$ID, patient.df$ID)
setdiff(ward.df$wards, patient.df$wards); setdiff(patient.df$wards, ward.df$wards)
setdiff(paste(patient.df$Date), paste(ward.df$Date)); setdiff(paste(ward.df$Date), paste(patient.df$Date))

df1 <- ward.df %>% inner_join(patient.df, by=c("wards", "Date", "ID"))
dim(patient.df); dim(df1)



##Rename variables
df1 <- df1 %>% dplyr::rename(abx1=abx, dose1=dose,doseper1=doseper,
                             freqdose1=freqdose, route1=route, 
                             indication1=indication, abxdt1=abxdt, 
                             reviewdt1=reviewdt, compliance1=compliance,
                             surveynumber = surveynumber.y)

##=======================================================================================
##Data cleaning 1: Basic information 
##=======================================================================================
##Departments
medw1<-c("Renal ward","TB ward","Female medical","Male medical", 
         "Adult oncology")

surgw1<-c("Male surgical",  "Female surgical", "Burns and plastics", "ENT")

obs_gyn_w1<-c("Labour ward", "Gynaecology ward","Post natal ward","Antenatal ward" )

peads1<-c("Chatinkha ward",  "Paeds special care", "Paeds oncology", 
          "Nutrition","Mercy James", "Paeds HDU","Paeds nursery",
          "Paeds orthopedic ward", "Paeds neurosurgery", "Paeds special care")

ICU <-c("ICU")

##Data cleaning  

merged_df <- df1 %>% transmute(PID=PID, Date=Date, Year=paste0(year(Date)), 
                         Month=factor(month(Date),levels=1:12, labels=month.name),
                    Ward=trimws(wards), WardType=wardtype,WardActivity=activity, 
                    Patients =patients, Beds=beds,
                    Department=case_when(Ward%in%ICU~"ICU",
                                         Ward%in%medw1~"Medical",
                                         Ward%in%surgw1~"Surgical",
                                         Ward%in%obs_gyn_w1~"Obstetrics and Gynaecology",
                                         Ward%in%peads1~"Paediatrics"),
                    Sex=sex, Age_yrs=age,
                    Agegroups= cut(age, 
                                  right=FALSE,
                                  breaks = c(0, 5,18,50, 100),
                    labels=c("<5 years", "5-17 years", "18-49 years", "≥50 years")),
                    CurrentWeight=weight, BirthWeight=bweight,
                    BiomarkerBasedTreatment=factor(rxwbc, levels=c("Yes", "No", "Unknown")),
                    BiomarkerSample=case_when(bloodbio%in%"Yes"~"Blood",
                                              urinebio%in%"Yes"~"Urine",
                                              biofluid%in%"Yes"~"Other", .default = NA),
                    BiomarkerName=rx,BiomarkerThresholdValue=as.numeric(value),
                    BiomarkerUnits=valueunit,
                    Blood=blood,Urine=urine,Stool=stool, CerebrospinalFluid=cfluid,
                    WoundSurgeryBiopsy=wsurgery, BAL=bal,
                    SputumBronchialAspirate=sputum, OtherSpecimen=ospecimen,
                    Diabetes=diabetes, AIDS=aids, HIVstatus= hivstatus, Hematological=hemato, Stemcell=stemcell,
                    ChronicRenalDisease=crd, ActiveTB=activetb, GeneticDisorder=genetic,
                    CongenitalHeartDiseases=chd, LungDisease=lung,Neutropenia=neutro,
                    HighDoseSteroids=steroids,  Malnutrition=maln,LongCovid=lcovid, 
                    LiverDisease=liverd, Trauma=trauma, GastroenterologicalDisease=ge,
                    NeurologicalDisease=neuro,
                    Morbidities1=rowSums(across(Diabetes:NeurologicalDisease, function(x) x%in%"Yes")),
                    Morbidities=factor(Morbidities1,levels=0:3, labels=c("None", "One", "Two", "Three")),
                    AnyMorbidity=factor(ifelse(Morbidities1>0, "Yes","No")),
                    MultMorbidity=factor(ifelse(Morbidities1>1, "Yes","No")),
                    OrganismsNum1=rowSums(across(c(organism1,organism2, organism3), function(x) !is.na(x))),
                    OrganismsNum=factor(OrganismsNum1,levels=0:1, labels=c("None", "One")),
                    DiagnosisNum1=rowSums(across(c(dx1, dx2,dx3,dx4,dx5), function(x) !is.na(x))),
                    DiagnosisNum=factor(DiagnosisNum1,levels=0:5,
                                        labels=c("None", "One", "Two", "Three", "Four", "Five")),
                    AntibioticsNum1=rowSums(across(c(abx1, abx2, abx3), function(x) !is.na(x))),
                    AntibioticsNum=factor(AntibioticsNum1,levels=1:3, labels=c("One", "Two", "Three")),
                    OrganismName=organism1, Resistance=resistance1,HAI_Patient=hai,
                    surveynumber=surveynumber)
                   

dim(merged_df)
table(is.na(merged_df$Department))
##=======================================================================================
## Data cleaning 2: Antibiotic data
##=======================================================================================

##-----------------------------------------------------------------------
## (1) Function. (ABX is Antibiotic in the data codebook)
##-----------------------------------------------------------------------
##Test input::stem1<-"abx"; name1<-"Antibiotics"
ABX_DataCleaning.fn1<-function(stem1, name1){
  (vars1<-paste0(stem1, 1:3))
  abx.df1<-df1 %>% dplyr::select(all_of(c("PID", vars1))) ; head(abx.df1)
  abx.df2<-abx.df1 %>% pivot_longer(cols = -PID, values_to = name1) %>%
    mutate(Number=gsub(stem1, "", name)) %>% dplyr::select(-name)
    #mutate(Number=gsub("[^0-9.-]", "", name))
  head(abx.df2)
 return(abx.df2) 
}

##-----------------------------------------------------------------------
##(1) Apply the ABX_DataCleaning.fn1 function (ABX= antibiotic in dictionary)
##-----------------------------------------------------------------------
Stems1<-c(AntibioticName="abx", ABX_StartDate = "abxdt", DoseUint="dose",Dosage="doseper",
                            DoseFrequency="freqdose", Route="route",
          Indication="indication",  Compliance = "compliance", StopdateInfo= "reviewdt")

abx.df.list1<-mapply(function(x,y) ABX_DataCleaning.fn1(stem1 = x,name1 = y),
                     x=Stems1, y=names(Stems1), SIMPLIFY = FALSE)
sapply(abx.df.list1, names)
sapply(abx.df.list1, dim)

##Merged ABX data
abx.df1<-abx.df.list1 %>% purrr::reduce(inner_join, by=c("PID","Number")) %>%
 filter(!is.na(AntibioticName) & AntibioticName!= "")
dim(abx.df1)
names(abx.df1)

##=======================================================================================
## Data cleaning 3: Diagnosis data (dx1= diagnosis in dictionary)
##=======================================================================================
diag.df1<-df1  %>%   dplyr::select(PID,surveynumber, age, wards, dx1, dx2, dx3, dx4,dx5) %>%
  pivot_longer(cols = dx1:dx5, names_to ="dx_number", values_to =  "Diagnosis") %>%
  filter(!is.na(Diagnosis))


  levels(as_factor(diag.df1$Diagnosis))
##Diagnosis classes (top cases)
diag.tab1<-table(diag.df1$Diagnosis)
(top.diag1<-names(sort(diag.tab1[diag.tab1>20], decreasing = TRUE)))


diag.df1 <- diag.df1 %>% 
  mutate(Number=as.numeric(gsub("dx", "", dx_number)),
         DiagnosisGroups=factor(case_when(Diagnosis%in%top.diag1~Diagnosis,
                                          .default = "Other"), 
                                levels=c(top.diag1, "Other"))) 
table(diag.df1$DiagnosisGroups)
 

merged_diag.df<-merged_df %>% left_join(diag.df1, by="PID")
dim(merged_df)
dim(merged_diag.df)

##=======================================================================================
##Final data merging. abx is short for antibiotic in Data codebook
##=======================================================================================
merged_abx.df <- merged_df %>% left_join(abx.df1, by="PID") 
#view(merged_abx.df)
names(merged_abx.df)
dim(merged_abx.df)
dim(abx.df1)

##---------------------------------------------------------------------------------------
##Antibiotic classes....
##---------------------------------------------------------------------------------------
##Penicillin

unique(grep("cillin", x=merged_abx.df$AntibioticName, value = TRUE))
Penicillins1<-c("Amoxicillin","Ampicillin","Benzathine penicillin",
         "Benzyl penicillin","Piperacillin tazobactam", "Flucloxacillin" , "Xpen")


##Beta-lactam, beta-lactamase inhibitor combinations
(Co_amoxiclav1<-grep("moxiclav",merged_abx.df$AntibioticName, value = TRUE))

##3rd generation Cephalosporins
table(grep("Cef", merged_abx.df$AntibioticName, value = TRUE))
Cephalosporins1<-c("Cefotaxime","Ceftriaxone")

##Carbapenems
  table(grep("penem", merged_abx.df$AntibioticName, value = TRUE))
  Carbapenems1<-c("Meropenem","Imipenem")
  
##Macrolides and lincosamides
  table(grep("mycin", merged_abx.df$AntibioticName, value = TRUE))
  Macrolides1<-c("Erythromycin","Azithromycin") 

##Lincosamides 
  
Lincosamides1 <- c("Clindamycin")

##Tetracyclines
Tetracyclines1<-c("Doxycycline")
            
###Nitro
Nitroimidazoles1<- c("Metronidazole")

###Aminoglycosides

table(grep("Genta", merged_abx.df$AntibioticName, value = TRUE))
Aminoglycosides1 <- c("Amikacin", "Gentamycin")

###Glycopeptides 
Glycopeptides1<- c("Vancomycin")

##Beta-Lactamase inhibitor combinations 

Betalactam1<- c("Co-amoxiclav", "Co-amoxiclav/Augmentin") 

###Fluoroquinolones

Fluoroquinolones1 <- c("Ciprofloxacin")

###Folate Metabolism Inhibitors
unique(grep("Cotri", merged_abx.df$AntibioticName, value = TRUE))

FolateInhibitors1 <- c("Cotrimoxazole")

##WHO classifications
watch1<-c("Azithromycin", "Erythromicin", "Cefotaxime","Ceftriaxone","Ciprofloxacin",
          "Clindamycin", "Meropenem", "Vancomycin", "Piperacillin tazobactam")

reserve1<-c("Imipenem") 


#####Antibiotic data set with categories 
merged_abx.df <- merged_abx.df %>%
  mutate(
    AntibioticClass = case_when(
      AntibioticName %in% Penicillins1 ~ "Penicillins",
      AntibioticName %in% Co_amoxiclav1 ~ "Beta-lactam/Beta-lactamase Inhibitor Combinations",
      AntibioticName %in% Cephalosporins1 ~ "Cephalosporins",
      AntibioticName %in% Carbapenems1 ~ "Carbapenems",
      AntibioticName %in% Macrolides1 ~ "Macrolides",
      AntibioticName %in% Lincosamides1 ~ "Lincosamides", 
      AntibioticName %in% Glycopeptides1 ~ "Glycopeptides",
      AntibioticName %in% Fluoroquinolones1 ~ "Fluoroquinolones", 
      AntibioticName %in% Tetracyclines1 ~ "Tetracyclines", 
      AntibioticName %in% Aminoglycosides1 ~ "Aminoglycosides", 
      AntibioticName %in% FolateInhibitors1 ~ "Folate Metabolism Inhibitors",
      AntibioticName %in% Nitroimidazoles1 ~ "Nitroimidazoles", 
      #AntibioticName %in% Nitrofurantoin1 ~ "Nitrofurantoin", 
      .default = AntibioticName
    ),
    AntioticAwareClass = case_when(
      .default = "Access",
      AntibioticName %in% watch1 ~ "Watch",
      AntibioticName %in% reserve1 ~ "Reserve"
    )
  )


table(merged_abx.df$AntibioticName, merged_abx.df$AntibioticClass)
table(merged_abx.df$AntibioticClass) 
table(merged_abx.df$AntioticAwareClass)

##=======================================================================================
##Variable labels
##=======================================================================================
VarLabs1<-c(Year="Year",Month="Month", Ward="Ward name",WardType="WardType",
            WardActivity = "Ward activity",
            Department = "Department", 
            Patients ="Number of admitted patients",
            Beds="Number of beds",
            Sex = "Sex",
            Age_yrs = "Age in years",
            Agegroups = "Age groups in years",
            CurrentWeight = "Current weight",
            BirthWeight = "Birth weight",
            BiomarkerBasedTreatment = "Treatment based on biomarkers?",
            BiomarkerSample = "Biomarker sample type",
            BiomarkerName = "Biomarker name",
            BiomarkerThresholdValue = "Biomarker threshold value",
            BiomarkerUnits = "Biomarker units",
            Blood = "Blood biomarker",
            Urine = "Urine biomarker",
            Stool = "Stool biomarker",
            CerebrospinalFluid =" Cerebros-pinal fluid",
            WoundSurgeryBiopsy = "Wound surgery/ Biopsy",
            BAL = "BAL (protected resp. specimen)",
            SputumBronchialAspirate = "Sputum/ bronchial aspirate",
            OtherSpecimen = "Other specimen",
            Diabetes = "Diabetes mellitus type 1 or 2",
            AIDS = "AIDS/ HIV (only if last CD4 count < 500/mm)",
            Hematological = "Hematological or solid cancer/ recent chemotherapy (<3 months)",
            Stemcell = "Solid organ transplant",
            ChronicRenalDisease = "Chronic renal disease all stages",
            ActiveTB = "Active tuberculosis",
            GeneticDisorder = "Genetic disorder",
            CongenitalHeartDiseases = "Congenital heart diseases",
            LungDisease = "Chronic lung diseases (cystic fibrosis/COPD/bronchiectasis/asthma",
            Neutropenia = "Neutropenia (Low WBC)",
            HighDoseSteroids = "High dose steroids",
            Malnutrition = "Malnutrition",
            LongCovid = "Long COVID",
            LiverDisease = "End-stage liver disease, cirrhosis",
            Trauma = "Trauma",
            GastroenterologicalDisease = "Gastroenterological disease (IBS, coeliac disease)",
            NeurologicalDisease = "Chronic neurological conditions",
            Morbidities = "Number of underlying morbidities",
            AnyMorbidity = "Any underlying morbidity",
            MultMorbidity = "Multiple underlying morbidities",
            OrganismsNum = "Number of isolated organisms",
            DiagnosisNum = "Number of diagnosis",
            AntibioticsNum = "Number of antibiotics",
            OrganismName = "Name of organism",
            Resistance = "Type of Resistance",
            HAI_Patient = "Hospital acquired infection patient",
            AntibioticName = "Antibiotic name", 
            AntibioticClass="Antibiotic class",
            AntioticAwareClass= "Antibiotic class (WHO awar classification)",
            #Number = "Number of antibiotics",
            DoseUint = "Antibiotic dose (units) ",
            Dosage = "Antibiotic dosage",
            DoseFrequency = "Antibiotic dose frequency",
            Route = "Antibiotic route",
            Indication = "Antibiotic purpose (Indication)",
            Diagnosis="Diagnsosis", DiagnosisGroups="Diagnosis categories")

##=======================================================================================
print("Data cleaning done!!!")


#Save data 
write_excel_csv(merged_df, "EM_Adila_MetaData2025May17.csv")
write_excel_csv(merged_abx.df, "EM_Adila_MergedData_ABX2025May19.csv")
write_excel_csv(merged_diag.df, "EM_Adila_MergedData_Diagnosis2025May12.csv")

