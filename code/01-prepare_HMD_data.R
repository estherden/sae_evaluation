## Esther Denecke
# Prepare HMD data: one format and all together.

## Prerequisites to this code:
# HMD data: From working directory there should be paths
#           data/HMD/deaths/Deaths_1x1
#           data/HMD/exposures/Exposures_1x1
#           data/HMD/deaths/Deaths_1x10
#           data/HMD/exposures/Exposures_1x10
# Each of these should contain textfiles with data for each country in the 
# HMD. These will be read-in in the course of executing this script.

## prep ------------------------------------------------------------------------

library(checkmate) # checking code
library(data.table) # data wrangling
library(here) # paths

# date: this will be used to save the output of this script.
today = format(Sys.Date(), format = "%Y%m%d")

## source ----------------------------------------------------------------------

source(here("code", "00-functions.R"))
source(here("code", "00-functions_for_testing.R"))

## read in data ----------------------------------------------------------------

path_Dx = here("data", "HMD", "deaths", "Deaths_1x1")
path_Nx = here("data", "HMD", "exposures", "Exposures_1x1")
path_Dx_1x10 = here("data", "HMD", "deaths", "Deaths_1x10")
path_Nx_1x10 = here("data", "HMD", "exposures", "Exposures_1x10")

Dx = readBindHmdZips(path_Dx, type = "Dx")
Nx = readBindHmdZips(path_Nx, type = "Nx")
Dx1x10 = readBindHmdZips(path_Dx_1x10, type = "Dx")
Nx1x10 = readBindHmdZips(path_Nx_1x10, type = "Nx")

# clean-up
rm(list = c("path_Dx", "path_Dx_1x10", "path_Nx", "path_Nx_1x10"))

## remove NA's -----------------------------------------------------------------

# NA's are coded as . -> remove
Dx = Dx[male != ".", ]
Nx = Nx[male != ".", ]
Dx1x10 = Dx1x10[male != ".", ]
Nx1x10 = Nx1x10[male != ".", ]

## convert classes -------------------------------------------------------------

# rename age column for testing
setnames(Dx, 
         old = c("age", "female", "male", "total"), 
         new = c("age_old", "female_old", "male_old", "total_old"))
setnames(Nx, 
         old = c("age", "female", "male", "total"), 
         new = c("age_old", "female_old", "male_old", "total_old"))
setnames(Dx1x10, 
         old = c("age", "female", "male", "total"), 
         new = c("age_old", "female_old", "male_old", "total_old"))
setnames(Nx1x10, 
         old = c("age", "female", "male", "total"), 
         new = c("age_old", "female_old", "male_old", "total_old"))

# change classes
Dx[, ':='(female = as.numeric(female_old),
          male = as.numeric(male_old),
          total = as.numeric(total_old))]

# Visual inspection for 'testing':
print(Dx[, .(female, female_old)])

Dx1x10[, ':='(female = as.numeric(female_old),
              male = as.numeric(male_old),
              total = as.numeric(total_old))]
print(Dx1x10[, .(female, female_old)])

Nx[, ':='(female = as.numeric(female_old),
          male = as.numeric(male_old),
          total = as.numeric(total_old))]
print(Nx[, .(female, female_old)])

Nx1x10[, ':='(female = as.numeric(female_old),
              male = as.numeric(male_old),
              total = as.numeric(total_old))]
print(Nx1x10[, .(female, female_old)])


Dx[, age := as.numeric(unlist(strsplit(age_old, "+", fixed = TRUE)))]
Nx[, age := as.numeric(unlist(strsplit(age_old, "+", fixed = TRUE)))]
Dx1x10[, age := as.numeric(unlist(strsplit(age_old, "+", fixed = TRUE)))]
Nx1x10[, age := as.numeric(unlist(strsplit(age_old, "+", fixed = TRUE)))]

# tests
testConversion(data = Dx, col1 = "age_old", col2 = "age", 
               num = 4854, uniqueValues = 111)
testConversion(data = Nx, col1 = "age_old", col2 = "age", 
               num = 4854, uniqueValues = 111)
testConversion(data = Dx1x10, col1 = "age_old", col2 = "age", 
               num = 503, uniqueValues = 111)
testConversion(data = Nx1x10, col1 = "age_old", col2 = "age", 
               num = 503, uniqueValues = 111)

# clean-up
Dx[, ':='(age_old = NULL, female_old = NULL, male_old = NULL, total_old = NULL)]
Nx[, ':='(age_old = NULL, female_old = NULL, male_old = NULL, total_old = NULL)]
Dx1x10[, ':='(age_old = NULL, female_old = NULL, male_old = NULL, total_old = NULL)]
Nx1x10[, ':='(age_old = NULL, female_old = NULL, male_old = NULL, total_old = NULL)]

# rename columns to something more meaningful
data.table::setnames(x = Dx,
                     old = c("female", "male", "total"),
                     new = c("Dx_female", "Dx_male", "Dx_total"))
data.table::setnames(x = Dx1x10,
                     old = c("female", "male", "total"),
                     new = c("Dx_female", "Dx_male", "Dx_total"))

data.table::setnames(x = Nx,
                     old = c("female", "male", "total"),
                     new = c("Nx_female", "Nx_male", "Nx_total"))
data.table::setnames(x = Nx1x10,
                     old = c("female", "male", "total"),
                     new = c("Nx_female", "Nx_male", "Nx_total"))

## merge Dx and Nx -------------------------------------------------------------

dat = merge.data.table(x = Dx, y = Nx, by = c("year", "age", "region"))
assertDataTable(dat, nrows = nrow(Dx), any.missing = FALSE)

dat1x10 = merge.data.table(x = Dx1x10, y = Nx1x10, by = c("year", "age", "region"))
assertDataTable(dat1x10, nrows = nrow(Dx1x10), any.missing = FALSE)


## change last age interval ----------------------------------------------------

# last age interval should be 100+ instead of 110+
tmp = dat[age >= 100, .(Dx_female = sum(Dx_female),
                        Dx_male = sum(Dx_male),
                        Dx_total = sum(Dx_total),
                        Nx_female = sum(Nx_female),
                        Nx_male = sum(Nx_male),
                        Nx_total = sum(Nx_total)), 
          by = .(year, region)]
tmp[, age := 100]

assertDataTable(x = tmp, nrows = nrow(unique(dat[, .(year, region)])), any.missing = FALSE)


tmp1x10 = dat1x10[age >= 100, .(Dx_female = sum(Dx_female),
                                Dx_male = sum(Dx_male),
                                Dx_total = sum(Dx_total),
                                Nx_female = sum(Nx_female),
                                Nx_male = sum(Nx_male),
                                Nx_total = sum(Nx_total)), 
                  by = .(year, region)]
tmp1x10[, age := 100]

assertDataTable(x = tmp1x10, nrows = nrow(unique(dat1x10[, .(year, region)])), 
                any.missing = FALSE)


# remove ages >= 100 in dat and then add tmp as 100+
dat = dat[age <= 99, ]
dat2 = rbindlist(l = list(dat, tmp), use.names = TRUE)

assertDataTable(x = dat2, nrows = nrow(dat) + nrow(tmp))
assertNumeric(x = unique(dat2[, age]), lower = 0, upper = 100,
              len = 101, any.missing = FALSE, unique = TRUE)

# clean-up
rm(dat)

dat1x10 = dat1x10[age <= 99, ]
dat1x10_2 = rbindlist(l = list(dat1x10, tmp1x10), use.names = TRUE)

assertDataTable(x = dat1x10_2, nrows = nrow(dat1x10) + nrow(tmp1x10), 
                any.missing = FALSE)
assertNumeric(x = unique(dat1x10_2[, age]), lower = 0, upper = 100, 
              len = 101, any.missing = FALSE, unique = TRUE)

# clean-up
rm(dat1x10)

## reformat to long format -----------------------------------------------------

# remove total and keep only male and female

dat_male = dat2[, .(year, age, region, Dx_male, Nx_male)]
dat_male[, sex := "male"]
data.table::setnames(dat_male,
                     old = c("Dx_male", "Nx_male"),
                     new = c("Dx", "Nx"))

dat_female = dat2[, .(year, age, region, Dx_female, Nx_female)]
dat_female[, sex := "female"]
data.table::setnames(dat_female,
                     old = c("Dx_female", "Nx_female"),
                     new = c("Dx", "Nx"))

dat = data.table::rbindlist(l = list(dat_male, dat_female), use.names = TRUE)

assertDataTable(x = dat, nrows = nrow(dat_male) + nrow(dat_female), any.missing = FALSE)

rm(dat2)

dat1x10_male = dat1x10_2[, .(year, age, region, Dx_male, Nx_male)]
dat1x10_male[, sex := "male"]
data.table::setnames(dat1x10_male,
                     old = c("Dx_male", "Nx_male"),
                     new = c("Dx", "Nx"))

dat1x10_female = dat1x10_2[, .(year, age, region, Dx_female, Nx_female)]
dat1x10_female[, sex := "female"]
data.table::setnames(dat1x10_female,
                     old = c("Dx_female", "Nx_female"),
                     new = c("Dx", "Nx"))

dat1x10 = data.table::rbindlist(l = list(dat1x10_male, dat1x10_female), 
                                use.names = TRUE)

assertDataTable(x = dat1x10, nrows = nrow(dat1x10_female) + nrow(dat1x10_male),
                any.missing = FALSE)

rm(dat1x10_2)

## add some helpful columns ----------------------------------------------------

# add mx column
dat[, mx := Dx / Nx]
dat[, logmx := log(mx)]

dat1x10[, mx := Dx / Nx]
dat1x10[, logmx := log(mx)]

# add column year_region_sex as unique identifier
dat[, year_region_sex := paste0(year, "_", region, "_", sex)]

dat1x10[, year_min := sapply(strsplit(dat1x10[, year], "-"), "[", 1)]
dat1x10[, year_max := sapply(strsplit(dat1x10[, year], "-"), "[", 2)]

dat1x10[, year_region_sex := paste0(year_min, "_", region, "_", sex)]

testConversion(data = dat1x10, col1 = "year", col2 = "year_min", 
               num = NULL, uniqueValues = 57)

testConversion(data = dat1x10, col1 = "year", col2 = "year_max", 
               num = NULL, uniqueValues = 57)

# reorder rows
dat = dat[order(sex, region, year, age), ]
dat1x10 = dat1x10[order(sex, region, year_min, age), ]


# clean-up
rm(list = c("Dx", "Nx","tmp", "dat_male", "dat_female",
            "dat1x10_male", "dat1x10_female", "Dx1x10", "Nx1x10",
            "tmp1x10"))


## save files ------------------------------------------------------------------

saveRDS(dat, here("data", "dataProcessed", 
                  paste0("01_HMD_data_1x1_prepared_", today, ".rds")))
saveRDS(dat1x10, here("data", "dataProcessed", 
                      paste0("01_HMD_data_1x10_prepared_", today, ".rds")))
