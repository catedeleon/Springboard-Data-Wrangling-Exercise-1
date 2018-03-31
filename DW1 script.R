library(dplyr)
library(tidyr)

refine_original <- read.csv("refine_original.csv")

# Clean company names using agrep

refine_original$company[agrep("philips", refine_original$company, ignore.case = TRUE)] <- "philips"
refine_original$company[agrep("fillips", refine_original$company, ignore.case = TRUE)] <- "philips"
refine_original$company[agrep("akzo", refine_original$company, ignore.case = TRUE)] <- "akzo"
refine_original$company[agrep("van houten", refine_original$company, ignore.case = TRUE)] <- "van houten"
refine_original$company[agrep("unilever", refine_original$company, ignore.case = TRUE)] <- "unilever" 
  
# Split Product.code...number variable into product_code and product_number

refine_original <- refine_original %>% separate(Product.code...number, c("product_code", "product_number")) 

# Create variable product_category based on product_code

refine_original <- refine_original %>% 
mutate(product_category = case_when(product_code == "p" ~ "smartphone",
                                      product_code == "v" ~ "tv",
                                      product_code == "x" ~ "laptop",
                                      product_code == "q" ~ "tablet"))

# Combine location variables into full_address

refine_original <- refine_original %>%
mutate(full_address = paste(refine_original$address, refine_original$city, refine_original$country, sep=", "))

# Create binary dummy columns for each company name and product category

refine_original <- refine_original %>% 

mutate(company_philips = ifelse(refine_original$company == "phillips", 1, 0),
company_akzo = ifelse(refine_original$company == "akzo", 1, 0),
company_van_houten = ifelse(refine_original$company == "van houten", 1, 0),
company_unilever = ifelse(refine_original$company == "unilever", 1, 0),

product_smartphone = ifelse(refine_original$product_category == "smartphone", 1, 0), 
product_tv = ifelse(refine_original$product_category == "tv", 1, 0),
product_laptop = ifelse(refine_original$product_category == "laptop", 1, 0), 
product_tablet = ifelse(refine_original$product_category == "tablet", 1, 0))


# Write clean .csv file

write.csv(refine_original, file = "refine_clean.csv")
