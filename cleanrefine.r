# Foundations of Data Science - Data Wrangling Exercise 1
# 2016-11-10, Matt Thomas

library(dplyr)

# 0.0: Load the data in RStudio
refinedata = read.csv("refine_original.csv")

# 0.1: convert data to tbl
refinedata_tbl <- dplyr::tbl_df(refinedata)

# 1.0: clean up brand names
# set a vector of brandnames we want to standardise on
brandnames<-c("philips", "akzo", "van houten", "unilever")
# calculate distance and choose closest
bndist<-refinedata_tbl[[1]] %>% adist(brandnames)
bnmindist <- apply(bndist, 1, which.min)
refinedata_tbl <- refinedata_tbl %>% dplyr::mutate(codedbrandname = brandnames[bnmindist])

# 2.0: Separate product code and number
# use tidyr::separate, keep original column for comparison
refinedata_tbl <- refinedata_tbl %>% tidyr::separate(Product.code...number,c("product_code","product_number"), "-", remove=FALSE)

# 3.0: Add product categories
# set a df of product categories  we want to join to (use product_code for subsequent join)
prodcats<-data_frame(product_code = c("p", "v", "x", "q"), product_cat = c("Smartphone", "TV", "Laptop", "Tablet"))
# Join back to tibble. Use left_join to keep source rows in case of missing data
refinedata_tbl <- refinedata_tbl %>% dplyr::left_join(prodcats)

# 4.0: Add full address for geocoding
# Use tidyr::unite to concatenate address fields, keep original columns for comparison.
refinedata_tbl <- refinedata_tbl %>% tidyr::unite(col=full_address, address, city, country, sep=", ", remove = FALSE)

# 5.0: Create dummy variables for company and product category
# Use model.matrix to create dummy company variables in a df
companydummy <- as.data.frame(with(refinedata_tbl, model.matrix(~ codedbrandname + 0)))
# Rename columns
names(companydummy) <- gsub("codedbrandname", "company_", names(companydummy), fixed = TRUE)
# Use model.matrix to create dummy product category variables in a df
prodcatdummy <- as.data.frame(with(refinedata_tbl, model.matrix(~ product_cat + 0)))
# Rename columns, also making them lowercase 
names(prodcatdummy) <- tolower(gsub("product_cat", "product_", names(prodcatdummy), fixed = TRUE))
# bind both to our tibble
refinedata_tbl <- refinedata_tbl%>% dplyr::bind_cols(companydummy, prodcatdummy)


# 6: Export the data to csv file 
refinedata_tbl %>% write.csv(file="refine_clean.csv")
