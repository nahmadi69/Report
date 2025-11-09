# # Required library
# library(stringr)
# library(readxl)
# 
# # Read your Excel file (replace 'data-6.xlsx' with your actual filename)
# data <- read_excel("C:/Users/StatDep/Downloads/data (6).xlsx", sheet = 1)
# 
# # Your provided product list (edit/expand this as needed)
# product_list <- c(
#   "AryoSeven", "Altebrel", "ReciGen", "CinnoVex", "Pectuna", "CinnaTropin", "Perfacto",
#   "Zytux", "Melitide", "AryoTrust", "Stivant", "PegaGen", "ZakAria", "CinnoMer",
#   "CinnoRA", "CinnaTropin", "Cinnal-f", "CinnoPar", "Cinnapoietin", "Temziva", "Aryotrust"
# )
# 
# # Suppose the first column is named 'sentences'
# sentences <- data[[1]]
# 
# # Function to extract product names mentioned in each sentence
# extract_products <- function(text, products) {
#   matches <- products[str_detect(tolower(text), tolower(products))]
#   if(length(matches) > 0) {
#     paste(unique(matches), collapse = ", ")
#   } else {
#     NA
#   }
# }
# 
# # Apply the function to all sentences
# data$ProductFound <- sapply(sentences, extract_products, products = product_list)
# 
# # View results
# head(data)
# 
# # Save to a new Excel file if desired
# # library(writexl)
# # write_xlsx(data, "products_with_names.xlsx")
# qq


# Required libraries
library(stringr)
library(readxl)

# Read your Excel file
data <- read_excel("D:/Naser/Shiny & Power BI/Editted data/MA approval.xlsx", sheet = "MA1")

# List of product names
product_list <- c(
  "AryoSeven", "Altebrel", "ReciGen", "CinnoVex", "Pectuna", "CinnaTropin", "Perfacto",
  "Zytux", "Melitide", "AryoTrust", "Stivant", "PegaGen", "ZakAria", "CinnoMer",
  "CinnoRA", "CinnaTropin", "Cinnal-f", "CinnoPar","ZakAria", "Cinnapoietin", "Temziva", "Aryotrust"
)

# Assume first column is sentences
sentences <- data[[1]]

# Extract product name(s)
extract_products <- function(text, products) {
  matches <- products[str_detect(tolower(text), tolower(products))]
  if(length(matches) > 0) paste(unique(matches), collapse = ", ") else NA
}

# Extract dosage(s) (e.g., "1 mg", "50 mg", "4000 mg", etc.)
extract_dosage <- function(text) {
  match <- str_extract(text, "\\b\\d+\\.?\\d*\\s?(mg|g|mcg|µg|ml|IU|units)\\b")
  if(!is.na(match)) match else NA
}

# Apply functions
data$ProductFound <- sapply(sentences, extract_products, products=product_list)
data$DosageFound <- sapply(sentences, extract_dosage)

# View the results
head(data)

# Optional: Save to Excel
library(writexl)
write_xlsx(data, "D:/Naser/Shiny & Power BI/Editted data/MA approval_test.xlsx")


# test --------------------------------------------------------------------


data_NA <- read_excel("D:/Naser/Shiny & Power BI/Editted data/MA approval.xlsx", sheet = "MA")
data_FR <- read_excel("D:/Naser/Shiny & Power BI/Editted data/MA approval.xlsx", sheet = "Sheet1")


data_j=data_NA %>% full_join(data_FR,by=c("Manufacturer","Product","Country"))
