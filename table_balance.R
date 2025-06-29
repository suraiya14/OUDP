# assume your full data.frame is called `df` and has a column CVD_risk
# with values "yes" (340 rows) and "no" (3230 rows)

library(dplyr)
# Step 1: Read the CSV file
df <- read.csv("D:\\Research_Work\\research on new idae\\idea2\\Feature_Extraction\\SA_rev_comp_NSDUH_processed_all_corrected_v6_final.csv", header = TRUE, sep = ",")

set.seed(123)  # for reproducibility
df$CVD_risk
# 1. Split into the two classes
yes_df <- df %>% 
  filter(UD5OPIANY == 1)
nrow(yes_df)

no_df  <- df %>% 
  filter(UD5OPIANY == 0)
nrow(no_df)
# 2. Randomly sample 341 rows from the "no" set
no_down <- no_df %>% 
  sample_n(nrow(yes_df))
nrow(no_down)
# 3. Combine back into one balanced data.frame
balanced_df <- bind_rows(yes_df, no_down)

nrow(balanced_df)

# 4. (Optional) shuffle the rows so yes/no are mixed
#balanced_df <- balanced_df %>% 
  #slice_sample(n = n())

# Now balanced_df has 680 rows: 340 yes, 340 no
table(balanced_df$UD5OPIANY)


# Optional: Save updated dataframe to new CSV
write.csv(balanced_df, "D:\\Research_Work\\research on new idae\\idea2\\Feature_Extraction\\balanced_dataset.csv", row.names = FALSE)
