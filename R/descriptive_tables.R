char_data <- data.frame(
  Descriptives = c('Articles Included', 'Published Articles', 'Dissertations', 
                   'Experiments/Studies', 'Effec Sizes', 'Total Population', 
                   'University Students', 'Comunity', 'Mturk', "Prolific", 
                   'Location', 'United States', 'United Kingdome', 'Australia', 
                   'Canada', 'Germany', 'New-Zealand', 'Poland', 'Israel', 'Italy', 
                   'Spain', 'Taiwan', 'India', 'China', 'Belgum', 'Turkey', 
                   'Japan', 'Australia', 'Ireland', 'Scotland', 'South Korea', 
                   'Netherlands', 'Russia', 'Serbia', 'Population Unidentified (NA)',
                   'Sample Characteristics', 'Females(%)', 'Mean Age', 'Age Categories',
                   'Preschoolers(<5y)', 'School Age(5-12y)',' Adolecents(13-17y)', 'Adults(18-40y)',
                   'Older-Adults(<40)'),
  N           = c('294', '243', '27', '433', 'NA', '51 876', '37 816', '11 231', '2518', '311', "",
             '256 (87.07%)', '52 (12.00%)',' 32 (7.39%)',' 20 (4.62%)', '16 (3.70%)', 
             '11 (2.54%)','8 (1.85%) ','6 (1.39%) ','5 (1.15%) ', '4 (< 1%) ', 
             '3 (< 1%) ','2 (< 1%)', '2 (< 1%) ','2 (< 1%)',' 2 (< 1%)',' 2 (< 1%)',
             '1 (< 1%)','1 (< 1%)',' 1 (< 1%) ',' 1 (< 1%)','1 (< 1%) ','1 (< 1%)',
             '1 (< 1%)','5 (1.15%)', "", "", "", "", "", "", "", "", ""),
  
  Mean = c("", "", "", "", "", "", "", "", "","","","","","","","","","","","","",
           "","","","","","","","","","", "","","","","",
           '0.61 (0.73)', '19.98', "", '4.38 (0.45)', '7.14 (1.86)', 
           '16.6 (0.25)', '22.0 (4.12)', '69.0 (10.1)'),
  Median = c("", "", "", "", "", "", "", "", "","","","","","","","","","","","",
             "","","","","","","","","","","", "","","","","",
             '0.6', '19.70', "", '4.5', '6.08', '16.8', '20.4', '72.3')
             
             
)
 
names(char_data) <- c('Descriptives', 'N(%)', 'Mean', 'Median')           

ft <- flextable(char_data) %>% 
  bold(i  = c(6, 11, 36, 39), j = 1) %>% 
  bold(part = "header", bold = TRUE) %>% 
  theme_apa()
ft

design_data <- data.frame(
  Category = c('Original Event', "","","","","","","","","","","","","","", "",
               'Post-Event Information', "","","","","","","","",
               'Recall', "","","","","","","","","","","","","","","","",""),
               
  Variable = c("", 'Medium', "","","","","",
               'Materials', "","","","","", "","","",
               'Method', "","","","","","","","",
               'Test-Type', "","","","","","","","","","",
               'Control Item Type', "","","",
               'Test Items Mean (SD)', "",""),
  Subcategory = c("", 
                  'Visual', 
                  'Audiovisual', 
                  'Live', 
                  'Audio', 
                  'Text', 
                  'NA', 
                  'McCloskey & Zaragoza (1985) Slides',
                  '24 (TV-series)',
                  '“Eric the Electrician” Video',
                  'Flashpoint (TV-Series)',
                  'Rififi (Movie)',
                  'Loftus (1978) Slides',
                  'Okado & Stark (2005) Slides',
                  'Other Materials',
                  "",
                  'Narrative',
                  'Questionaire', 
                  'Interview', 
                  'Co-Witness', 
                  'Initial Test', 
                  'Mock Interegation', 
                  'Other Methods',
                  "",
                  "",
                  'Recognition',
                  'Cued Recall',
                  'Modified Test', 
                  'Source Monitoring',
                  'Free Recall',
                  'MMFR',
                  'Line-up Recognition',
                  'Total Recall',
                  'Interpolated Recall',
                  'Çolor Recognition',
                  'NA',
                  'Consistent', 
                  'Neutral',
                  'No Misinformation',
                  'NA',
                  'Control',
                  'Misled',
                  'Total'),
  N = c("", '193',' 164',' 52',' 12',' 12',' 3',' 45',' 25',' 15',' 13',' 9',' 9',' 8','200',
        "", '230',' 73',' 64', ' 24', '12', '5', '32', 
        "", "", '271', '117','33',' 27',' 19',' 5', '5',' 4', '1', '1', '2', '184', 
        '183', '143', '5', '7.21 (7.25)', '6.47 (6.51)', '21.5 (14.77)')
  
  
)

ft_design <- flextable(design_data) %>% 
  bold(i  = c(6, 11, 36, 39), j = 1) %>% 
  bold(part = "header", bold = TRUE) %>% 
  theme_apa()
ft_design

