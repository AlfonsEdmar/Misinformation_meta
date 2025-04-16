char_data <- data.frame(
  Descriptives = c('Articles Included', 'Published Articles', 'Book Chapters' , 'Dissertations', 
                   'Experiments/Studies', 'Effec Sizes', 'Total Population', 
                   'University Students', 'Comunity', 'Mturk', "Prolific", 
                   'Location', 'United States', 'United Kingdome', 'Australia', 
                   'Canada', 'Germany', 'New-Zealand', 'Poland', 'Italy', 'Israel', 
                   'Spain', 'Taiwan', 'India', 'China', 'Belgum', 'Turkey', 
                   'Japan', 'Australia', 'Ireland', 'Scotland', 'South Korea', 
                   'Netherlands', 'Russia', 'Serbia', 'Population Unidentified (NA)',
                   'Sample Characteristics', 'Females(%)', 'Mean Age', 'Age Categories',
                   'Preschoolers (<5y)', 'School Age (5-12y)',' Adolecents (13-17y)', 'Adults (18-40y)',
                   'Older-Adults (<40)'),
  N           = c('291', '202', '4','4', '428', '1756', '51 598', '37 793', '10 976', '2518', '311', "",
             '254 (87.07%)', '51 (12.00%)',' 31 (7.39%)',' 19 (4.62%)', '16 (3.70%)', 
             '11 (2.54%)','8 (1.85%) ','6 (1.39%) ','5 (1.15%) ',  
             '4 (< 1%) ','3 (< 1%)', '2 (< 1%) ','2 (< 1%)',' 2 (< 1%)',' 2 (< 1%)',
             '2 (< 1%)','1 (< 1%)',' 1 (< 1%) ',' 1 (< 1%)','1 (< 1%) ','1 (< 1%)',
             '1 (< 1%)', '1 (< 1%) ','5 (1.15%)', "", "", "", "", "34 (8%)", "36 (8%)", "3 (.1%)", "176 (41%)", "30 (7%)"),
  
  Mean = c("", "", "", "", "", "", "", "", "", "","","","","","","","","","","","","",
           "","","","","","","","","","", "","","","","",
           '0.61 (0.136)', '19.9', "", '4.38 (0.45)', '7.14 (1.86)', 
           '16.6 (0.25)', '22.0 (4.11)', '69.0 (10.2)'),
  Median = c("", "", "", "", "", "", "", "", "", "","","","","","","","","","","","",
             "","","","","","","","","","","", "","","","","",
             '0.59', '19.7', "", '4.5', '6.08', '16.8', '20.4', '72.6')
             
             
)
 
names(char_data) <- c('Descriptives', 'N(%)', 'Mean', 'Median')           

ft_char <- flextable(char_data) %>% 
  bold(i  = c(1, 5, 6, 7, 6, 12, 37, 40), j = 1) %>% 
  bold(part = "header", bold = TRUE) %>% 
  theme_apa()
ft_char

save_as_docx(ft_char, path = 'tables/table_characteristics.docx')


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
  N = c("", '193',' 160',' 52',' 12',' 12',' 2',' 45',' 25',' 15',' 13',' 9',' 9',' 8','319',
        "", '229',' 72',' 61', ' 22', '12', '5', '36', 
        "", "", '266', '116','33',' 27',' 17',' 5', '5',' 4', '1', '1', '2', 
        '183',  '182', '143', '4', '7.95 (7.36)', '7.14 (6.64)', '21.5 (14.6)')
  
  
)

ft_design <- flextable(design_data) %>% 
  bold(i  = c(6, 11, 36, 39), j = 1) %>% 
  bold(part = "header", bold = TRUE) %>% 
  theme_apa()
ft_design

save_as_docx(ft_design, path = 'tables/table_characteristics_design.docx')
