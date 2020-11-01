#Credit to Ethan for assisting me with the code for #6. 

#1
justice <- c("Clarence Thomas", "Ruth Bader Ginsberg", "Stephen Breyer", 
             "John Roberts", "Samuel Alito", "Sonia Sotomayer", "Elena Kagan",
             "Neil Gorsuch", "Brett Kavanaugh")
state <- c("GA", "NY", "MA", "MD", "NJ", "NY", "MA", "CO", "MD")
position <- c("Assoc Justice", "Assoc Justice", "Assoc Justice", "Chief Justice",
              "Assoc Justice", "Assoc Justice", "Assoc Justice", "Assoc Justice",
              "Assoc Justice")
replacing <- c("Thurgood Marshall", "Byron White", "Harry Blackmun", 
               "William Rehnquist", "Sandra Day O'Connor", "David Souter",
               "John Paul Stevens", "Antonin Scalia", "Anthony Kennedy")
yearconfirmed <- c("1991", "1993", "1994", "2005", "2006", "2009", "2010",
                   "2017", "2018")
senatevote <- c("52-48", "96-3", "87-9", "78-22", "58-42", "68-31", "63-37",
                "54-45", "50-48")
nominated_by <- c("H.W. Bush", "Clinton", "Clinton", "W. Bush", "W. Bush", 
                  "Obama", "Obama", "Trump", "Trump")
justices_info <- data.frame(justice, state, position, replacing, yearconfirmed,
                            senatevote, nominated_by)
view(justices_info)

#download and view data for #2-3
# ms: please show how you loaded the data
# ms: also you want to see whether the justiceNames are all the same after the merge
table(justices$justiceName)
table(SCDB_2020_01_justiceCentered_Citation$justiceName)

#3 mnerge data after checking variable simularity
merged_table <- inner_join(justices, SCDB_2020_01_justiceCentered_Citation, 
                           by = "justiceName")

#ms: consider merging by both justiceName and term so you don't have to denote term by term.x
merged_table <- inner_join(scotus, justices, 
                           by = c("justiceName", "term"))

#####

merged_table

#attempted to filter for #4
view(filter(merged_table, post_mn != NA)) 
#tried to filter based on those values are not equal to NA, so I would only get
#those instances where a Martin Quinn Score exists. 
# ms: the above code didn't work for me. it should be this?:
# merged_table <- View(filter(merged_table, !is.na(post_mn)))
# you need to do this so code in #5 would work

#used complete.cases instead to observe only those with complete data 
#(i.e. only those with Martin Quinn Scores)
complete.cases <- (merged_table)
# ms: this isn't doing what you think it's doing...
# I would check out this page on the complete.cases() function: https://statisticsglobe.com/complete-cases-in-r-example/

#5 
merged_table %>%
  group_by(term.x) %>%
  summarise(mean = mean(post_mn, na.rm = TRUE), n = n())

#6
merged_table <- mutate(merged_table,
        decisionDirection = case_when(
        decisionDirection == 3 ~ 0, #Re-code for conservative
        decisionDirection == 1 ~ 1, #Re-code for Liberal 
        decisionDirection == 2 ~ -1)) #Re-code for unspecified
#I tried to do this with piping (e.g. merged_table %>% ...) but it wouldn't apply
#to the table. 

# ms: this works:
merged_table <- merged_table %>%
  mutate(decisionDirection = case_when(
                         decisionDirection == 3 ~ 0,
                         decisionDirection == 1 ~ 1,
                         decisionDirection == 2 ~ -1))

####


merged_table %>%
  group_by(term.x) %>%
  summarise(mean = mean(decisionDirection, na.rm = TRUE), n = n())

# ms: where are the MQ scores? calculate the mean for post_mn as well:

merged_table %>%
  group_by(term) %>%
  summarise(mean = mean(decisionDirection, na.rm = TRUE), n = n(),
            mean = mean(post_mn, na.rm = TRUE), n = n())

```

#The two values are now in the same direction and similar with one another. 
