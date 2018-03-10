# Capture part between @ and . and after .
email <- capture(one_or_more(WRD)) %R% 
  "@" %R% capture(one_or_more(WRD)) %R% 
  DOT %R% capture(one_or_more(WRD))

# Check match hasn't changed
str_view(hero_contacts, pattern =  email)  

# Pull out match and captures
email_parts <- str_match(hero_contacts, pattern =  email)  

# Print email_parts
email_parts

# Save host
host <- email_parts[,3]
host

##################################
#https://stackoverflow.com/questions/201323/using-a-regular-expression-to-validate-an-email-address/201378#201378

contact = c("Call me at 555-555-0191", "123 Main St", "(555) 555 0191", "Phone: 555.555.0191 Mobile: 555.555.0192")

# View text containing phone numbers
contact

# Add capture() to get digit parts
phone_pattern <- capture(three_digits) %R% zero_or_more(separator) %R% 
           capture(three_digits) %R% zero_or_more(separator) %R%
           capture(four_digits)
           
# Pull out the parts with str_match()
phone_numbers <- str_match(contact, pattern=phone_pattern)

# Put them back together
str_c(
  "(",
  phone_numbers[,2],
  ") ",
  phone_numbers[,3],
  "-",
  phone_numbers[,4])
  
  
 ###################################
 
 narratives = c("19YOM-SHOULDER STRAIN-WAS TACKLED WHILE PLAYING FOOTBALL W/ FRIENDS ", 
"31 YOF FELL FROM TOILET HITITNG HEAD SUSTAINING A CHI ", "ANKLE STR. 82 YOM STRAINED ANKLE GETTING OUT OF BED ", 
"TRIPPED OVER CAT AND LANDED ON HARDWOOD FLOOR. LACERATION ELBOW, LEFT. 33 YOF*", 
"10YOM CUT THUMB ON METAL TRASH CAN DX AVULSION OF SKIN OF THUMB ", 
"53 YO F TRIPPED ON CARPET AT HOME. DX HIP CONTUSION ", "13 MOF TRYING TO STAND UP HOLDING ONTO BED FELL AND HIT FOREHEAD ON RADIATOR DX LACERATION", 
"14YR M PLAYING FOOTBALL; DX KNEE SPRAIN ", "55YOM RIDER OF A BICYCLE AND FELL OFF SUSTAINED A CONTUSION TO KNEE ", 
"5 YOM ROLLING ON FLOOR DOING A SOMERSAULT AND SUSTAINED A CERVICAL STRA IN"
)

# narratives has been pre-defined
narratives

# Add capture() to get age, unit and sex
pattern <- capture(optional(DGT) %R% DGT) %R%  
  optional(SPC) %R% capture(or("YO", "YR", "MO")) %R%
  optional(SPC) %R% capture(or("M", "F"))

# Pull out from narratives
str_match(narratives, pattern)

# Edit to capture just Y and M in units
pattern2 <- capture(optional(DGT) %R% DGT) %R%  
  optional(SPC) %R% capture(or("Y", "M")) %R% optional(or("O","R")) %R%
  optional(SPC) %R% capture(or("M", "F"))

# Check pattern
str_view(narratives, pattern2)

# Pull out pieces
str_match(narratives, pattern2)

####################################

# See names with three repeated letters
repeated_three_times <- capture(LOWER) %R% REF1 %R% REF1 
#<regex> ([:lower:])\1\1
str_view(boy_names, 
  pattern = repeated_three_times, 
  match = TRUE)

# See names with a pair of repeated letters
pair_of_repeated <- capture(repeated(LOWER,2)) %R% REF1
#<regex> ([[:lower:]]{2})\1
str_view(boy_names, 
  pattern = pair_of_repeated, 
  match = TRUE)

# See names with a pair that reverses
pair_that_reverses <- capture(LOWER) %R% capture(LOWER) %R% REF2 %R% REF1
#<regex> ([:lower:])([:lower:])\1\2\2\1
str_view(boy_names, 
  pattern = pair_that_reverses, 
  match = TRUE)

# See four letter palindrome names
four_letter_palindrome <- exactly(pair_that_reverses)
#<regex> ^([:lower:])([:lower:])\2\1$
str_view(boy_names, 
  pattern = four_letter_palindrome, 
  match = TRUE)

# See six letter palindrome names
six_letter_palindrome <- exactly(capture(LOWER) %R% capture(LOWER) %R% capture(LOWER) %R%  REF3 %R% REF2 %R% REF1)
#<regex> ^([:lower:])([:lower:])([:lower:])\3\2\1$
str_view(boy_names, 
  pattern = six_letter_palindrome, 
  match = TRUE)

