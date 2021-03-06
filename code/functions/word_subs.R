# Word substitutions

# first word = original word
# second word = replacement
replace <-
  c(
    "\btp\\b" = "taxpayer",
    "\btps\\b" = "taxpayer",
    "\btaxpayers\\b" = "taxpayer",
    "\bexecutor\\b" = "taxpayer",
    "\bestate\\b" = "taxpayer",
    "\bmstp\\b" = "taxpayer",
    "\bmrtp\\b" = "taxpayer",
    "\bsetup\\b" = "ia",
    "\bpay\\b" = "payment",
    "\bPay\\b" = "Payment",
    "\bstat\\b" = "status",
    "\bbal\\b" = "balance",
    "\bItin_renewal\\b" = "renew_itin",
    "\bitin_renewal\\b" = "renew_itin",
    "\bfax number\\b" = "fax",
    "\bfax_number\\b" = "fax",
    "\b433d\\b" = "ia",
    #"\binstallment\\b" = "ia",
    #"\bagreement\\b" = "ia",
    "\b3rd\\b" = "poa",
    "\bparty\\b" = "poa",
    "\bProfessional\\b" = "poa",
    "\bthird\\b" = "poa",
    "\bAttorney\\b" = "poa",
    "\bpower\\b" = "poa",
    "\\bpreparer\\b" = "poa",
    "\\bpay\\b" = "payment",
    "\\bPay\\b" = "Payment",
    "\\bDDIA\\b" = "payment",
    "\\bddia\\b" = "payment",
    "\\bDirect\\b" = "payment",
    "\\bdebet\\b" = "payment",
    "\\bCP2604\\b" = "payment" ,
    "\\bCP2626\\b" = "payment",
    "\\bCP2645\\b" = "payment",
    "\\bCP2848\\b" = "payment",
    "\\bCP39\\b" = "payment",
    "\\bCP40\\b" = "payment",
    "\\bCP421\\b" = "payment",
    "\\bCP484\\b" = "payment",
    "\\bCP501\\b" = "payment",
    "\\bCP521\\b" = "payment",
    "\\bCP540\\b" = "payment",
    "\\bCP5976\\b" = "payment",
    "\\bCP60\\b" = "payment",
    "\\bCP681\\b" = "payment",
    "\\bCP89\\b" = "payment" ,
    "\\bpayments\\b" = "payment",
    "\\bstat\\b" = "status",
    "\\bbal\\b" = "balance",
    "\\bcp2000\\b" = "cp_2000",
    "\\bcp2000s\\b" = "cp_2000",
    #"\\b2000\\b" = "cp_2000",
    "\\bCP226\\b" = "lein",
    "\\bCP\\(\\+\\#\\)\\b" = "notice",
    "\\bCP\\-\\(\\+\\#\\)\\b" = "notice",
    "\\b\\#alone\\b" = "notice",
    "\\b\\#\\(\\+A,B,C...\\)\\b" = "notice" ,
    "\\bForm W-#\\b" = "form",
    "\\#\\#\\#\\#\\b" = "form",
    "F\\#\\#\\#\\#\\b" = "form" ,
    "\\bForm \\#\\#\\#\\#\\b" = "form",
    "\\bexam\\b" = "audit",
    "\\bcp_01\\b" = "audit",
    "\\bCP01\\b" = "audit",
    "\\bCP05\\b" = "audit",
    "\\bcp_05\\b" = "audit",
    "\\bowe\\b" =         "balance_due",
    "\\bowes\\b" =        "balance_due",
    "\\bowes\\b" =        "balance_due",
    "\\bowe\\b" =         "balance_due",
    "\\bowes\\b" =        "balance_due",
    "\\bowes\\b" =        "balance_due",
    "\\bCP13\\b" =        "balance_due",
    "\\bcp_14\\b" =       "balance_due",
    "\\bCP14\\b" =        "balance_due",
    "\\bcp_13\\b" =       "balance_due",
    "\\bCP160\\b" =       "balance_due",
    "\\bcp_160\\b" =      "balance_due",
    "\\bCP171\\b" =       "balance_due",
    "\\bcp_171\\b" =      "balance_due",
    "\\bCP23\\b" =        "balance_due",
    "\\bCP_23\\b" =       "balance_due",
    "\\bCP240\\b" =       "balance_due",
    "\\bCP_240\\b" =      "balance_due",
    "\\bBalance Due\\b" = "balance_due",
    "\\bbalance\\b" =     "balance_due",
    "\\bcurrent\\b" =     "balance_due",
    "\\bCP11\\b" =        "balance_due" ,
    "\\bCP_11\\b" =       "balance_due" ,
    "\\bCP11\\b" =        "balance_due" ,
    "\\bCP12\\b" =        "balance_due",
    "\\bCP_12\\b" =       "balance_due" ,
    "\\bCP_132\\b" =      "balance_due" ,
    "\\bCP132\\b" =       "balance_due" ,
    "\\bCare\\b" = "Healthcare",
    "\\bAct\\b" = "Healthcare" ,
    "\\bACA\\b" = "Healthcare",
    "\\bPremium\\b" = "APTC",
    "\\bpremium\\b" = "APTC",
    "\\btheft\\b" = "identity_theft"   ,
    "\\bTheft\\b" = "identity_theft",
    "\\bOIC\\b" = "offer_in_compromise" ,
    "\\boic\\b" = "offer_in_compromise"   ,
    "\\bWMAR\\b" = "amended_return",
    "\\bCP2057\\b" = "amended_return",
    "\\bCP_2057\\b" = "amended_return",
    "\\bCP3324C\\b" = "amended_return",
    "\\bCP_3324C\\b" = "amended_return",
    "\\bty\\b" = "tax_year",
    "\\bCorp\\b" = "business" ,
    "\\bS-corporation\\b" = "business",
    "\\bS-Corporation\\b" = "business",
    "\\bS-corp\\b" = "business",
    "\\bS Corp\\b" = "business",
    "\\bcorporation\\b" = "business",
    "\\bCEO\\b" = "business",
    "\\bceo\\b" = "business",
    "\\bpresident\\b" = "business" ,
    "\\bpartner\\b" = "business",
    "\\bpartnership\\b" = "business" ,
    "\\bCorporation\\b" = "business",
    "\\bcorp\\b" = "business",
    "\\bSecretary\\b" = "business",
    "\\bsecretary\\b" = "business",
    "\\badministrator\\b" = "business",
    "\\bcorporation\\b" = "business",
    "\\bC-Corp\\b" = "business",
    "\\bBMF\\b" = "business",
    "\\bbmf\\b" = "business",
    "\\bSole\\b Proprietor\\b" = "business",
    "\\bLLC\\b" = "business",
    "\\bllc\\b" = "business",
    "\\bCP148\\b" = "business",
    "\\bCP112\\b" = "business",
    "\\bCP136\\b" = "business",
    "\\bCP161\\b" = "business",
    "\\bCP162\\b" = "business",
    "\\bCP166\\b" = "business",
    "\\bCP1865C\\b" = "business",
    "\\bform 990\\b" = "business",
    "\\b990\\b" = "business",
    "\\bCP20\\b" = "business",
    "\\bCP237\\b" = "business",
    "\\bCP575\\b" = "business",
    "\\bCP193\\b" = "business",
    "\\bCP220\\b" = "business",
    "\\bCP235\\b" = "business",
    "\\bCP249\\b" = "business",
    "\\bCP259\\b" = "business",
    "\\bCP267\\b" = "business",
    "\\bCP2695\\b" = "business",
    "\\bCP276B\\b" = "business",
    "\\bcp96C\\b" = "business",
    "\\bform\\b 941\\b" = "business",
    "\\b941\\b" = "business",
    "\\bform 940\\b" = "business",
    "\\b940\\b" = "business",
    "\\bCP080\\b" = "business",
    "\\bCP80\\b" = "business",
    "\\bCP101\\b" = "business",
    "\\bForm\\b 944\\b" = "business",
    "\\bCP108\\b" = "business",
    "\\bForm\\b 941X\\b" = "business",
    "\\bForm\\b 940X\\b" = "business",
    "\\bcp128\\b" = "business",
    "\\b147C\\b" = "business",
    "\\bLetter\\b" = "business",
    "\\bEIN\\b letter\\b" = "business",
    "\\bEmployer\\b" = "business",
    "\\bESRP\\b" = "business",
    "\\bentity\\b" = "business",
    "\\b312C\\b" = "business",
    "\\bcp2802\\b" = "withholding" ,
    "\\bw4\\b" = "withholding",
    "\\btwe\\b" = "withholding",
    "\\btelephone\\b" = "phone",
    "\\bw-2s\\b" = "w-2",
    #"\\bs\\b \\bc\\b" = "social_security_number",
    "\\bzero\\b" = "0",
    "\\bone\\b" = "1",
    "\\btwo\\b" = "2",
    "\\bthree\\b" = "3",
    "\\bfour\\b" = "4",
    "\\bfive\\b" = "5",
    "\\bsix\\b" = "6",
    "\\bseven\\b" = "7",
    "\\beight\\b" = "8",
    "\\bnine\\b" = "9",
    "\\bdress\\b" = "address",
    "\\bcovid\\b \\b19\\b" = "covid_19"
    # "\\beleven\\b" = "11",
    # "\\btwelve\\b" = "12",
    # "\\bthirteen\\b" = "13",
    # "\\bfourteen\\b" = "14",
    # "\\bfifteen\\b" = "15",
    # "\\bsixteen\\b" = "16",
    # "\\bseventeen\\b" = "17",
    # "\\beighteen\\b" = "18",
    # "\\bnineteen\\b" = "19",
    # "\\bfifty\\b" = "50"

  )

bigrams <-
  c(  "\\btwenty\\b twenty" = "2020"
    , "\\btwenty\\b nineteen" = "2019"
    , "\\bsocial\\b \\bsecurity\\b" = "social_security_number"
    , "\\bpayment\\b \\bplan" = "payment_plan"
    , "\\breset\\b \\bpassword\\b" = "reset_password"
    , "\\btwenty\\b eighteen" = "2018"
    , "\\btax\\b \\breturn" = "tax_return"
    , "\\btax\\b \\brefund" = "tax_refund"
    , "\\bfederal\\b income" = "federal_income"
    , "\\bten\\b forty" = "1040"
    , "\\bten\\b ninety" = "1090"
    , "\\breceived\\b letter" = "received_letter"
    , "\\bbank\\b account" = "bank_account"
    , "\\bdirect\\b deposit" = "direct_deposit"
    , "\\bfile\\b taxes" = "file_taxes"
    , "\\binstallment\\b agreement" = "installment_agreement"
    , "\\bbalance\\b due" = "balance_due"
    , "\\breceived\\b notice" = "received_notice"
    , "\\bpayment\\b arrangement" = "payment_arrangement"
    , "\\bpower\\b attorney" = "power_attorney"
    , "\\bincome\\b tax" = "income_tax"
    , "\\bsmart\b card" = "smart_card"
    , "\\bcovid\\b 19" = "covid_19"
    , "\\bthird\\b party" = "third_party"
    , "\\bthird\\b parti" = "third_party"
    , "\\btax\\b year" = "tax_year"
    , "\\beip\\b 1" = "eip_1"
    , "\\bamend\\b return" = "amend_return"
    , "\\binjured\\b spouse" = "injured_spouse"
    , "\\binjure\\b spous" = "injured_spouse"
    , "\\bip\\b pin" = "ip_pin"
    , "\\bcorrect\\b address"= "correct_address"
    , "\\bform\\b 1095" = "form_1095"
    , "\\btax\\b year" = "tax_year"
    ,"\\bpaper\\b \\bcheck\\b"="paper_check"
    ,"\\bw\\b \\btwo\\b" = "w_2"
    ,"\\bw\\b \\b2\\b" = "w_2"
    # , "\\btwo\\b \\bthousand\\b" = "2000"
    # , "\\bthirty\\b \\bthree\\b" = "33"
    # , "\\btwo\\b \\bhundred\\b" = "200"
    # , "\\bthree\\b \\bhundred\\b" = "300"
    # , "\\bfive\\b \\bhundred\\b" = "500"
    # , "\\btwenty\\b \\bfive\\b" = "25"
    # , "\\bforty\\b \\bone\\b" = "41"
    )

trigrams<-
  c( "\\bsocial\\b \\bsecurity\\b \\bnumber" = "social_security_number"
    ,"\\binternal\\b revenue\\b \\bservice" = "internal_revenue_service"
    ,"\\bpower\\b \\bof\\b \\battorney" = "power_of_attorney"
    ,"\\btwo\\b \\bthousand\\b \\bnineteen" = "2019"
    ,"\\btwo\\b \\bthousand\\b \\beighteen" = "2018"
    ,"\\btwo\\b \\bthousand\\b \\bseventeen" = "2017"
    ,"\\bpower\\b \\battorney\\b \\bform" = "power_attorney_form"
    ,"\\binternal\\b \\brevenue\\b \\bservice" = "internal_revenue_service"
    ,"\\binternal\\b \\breference\\b \\bservice" = "internal_revenue_service"
    ,"\\btriple\\b \\bzero\\b" = "0 0 0"
    ,"\\beconom\\b \\bimpact\\b \\bpayment\\b" = "economic_income_payment"
    ,"\\bon\\b \\bthe\\b \\baccount\\b" = 	"on_the_account"
    ,"\\bdate\\b \\bof\\b \\bbirth\\b"="date_of_birth"
    ,"\\brecoveri\\b \\nrebat\\b \\bcredit\\b"="recovery_rebate_credit"
    ,"\\brecovery\\b \\nrebate\\b \\bcredit\\b"="recovery_rebate_credit"
    ,"\\bget\\b \\bmy\\b \\bpayment\\b" = "get_my_payment"
    ,"\\bverify\\b \\byour\\b \\bidentity\\b"="verify_your_identity"
    #,"\\btwo\\b \\bthousand\\b \\btwenty\\b" = "2020"
    # ,"\\bhundred\\b \\btwenty\\b \\bfive\\b" = "125"
    # ,"\\btwo\\b \\bhundred\\b \\btwenty\\b" = "220"
  )

quadgrams <-
  c(  "\\bset\\b up\\b a\\b payment" = "set_up_a_payment"
    , "\\btwenty\\b nineteen\\b tax\\b return" = "2019_tax_return"
    , "\\btwo\\b thousand\\b and\\b seventeen" = "2017"
    , "\\btwo\\b thousand\\b and\\b eighteen" = "2018"
    , "\\btwo\\b thousand\\b and\\b \\bnineteen" = "2019"
    , "\\btwo\\b \\bthousand\\b \\band\\b \\btwenty\\b" = "2020"
    , "\\btwo\\b thousand\\b and\\b fifteen" = "2015"
    # , "\\btwenty\\b \\beight\\b \\bforty\\b eight" = "2848"
    # , "\\beighty\\b \\beight\\b \\btwenty\\b one" = "8821"
    #, "\\beight\\b \\bfive\\b \\bfive\\b seven" = "8557"
    #, "\\bzero\\b \\bthree\\b \\bone\\b three" = "0313"
    #, "\\bseven\\b \\bzero\\b \\bzero\\b nine" = "7009"
    #, "zero zero nine six" = ""
  )

# Convert written numbers to numeric versions
# char_to_num_df <- data.frame(
#   str_num = str_replace_all(gsub("\\-", " ", english::words(10:9999)), "\\b", "\\\\b"),
#   num = as.character(10:9999),
#   stringsAsFactors = F)
# fwrite(char_to_num_df,"~/char_to_num_df.csv")
char_to_num_df <- fread("~/char_to_num_df.csv")
char_to_num <- as.character(char_to_num_df$num)
names(char_to_num) <- as.character(char_to_num_df$str_num)

