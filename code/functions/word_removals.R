# Word Removals

# remove stop_words - there are three different lists of stopwords -
# 1. tm::stopwords()
# 2. tidytext::stop_words
# 3. custom made list

# List #1
stopwords1 <- tibble(word = tm::stopwords())

# List #2 is the first "anti_join(stop_words)"

# List #3
custom_stopwords <- tibble(word = c("the", "a", "an", "this", "these", "that",
                                    "those","i", "me", "my", "mine", "we", "us",
                                    "our", "ours", "you", "your", "yours", "he",
                                    "him", "his", "she", "her", "hers", "they",
                                    "them", "their", "theirs", "am", "is", "are",
                                    "was", "were", "have", "been", "do", "does",
                                    "did","and", "also", "as", "since", "because",
                                    "usually", "said", "asked", "explained",
                                    "told", "advised", "stated", "called",
                                    "informed", "like", "want", "know",
                                    "research", "researched", "transferred",
                                    "transfer",  "access", "accessed",
                                    "individual", "employee", "csr", "caller",
                                    "assistor", "taxpayer", "question",
                                    "disclosure", "account","ac", "t", "dv",
                                    "tppi","DV", "TPPI", "CALENDAR_YEAR",
                                    "calendar_year", "dv", "tppi",
                                    "status_22", "tp", "tps", "irs", "tp's",
                                    "daughter", "mr.", "ms.", "hctx", "cr", "csr",
                                    "mr", "ms", "mrs","wife", "husband", "son",
                                    "grandson", "granddaughter",
                                    "txpn", "said", "advised", "IDRS", "transfer",
                                    "'", "yeah","alright","bye","ma'am", "sir","uh",
                                    "wait","gonna","happen","chang","time","talk",
                                    "lot","pick","bottom","run","pick", "okay","thank",
                                    "just", "ahead", "hello","mister","jeez",
                                    
                                    #NAMES
                                    "murphy","cindy","ryan","moore"
)
)
