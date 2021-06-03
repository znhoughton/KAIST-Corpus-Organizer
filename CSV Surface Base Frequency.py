#read csv and calculate: base frequency (걸 + 걸리) surface frequency exact match (걸리다/걸려요/등), surface frequency morpheme (걸리)

import csv
import re
import sys
import sys
import csv
#passive csv should be organized as follows: col1 = active morpheme, col2 = passive morpheme, col3 = specific conjugation

with open("D:/Corpora/Corpora Code/Exact Match Only.csv", 'r', encoding = 'euc_kr', errors='ignore') as exact_match: #open the csv made from our corpus organizer
    with open('D:/Corpora/Corpora Code/Root Morpheme Only.csv', 'r', encoding = 'euc_kr', errors='ignore') as morphemes_only: #open the morpheme only csv made from our corpus organizer
        with open ('D:/Corpora/Corpora Code/List of Passives.csv', 'r', encoding = 'euc_kr', errors='ignore') as passive_list: #open the list of words we wish to calculate the surface and base frequencies for
            with open('D:/Corpora/Corpora Code/Surface and Base Frequencies of Korean Passives.csv', 'w', encoding = 'euc_kr', errors='ignore') as output: #create an output file
                mycsvwriter = csv.writer(output, delimiter=',', lineterminator='\n') #create csv.writer
                mycsvwriter.writerow(["Number", "Subnumber", "Active Morpheme", "Passive Morpheme", "Specific Word", "Base Frequency", "Surface Frequency of the Passive Morpheme", "Surface Frequency of the Exact Word"])
                mycsvreader = csv.reader(passive_list, delimiter = ',') #create csv reader
                exactmatch = exact_match.read() #read our exact match input file
                morphemes = morphemes_only.read() #read our morphemes only input file
                for line in mycsvreader:
                    number = line[0] #number of our stimulus 
                    subnumber = line[1] #subnumber of our stimulus
                    active_morpheme = line[2] #active morpheme of our stimulus
                    passive_morpheme = line[3] #passive morpheme of our stimulus
                    specific_word = line[4] #exact match of our item 
                    
                    ##e.g., number = 1; subnumber = 2; active_morpheme = 걸; passive morpheme = 걸리; specific word = 걸린다
                    
                    active_morpheme_count = sum(1 for _ in re.finditer(r'\b%s\b' % re.escape(active_morpheme), morphemes)) #count all the morphemes that match our active morpheme EXACTLY
                    passive_morpheme_count = sum(1 for _ in re.finditer(r'\b%s\b' % re.escape(passive_morpheme), morphemes)) #count all the morphemes that match our passive morpheme EXACTLY
                    specific_word_count = sum(1 for _ in re.finditer(r'\b%s\b' % re.escape(specific_word), exactmatch)) #count all the morphemes that match our word EXACTLY
                    base_frequency = int(active_morpheme_count) + int(passive_morpheme_count) #base frequencies is the sum of all the active morphemes and all the passive morphemes
                    mycsvwriter.writerow([number, subnumber, active_morpheme, passive_morpheme, specific_word, base_frequency, passive_morpheme_count, specific_word_count]) #write these calculations to a csv
                
