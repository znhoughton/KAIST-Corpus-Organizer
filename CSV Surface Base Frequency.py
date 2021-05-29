#read csv and calculate: base frequency (걸 + 걸리) surface frequency exact match (걸리다/걸려요/등), surface frequency morpheme (걸리)

import csv
import re
import sys
import sys
import csv
#passive csv should be organized as follows: col1 = active morpheme, col2 = passive morpheme, col3 = specific conjugation

with open("D:/Corpora/Corpora Code/Exact Match Only.csv", 'r', encoding = 'euc_kr', errors='ignore') as exact_match:
    with open('D:/Corpora/Corpora Code/Root Morpheme Only.csv', 'r', encoding = 'euc_kr', errors='ignore') as morphemes_only:
        with open ('D:/Corpora/Corpora Code/List of Passives.csv', 'r', encoding = 'euc_kr', errors='ignore') as passive_list:
            with open('D:/Corpora/Corpora Code/Surface and Base Frequencies of Korean Passives.csv', 'w', encoding = 'euc_kr', errors='ignore') as output:
                mycsvwriter = csv.writer(output, delimiter=',', lineterminator='\n')
                mycsvwriter.writerow(["Number", "Subnumber", "Active Morpheme", "Passive Morpheme", "Specific Word", "Base Frequency", "Surface Frequency of the Passive Morpheme", "Surface Frequency of the Exact Word"])
                mycsvreader = csv.reader(passive_list, delimiter = ',')
                exactmatch = exact_match.read()
                morphemes = morphemes_only.read()
                for line in mycsvreader:
                    number = line[0]
                    subnumber = line[1]
                    active_morpheme = line[2]
                    passive_morpheme = line[3]
                    specific_word = line[4]
                    active_morpheme_count = sum(1 for _ in re.finditer(r'\b%s\b' % re.escape(active_morpheme), morphemes))
                    passive_morpheme_count = sum(1 for _ in re.finditer(r'\b%s\b' % re.escape(passive_morpheme), morphemes))
                    specific_word_count = sum(1 for _ in re.finditer(r'\b%s\b' % re.escape(specific_word), exactmatch))
                    base_frequency = int(active_morpheme_count) + int(passive_morpheme_count)
                    mycsvwriter.writerow([number, subnumber, active_morpheme, passive_morpheme, specific_word, base_frequency, passive_morpheme_count, specific_word_count])
                
