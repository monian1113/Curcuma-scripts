# Curcuma-scripts
Scripts for analyzing Curcuma genomic data
### boxplot.R 
It was used for box plots and to detect differences between groups by pairwise t-tests, as well as for p-value correction by the bonferroni method.

### inverion-freq.R
Box plots were used for paired data and differences between groups were detected by paired t-tests.

### sweep-fst-pi.R
Selective sweep analysis based on fst-pi requires pi files and an fst file in the same format as the vcftools output.

### pic1.py
vcftools --freq ## Calculate the frequency for each locus
less out.freq.frq|sed 's/\t/-/;s/\t/-/;s/\t/-/;s/\t/-/;s/\t/,/g;s/-/\t/g' |grep -v nan > new.frq
Then the PIC is calculated using pic1.py.
python pic1.py new.frq pic.txt

### pic2.py
Used to calculate the mean value of the PIC in regions.
python pic.py region.bed pic.bed mean.pic.txt

### Manhattan.R
For visualization of Manhattan plots, the inversion will be highlighted.
