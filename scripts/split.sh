#!/bin/bash

# create output directory if missing
mkdir -p output

# split by Gender
awk -F "\t" 'NR==1; NR>1{s=0; for (i=2; i<=NF; i++) s+=$i; if ($1 == "Female") {print} }' $1 > output/Gender_female.tsv
echo Wrote $(cat output/Gender_female.tsv | wc -l) lines to output/Gender_female.tsv
awk -F "\t" 'NR==1; NR>1{s=0; for (i=2; i<=NF; i++) s+=$i; if ($1 == "Male") {print} }' $1 > output/Gender_male.tsv
echo Wrote $(cat output/Gender_male.tsv | wc -l) lines to output/Gender_male.tsv

# split by weightclass
WC=(under_weight normal overweight obese)
for i in "${!WC[@]}"; do 
  outfile="output/WeightClass_${WC[$i]}.tsv"
  echo "Gender	Age	Height	Weight	FHO	FAVC	FCVC	NCP	CAEC	SMOKE	CH2O	SCC	FAF	TUE	CALC	MTRANS	WeightClass" > $outfile
  idx=$(echo $((i+1)))
  awk -F "\t" -v idx=$idx '$17~idx{print}' $1 >> $outfile
  echo Wrote $(cat $outfile | wc -l) lines to $outfile
done





