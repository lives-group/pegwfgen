#! /bin/sh

:'
echo "now runnig 10k";
(time raco  cover  experiment/ewf-10k.rkt  peg-gen.rkt) &> TestData/logout-10k.txt
if [ -d coverage ]
  then 
   mv coverage TestData/coverarage-10k 
fi

echo "now runnig 50k";

(time raco  cover  experiment/ewf-50k.rkt  peg-gen.rkt) &> TestData/logout-50k.txt
if [ -d coverage ]  
  then 
   mv coverage TestData/coverarage-50k
fi
'
echo "now runnig 100k";

(time raco  cover  experiment/ewf-100k.rkt  peg-gen.rkt) &> TestData/logout-100k.txt
if [ -d coverage ]  
  then 
   mv coverage TestData/coverarage-100k
fi


echo "Finished";
