#!/bin/bash 

rm -rf /aqua/dtolley/UCODE_Linear_Uncert/ILR

mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR
mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_1
mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_2
mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_3
mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4
mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_5

mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_1/Runner1
mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_1/Runner2
mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_1/Runner3
mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_1/Runner4
mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_1/Runner5
mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_1/Runner6
mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_1/Runner7 

mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_2/Runner1
mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_2/Runner2
mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_2/Runner3
mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_2/Runner4
mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_2/Runner5
mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_2/Runner6
mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_2/Runner7

mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_3/Runner1
mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_3/Runner2
mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_3/Runner3
mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_3/Runner4
mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_3/Runner5
mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_3/Runner6
mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_3/Runner7

mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4/Runner1
mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4/Runner2
mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4/Runner3
mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4/Runner4
mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4/Runner5
mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4/Runner6
mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4/Runner7

mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_5/Runner1
mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_5/Runner2
mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_5/Runner3
mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_5/Runner4
mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_5/Runner5
mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_5/Runner6
mkdir /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_5/Runner7

cp Calibration_1/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_1
cp Calibration_2/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_2
cp Calibration_3/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_3
cp Calibration_4/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4
cp Calibration_5/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_5

cp Calibration_1/UCODE_Output_Files/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_1
cp Calibration_2/UCODE_Output_Files/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_2
cp Calibration_3/UCODE_Output_Files/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_3
cp Calibration_4/UCODE_Output_Files/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4
cp Calibration_5/UCODE_Output_Files/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_5

cp SVIHM_prediction_1.in /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_1
cp SVIHM_prediction_2.in /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_2
cp SVIHM_prediction_3.in /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_3
cp SVIHM_prediction_4.in /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4
cp SVIHM_prediction_5.in /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_5

cp ../UCODE/UCODE_Input_Files/SVIHM_Cal_1.corfac /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_1
cp ../UCODE/UCODE_Input_Files/SVIHM_Cal_2.corfac /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_2
cp ../UCODE/UCODE_Input_Files/SVIHM_Cal_3.corfac /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_3
cp ../UCODE/UCODE_Input_Files/SVIHM_Cal_4.corfac /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4
cp ../UCODE/UCODE_Input_Files/SVIHM_Cal_5.corfac /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_5      

cp Calibration_1/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_1
cp Calibration_2/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_2
cp Calibration_3/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_3
cp Calibration_4/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4
cp Calibration_5/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_5


cp Calibration_1/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_1/Runner1
cp Calibration_1/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_1/Runner2
cp Calibration_1/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_1/Runner3
cp Calibration_1/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_1/Runner4
cp Calibration_1/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_1/Runner5
cp Calibration_1/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_1/Runner6
cp Calibration_1/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_1/Runner7

cp Calibration_2/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_2/Runner1
cp Calibration_2/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_2/Runner2
cp Calibration_2/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_2/Runner3
cp Calibration_2/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_2/Runner4
cp Calibration_2/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_2/Runner5
cp Calibration_2/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_2/Runner6
cp Calibration_2/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_2/Runner7

cp Calibration_3/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_3/Runner1
cp Calibration_3/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_3/Runner2
cp Calibration_3/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_3/Runner3
cp Calibration_3/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_3/Runner4
cp Calibration_3/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_3/Runner5
cp Calibration_3/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_3/Runner6
cp Calibration_3/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_3/Runner7

cp Calibration_4/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4/Runner1
cp Calibration_4/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4/Runner2
cp Calibration_4/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4/Runner3
cp Calibration_4/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4/Runner4
cp Calibration_4/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4/Runner5
cp Calibration_4/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4/Runner6
cp Calibration_4/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4/Runner7

cp Calibration_5/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_5/Runner1
cp Calibration_5/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_5/Runner2
cp Calibration_5/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_5/Runner3
cp Calibration_5/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_5/Runner4
cp Calibration_5/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_5/Runner5
cp Calibration_5/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_5/Runner6
cp Calibration_5/* /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_5/Runner7

sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_1/general_inputs.txt                             #replace basecase flag with ILR flag for SWBM
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_1/SVIHM_SWBM_General_Inputs.jtf                   #replace basecase flag with ILR flag for SWBM
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_2/general_inputs.txt                             #replace basecase flag with ILR flag for SWBM       
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_2/SVIHM_SWBM_General_Inputs.jtf                   #replace basecase flag with ILR flag for SWBM      
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_3/general_inputs.txt                             #replace basecase flag with ILR flag for SWBM       
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_3/SVIHM_SWBM_General_Inputs.jtf                   #replace basecase flag with ILR flag for SWBM      
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4/general_inputs.txt                             #replace basecase flag with ILR flag for SWBM       
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4/SVIHM_SWBM_General_Inputs.jtf
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_5/general_inputs.txt                               #replace basecase flag with ILR flag for SWBM      
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_5/SVIHM_SWBM_General_Inputs.jtf

sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_1/Runner1/general_inputs.txt            
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_1/Runner1/SVIHM_SWBM_General_Inputs.jtf 
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_1/Runner2/general_inputs.txt            
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_1/Runner2/SVIHM_SWBM_General_Inputs.jtf 
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_1/Runner3/general_inputs.txt            
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_1/Runner3/SVIHM_SWBM_General_Inputs.jtf 
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_1/Runner4/general_inputs.txt            
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_1/Runner4/SVIHM_SWBM_General_Inputs.jtf 
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_1/Runner5/general_inputs.txt            
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_1/Runner5/SVIHM_SWBM_General_Inputs.jtf 
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_1/Runner6/general_inputs.txt            
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_1/Runner6/SVIHM_SWBM_General_Inputs.jtf 
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_1/Runner7/general_inputs.txt            
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_1/Runner7/SVIHM_SWBM_General_Inputs.jtf 

sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_2/Runner1/general_inputs.txt            
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_2/Runner1/SVIHM_SWBM_General_Inputs.jtf 
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_2/Runner2/general_inputs.txt            
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_2/Runner2/SVIHM_SWBM_General_Inputs.jtf 
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_2/Runner3/general_inputs.txt            
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_2/Runner3/SVIHM_SWBM_General_Inputs.jtf 
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_2/Runner4/general_inputs.txt            
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_2/Runner4/SVIHM_SWBM_General_Inputs.jtf 
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_2/Runner5/general_inputs.txt            
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_2/Runner5/SVIHM_SWBM_General_Inputs.jtf 
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_2/Runner6/general_inputs.txt            
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_2/Runner6/SVIHM_SWBM_General_Inputs.jtf 
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_2/Runner7/general_inputs.txt            
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_2/Runner7/SVIHM_SWBM_General_Inputs.jtf 

sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_3/Runner1/general_inputs.txt            
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_3/Runner1/SVIHM_SWBM_General_Inputs.jtf 
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_3/Runner2/general_inputs.txt            
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_3/Runner2/SVIHM_SWBM_General_Inputs.jtf 
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_3/Runner3/general_inputs.txt            
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_3/Runner3/SVIHM_SWBM_General_Inputs.jtf 
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_3/Runner4/general_inputs.txt            
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_3/Runner4/SVIHM_SWBM_General_Inputs.jtf 
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_3/Runner5/general_inputs.txt            
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_3/Runner5/SVIHM_SWBM_General_Inputs.jtf 
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_3/Runner6/general_inputs.txt            
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_3/Runner6/SVIHM_SWBM_General_Inputs.jtf 
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_3/Runner7/general_inputs.txt            
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_3/Runner7/SVIHM_SWBM_General_Inputs.jtf 

sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4/Runner1/general_inputs.txt            
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4/Runner1/SVIHM_SWBM_General_Inputs.jtf 
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4/Runner2/general_inputs.txt            
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4/Runner2/SVIHM_SWBM_General_Inputs.jtf 
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4/Runner3/general_inputs.txt            
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4/Runner3/SVIHM_SWBM_General_Inputs.jtf 
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4/Runner4/general_inputs.txt            
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4/Runner4/SVIHM_SWBM_General_Inputs.jtf 
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4/Runner5/general_inputs.txt            
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4/Runner5/SVIHM_SWBM_General_Inputs.jtf 
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4/Runner6/general_inputs.txt            
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4/Runner6/SVIHM_SWBM_General_Inputs.jtf 
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4/Runner7/general_inputs.txt            
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_4/Runner7/SVIHM_SWBM_General_Inputs.jtf 

sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_5/Runner1/general_inputs.txt            
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_5/Runner1/SVIHM_SWBM_General_Inputs.jtf 
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_5/Runner2/general_inputs.txt            
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_5/Runner2/SVIHM_SWBM_General_Inputs.jtf 
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_5/Runner3/general_inputs.txt            
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_5/Runner3/SVIHM_SWBM_General_Inputs.jtf 
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_5/Runner4/general_inputs.txt            
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_5/Runner4/SVIHM_SWBM_General_Inputs.jtf 
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_5/Runner5/general_inputs.txt            
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_5/Runner5/SVIHM_SWBM_General_Inputs.jtf 
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_5/Runner6/general_inputs.txt            
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_5/Runner6/SVIHM_SWBM_General_Inputs.jtf 
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_5/Runner7/general_inputs.txt            
sed -i 's/basecase/ILR/1I' /aqua/dtolley/UCODE_Linear_Uncert/ILR/Calibration_5/Runner7/SVIHM_SWBM_General_Inputs.jtf 