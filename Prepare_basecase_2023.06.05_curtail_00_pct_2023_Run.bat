@setlocal
@echo off
set scen=basecase_2023.06.05_curtail_00_pct_2023

:: Intended to be run in the main SVIHM folder

:: Create Run folders (if does not exist)
if not exist "Run" mkdir Run
if not exist "Run\SWBM" mkdir Run\SWBM
if not exist "Run\MODFLOW" mkdir Run\MODFLOW

:: Copy scenario independant MODFLOW model files
xcopy SVIHM_Input_Files\time_independent_input_files\system_commands.txt Run\SWBM /Y /I
xcopy SVIHM_Input_Files\time_independent_input_files\ET_Zone_Cells.txt Run\SWBM /Y /I
xcopy SVIHM_Input_Files\time_independent_input_files\ET_Cells_Extinction_Depth.txt Run\SWBM /Y /I
xcopy SVIHM_Input_Files\time_independent_input_files\recharge_zones.txt Run\SWBM /Y /I
xcopy SVIHM_Input_Files\time_independent_input_files\SFR_network.txt Run\SWBM /Y /I
xcopy SVIHM_Input_Files\time_independent_input_files\SFR_routing.txt Run\SWBM /Y /I
xcopy SVIHM_Input_Files\time_independent_input_files\SFR_inflow_segments.txt Run\SWBM /Y /I
xcopy SVIHM_Input_Files\time_independent_input_files\ag_well_summary.txt Run\SWBM /Y /I
xcopy SVIHM_Input_Files\time_independent_input_files\ag_well_list_by_polygon.txt Run\SWBM /Y /I
xcopy SVIHM_Input_Files\time_independent_input_files\muni_well_summary.txt Run\SWBM /Y /I
xcopy SVIHM_Input_Files\time_independent_input_files\muni_well_list_by_polygon.txt Run\SWBM /Y /I
xcopy SVIHM_Input_Files\time_independent_input_files\print_daily.txt Run\SWBM /Y /I
xcopy SVIHM_Input_Files\time_independent_input_files\polygons_table.txt Run\SWBM /Y /I
xcopy SVIHM_Input_Files\time_independent_input_files\precip_factors.txt Run\SWBM /Y /I
xcopy SVIHM_Input_Files\time_independent_input_files\landcover_table.txt Run\SWBM /Y /I
xcopy SVIHM_Input_Files\time_independent_input_files\MAR_depth.txt Run\SWBM /Y /I
xcopy SVIHM_Input_Files\time_independent_input_files\curtailment_fractions.txt Run\SWBM /Y /I
xcopy SVIHM_Input_Files\time_independent_input_files\SVIHM.* Run\MODFLOW /Y /I
xcopy SVIHM_Input_Files\time_independent_input_files\Starting_Heads_L*.txt Run\MODFLOW /Y /I

:: Copy files from scenario folder to run folder
xcopy Scenarios\%scen%\*.zone Run\SWBM /Y /I
xcopy Scenarios\%scen%\SVIHM.* Run\MODFLOW /Y /I 
xcopy SVIHM_Input_Files\time_independent_input_files\SVIHM_*_template.txt Run\SWBM /Y /I

:: Copy in generic run batch file
xcopy Scripts\Batch_Scripts\Run_SVIHM.bat Run /Y /I

:: Copy in Update_Drain_Inflows input file
xcopy SVIHM_Input_Files\Update_Drain_Inflows.in Run\MODFLOW /Y /I

:: Files of Unknown Necessity
xcopy Calib_Sens_Files\UCODE\UCODE_Template_Files\SVIHM_SFR.jtf Run\SWBM /Y /I
