*Load_history is the original file that was given us to by kaggle

*Load_history_clean is the output of the cleanup.py script under the python folder.  It removes the extraneous commas and quotes around numerics.
This file should be the starting point for input files

*Load_history_training is the same as Load_history_clean without the final week, which is the time period we are supposed to forecast into

*Load_history_mmx is the input file that was fed into the MMX Dash

*mmx_preprocess is the script that converts Load_history_training into Load_history_mmx
