library(readxl)
library(stringi)
library(lattice)
library(ggplot2)

# TO DO: Figure out where to put the processed data.. if there is any

nback_data1 = read_excel("C:/data/nback_results.xlsx", range = "AG2:AS450", sheet = 1, col_types = "text")
nback_data2 = read_excel("C:/data/nback_results.xlsx", range = "BL2:BP450", sheet = 1, col_types = "text")
nback_data3 = read_excel("C:/data/nback_results.xlsx", range = "GE2:GP450", sheet = 1, col_types = "text")
nback_data4 = read_excel("C:/data/nback_results.xlsx", range = "DX2:DX450", sheet = 1, col_types = "text")

block1_nulls = nback_data1$BlockList1
block2_nulls = nback_data1$BlockList2
block3_nulls = nback_data1$BlockList3
block4_nulls = nback_data1$BlockList4
blocks_nums = nback_data1$Trial

blocks_str = nback_data2$`Running[Trial]`

subtrial = nback_data2$SubTrial

interstimulus_interval = nback_data2$ISI

nback = nback_data4$`Running[SubTrial]`
nback = stri_sub(nback, -1,-1)
nback = as.numeric(nback)

# Check this
expected_correct_response1 = nback_data2$CorrectAnswer
expected_correct_response2 = nback_data3$Stimulus.CRESP

subject_response = nback_data3$Stimulus.RESP

#need to remove indices where mr triggered
indices_to_remove = which(subject_response == 5)
subject_response_onset = nback_data3$Stimulus.RT
subject_response_onset[indices_to_remove] = 0
subject_response_onset[subject_response_onset == 0] = NA
subject_response_onset = as.numeric(subject_response_onset)

# check this
subject_accuracy_eprime = nback_data3$Stimulus.ACC
subject_accuracy_r = expected_correct_response2 == subject_response

# sanity check
matching_expected_correct = expected_correct_response1 == expected_correct_response2
matching_accuracy_vars = subject_accuracy_eprime == subject_accuracy_r

# response time by nback
responsetime_by_nback = data.frame(nback, subject_response_onset)


# Check for outliers
hist(subject_response_onset, breaks=10, col="red", xlab="response time (ms)", main="Total Reaction Time")


# TO DO: adjust x axis to represent strings of 0,1,2,3 nback
# TO DO: color code based on ISI
# TO DO: come up with threshold for ISI for each nback (0-back should have lower threshold for "early" response)
# TO DO: plot accuracy as a function of nback condition (figure out how to group properly in R)

xyplot(subject_response_onset ~ nback)

ggplot(data=responsetime_by_nback, aes(x=nback,y=subject_response_onset))
