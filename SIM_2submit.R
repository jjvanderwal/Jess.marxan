### Batching Marxan

### Directory

basedir = '/home/jc152199/Others/JC/BASESIM/'

# Establish a temporary directory to write out the .sh files and .Rout files to
# This keeps your workspace more orderly and should the script fuck up all error logs will be stored in one location

tmp.pbs = '/home/jc152199/tmp.pbs2/'
setwd(tmp.pbs)

# Input location of the script to be run by this batch script

script.file = paste(basedir,'SIM_JTest_2run.r',sep='')

### Loop through number of planning units to investigate in each Marxan run

xxs = c(25,50,75,100) # These numbers represent the number of planning units which will be investigated in each iteration of Marxan

for (xx in xxs)

	{
	
	for (it in sprintf('%03i',1:100)) # This number corresponds to the replicate run of Marxan
	
		{
		
		### Define arguments

		arg.xx = paste('xx="',xx,'" ',sep='') # The argument for number of Planning Units to investigate
		arg.it = paste('it="',it,'" ',sep='') # The argument for replicate run

		### Name for the submission

		tname = paste('Investigate',xx,'PUs','Replicate',it,'.sh',sep='') #Create a name for the .sh file for each element (dayx) in days.list
				
		zz = file(tname,'w') # This command creates a file with the name 'tname' The 'w' command opens the file and writes the following lines of text into it
			
			cat('cd $PBS_O_WORKDIR\n',file=zz) # Sets the directory to write out summary files at the above directory 'tmp.pbs'
			cat('source /etc/profile.d/modules.sh\n',file=zz) ### Runs an .sh file which allows modules to be loaded
			cat('module load R\n',file=zz) # Open the R module
			cat("R CMD BATCH --no-save --no-restore '--args ",arg.xx,arg.it,"' ",script.file,' ',gsub('.sh','',tname),'.Rout \n',sep='',file=zz) # This line tells R not to save to the .Rdata file and also not to restore the environment from the .Rdata file, very important when batching or you will get multiple fuckups, also defines the arguments, says where script to run is located, and defines the name for the R.out file
				
		close(zz) # Closes the .sh file that you've just written

		# The follow line submits the job to the HPC, leave it commented out until you're ready to RUN all the jobs\
		# Always run the script first without removing the comment, this will still generate the .sh files for you to check before running
		# Once you're happy with that then submit a single .sh file and make sure it runs through to completion before batching out the rest with the following line

		system(paste('qsub -m n -l walltime=00:30:00 -l pmem=2gb -l nodes=1:ppn=1 ',tname,sep='')) 
		system(paste('sleep .1'))
		
		}
	
	}



	
