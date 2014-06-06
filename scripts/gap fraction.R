#read in text files from each chamber (top, sun, shade) for each month


#need to set a new path for the function
getwd()
files_oct <- dir("raw data/gap_oct")
files_dec <- dir("raw data/gap_dec")
files_jan <- dir("raw data/gap_jan")

#function to paste gap files
read_gap <- function(filename){
  a <- read.table(paste("raw data/gap_oct/",filename,sep=""),header=TRUE)
  a$chamber <- filename
  return(a)
}
#runfunction

#October
gap_oct <- lapply(files_oct,read_gap) 
#rbind for final df
gap_oct <- rbind.fill(gap_oct)

#####function in not generic enough ...../gap_oct
#Dec
gap_dec <- lapply(files_dec,read_gap) 
gap_dec <- rbind.fill(gap_dec)

#Jan
gap_jan <- lapply(files_jan,read_gap) 
gap_jan <- rbind.fill(gap_jan)


######create a outer function that will deal with the different months of data
