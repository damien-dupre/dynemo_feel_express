library(tidyverse)

################################################################################
get_video_duration <- function(file_path) {
  
  ffprob_path <- "C:\\Users\\dupred\\Desktop\\ffmpeg-20190312-d227ed5-win64-static\\ffmpeg-20190312-d227ed5-win64-static\\bin\\ffprobe.exe"
  system_code <- "-show_entries format=duration"
  
  ffprobe_duration <- paste(ffprob_path, system_code,file_path)
  
  system(command = ffprobe_duration, intern = TRUE)[2]
  
}
################################################################################

list_video <- "C:\\Users\\dupred\\Desktop\\DynEmo_Database_mp4" %>% 
  list.files(pattern = "\\.mp4$",full.names = TRUE)

ffprobe_duration <- list_video %>% 
  purrr::map(get_video_duration) %>% 
  plyr::ldply(data.frame)%>% 
  dplyr::rename_at(1,~"ffprobe_duration")

metadata_video <- data.frame(list_video, ffprobe_duration) %>% 
  dplyr::mutate_if(is.factor,as.character) %>% 
  dplyr::mutate(C_Video = base::basename(list_video) %>% stringr::str_replace(".mp4", "")) %>% 
  dplyr::mutate(ffprobe_duration = stringr::str_replace(ffprobe_duration, "duration=", "") %>% as.numeric()) %>% #as.numeric()*1000 for millisecond 
  dplyr::mutate(ffprobe_duration = round(ffprobe_duration,0)) %>% 
  dplyr::select(-list_video)

# write_rds(metadata_video, "metadata_video.rds")


