########################################
############# Set working directory
########################################

setwd ("C:\\Users\\MAHESAR1\\Desktop\\R_SME\\Srinivas")

########################################
####### invoking libraries
########################################

library(openxlsx)
library(dplyr)


#######################################
###### reading dataset
#######################################

dat <- read.xlsx("data.xlsx", detectDates = T)

#####################################
##### Getting first and last values
#####################################

##dplyr

# to get first and last values of based on uniique subject id

data1 <- dat %>% 
    group_by(subject) %>% 
  summarise(first_value = first(na.omit(values)),
            last_value = last(na.omit(values))) %>% 
               left_join(dat, ., by = 'subject')

# to get first and last values of based on unique subject id and lab values

data2 <- dat %>% 
  group_by(subject, lab) %>% 
  summarise(first_value = first(na.omit(values)),
            last_value = last(na.omit(values))) %>% 
                      left_join(dat, ., by = c('subject','lab'))

# to get first and last values of based on unique subject id, lab values, and date

data3 <- dat %>% 
  group_by(subject, lab, date) %>% 
  summarise(first_value = first(na.omit(values)),
            last_value = last(na.omit(values))) %>% 
              left_join(dat, ., by = c('subject','lab', 'date'))



#######################
######## loops
#######################


## unique subject

dat_loop <- dat

for( i in unique(dat_loop$subject)){
  x <- dat_loop$values[dat_loop$subject==i][1]
  y <- dat_loop$values[dat_loop$subject==i][max(length(dat_loop$values[dat_loop$subject==i]))]
  
  dat_loop$first[dat_loop$subject==i] <- x
  dat_loop$last[dat_loop$subject==i] <- y
 
}

## unique subject & Lab

dat_loop1 <- dat

for( i in unique(dat_loop1$subject)){
  for( j in unique(dat_loop1$lab[dat_loop1$subject == i])){
  x <- dat_loop1$values[dat_loop1$subject==i & dat_loop1$lab==j][1]
  y <- dat_loop1$values[dat_loop1$subject==i & dat_loop1$lab==j][max(length(dat_loop1$values[dat_loop1$subject==i & dat_loop1$lab==j]))]
  
  dat_loop1$first[dat_loop1$subject==i & dat_loop1$lab==j] <- x
  dat_loop1$last[dat_loop1$subject==i & dat_loop1$lab==j] <- y
  
  }
  
}

######### Unique Subject, lab & Date


dat_loop2 <- dat

for( i in unique(dat_loop2$subject)){
  for( j in unique(dat_loop2$lab[dat_loop2$subject == i])){
    for (k in unique (dat_loop2$date[dat_loop2$subject==i & dat_loop2$lab==j])){
    x <- dat_loop2$values[dat_loop2$subject==i & dat_loop2$lab==j & dat_loop2$date ==k][1]
    y <- dat_loop2$values[dat_loop2$subject==i & dat_loop2$lab==j & dat_loop2$date ==k][max(length(dat_loop2$values[dat_loop2$subject==i & dat_loop2$lab==j & dat_loop2$date ==k]))]
    
    dat_loop2$first[dat_loop2$subject==i & dat_loop2$lab==j & dat_loop2$date ==k] <- x
    dat_loop2$last[dat_loop2$subject==i & dat_loop2$lab==j & dat_loop2$date ==k] <- y
    
    }
    
  }
  
}


#############################################################
#### If we want to see first and last values in same column
##############################################################

## unique Subject

dat_same <- dat

dat_same$first_last <- NA

for( i in unique(dat_same$subject)){
  x <- dat_same$values[dat_same$subject==i][1]
  y <- dat_same$values[dat_same$subject==i][max(length(dat_same$values[dat_same$subject==i]))]
  
  dat_same$first_last[dat_same$subject==i] [1] <- x
  dat_same$first_last[dat_same$subject==i][max(length(dat_same$values[dat_same$subject==i]))] <- y
  
}

##########
## unique Subject & lab
#########

dat_same1 <- dat

dat_same1$first_last <- NA

for( i in unique(dat_same1$subject)){
  for( j in unique(dat_same1$lab[dat_same1$subject == i])){
    x <- dat_same1$values[dat_same1$subject==i & dat_same1$lab==j][1]
    y <- dat_same1$values[dat_same1$subject==i & dat_same1$lab==j][max(length(dat_same1$values[dat_same1$subject==i & dat_same1$lab==j]))]
    
    dat_same1$first_last[dat_same1$subject==i & dat_same1$lab==j] [1] <- x
    dat_same1$first_last[dat_same1$subject==i & dat_same1$lab==j] [max(length(dat_same1$values[dat_same1$subject==i & dat_same1$lab==j]))] <- y
    
  }
  
}

########################
### Subject, lab And Date
########################
dat_same2 <- dat

dat_same2$first_last <- NA

for( i in unique(dat_same2$subject)){
  for( j in unique(dat_same2$lab[dat_same2$subject == i])){
    for (k in unique (dat_same2$date[dat_same2$subject==i & dat_same2$lab==j])){
    x <- dat_same2$values[dat_same2$subject==i & dat_same2$lab==j & dat_same2$date==k][1]
    y <- dat_same2$values[dat_same2$subject==i & dat_same2$lab==j & dat_same2$date==k][max(length(dat_same2$values[dat_same2$subject==i & dat_same2$lab==j & dat_same2$date==k]))]
    
    dat_same2$first_last[dat_same2$subject==i & dat_same2$lab==j & dat_same2$date ==k ] [1] <- x
    dat_same2$first_last[dat_same2$subject==i & dat_same2$lab==j & dat_same2$date == k] [max(length(dat_same2$values[dat_same2$subject==i & dat_same2$lab==j & dat_same2$date ==k ]))] <- y
    
    }
  
  }
}

#####################################################
############# END
#####################################################