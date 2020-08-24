# Part 1 ---------------------------------

# * device, channel ---------------------------------
df = data_df %>% #dplyr::filter(session_duration_seconds>0) %>% 
  dplyr::group_by(channel,device) %>% 
  dplyr::summarise(NoConversion = sum(!conversion),
                   conversion = sum(conversion),
                   totalSession = conversion + NoConversion,
                   conversionRate = 100*conversion/totalSession,
                   event_count = sum(event_count))

df %>%dplyr::filter(conversionRate>0) %>%  
  ggplot() + geom_bar(aes(x=device, y= conversionRate,fill=channel),position = "dodge",stat = "identity") + 
  theme_minimal()

df %>%dplyr::filter(conversion>0) %>%  
  ggplot() + geom_bar(aes(x=device, y= conversion,fill=channel),position = "dodge",stat = "identity") + 
  theme_minimal()

# * region ---------------------------------
df = data_df %>% #dplyr::filter(session_duration_seconds>0) %>% 
  dplyr::group_by(region) %>% 
  dplyr::summarise(NoConversion = sum(!conversion),
                   conversion = sum(conversion),
                   totalSession = conversion + NoConversion,
                   conversionRate = 100*conversion/totalSession,
                   event_count = sum(event_count))

df %>%dplyr::filter(conversionRate>0) %>%  
  ggplot() + geom_bar(aes(x=region, y= conversionRate,fill=region),position = "dodge",stat = "identity") + 
  theme_minimal()

df %>%dplyr::filter(conversion>0) %>%  
  ggplot() + geom_bar(aes(x=device, y= conversion,fill=channel),position = "dodge",stat = "identity") + 
  theme_minimal()

# * event count ----------------------------------
data_df %>% dplyr::filter(session_duration_seconds>0 & event_count < 25) %>%
  dplyr::group_by(conversion,event_count) %>%
  dplyr::summarise(count = n()) %>% ungroup() %>%
  dplyr::group_by(conversion)%>%
  dplyr::mutate(countPercent = 100*count/sum(count))%>%
  ggplot()+geom_bar(aes(x=event_count,y=countPercent,fill=conversion),stat = "identity",position = "dodge") + theme_minimal()


# * session_duration_seconds ----------------------------------
data_df %>% dplyr::filter(session_duration_seconds>0,session_duration_seconds < 5000) %>%
  ggplot()+geom_histogram(aes(x=session_duration_seconds,fill = conversion)) + 
  facet_wrap(~conversion,scales = "free_y") + theme_minimal()

data_df %>% dplyr::filter(conversion == T & session_duration_seconds>0,session_duration_seconds < 5000) %>%
  ggplot()+geom_histogram(aes(x=session_duration_seconds))

data_df %>% dplyr::filter(conversion == T & session_duration_seconds>0,session_duration_seconds < 5000) %>%
  ggplot()+geom_histogram(aes(x=session_duration_seconds))

data_df %>% dplyr::filter(conversion == F & session_duration_seconds>0,session_duration_seconds < 5000 ) %>%
  ggplot()+geom_histogram(aes(x=session_duration_seconds))


# session duration and event count
conversionDateDF_exclude0sessions = data_df %>% 
  dplyr::filter(session_duration_seconds>0) %>% 
  dplyr::mutate(date=as.Date(session_start_time)) %>% dplyr::group_by(date) %>% 
  dplyr::summarise(NoConversion = sum(!conversion),
                   conversion = sum(conversion)) 

plot_ly(data = conversionDateDF_exclude0sessions %>% dplyr::mutate(rate = conversion / (conversion + NoConversion) )) %>% 
  add_trace(x=~date,y=~rate, name = "conversion rate", type = 'scatter',mode = 'lines') %>%
  layout( title = "Conversions rate over time")

data_df[sample(1:nrow(data_df), floor(nrow(data_df)*0.5), replace=FALSE),] %>% dplyr::filter(session_duration_seconds>0,session_duration_seconds<4e4, event_count< 200) %>% 
  ggplot + geom_point(aes(x=session_duration_seconds, y=event_count,color = conversion)) + facet_wrap(~channel) + theme_minimal()

# * conversion over time ----------------------------------
conversionDateDF = data_df %>% dplyr::mutate(date=as.Date(session_start_time)) %>% dplyr::group_by(date) %>% 
  dplyr::summarise(NoConversion = sum(!conversion),
                   conversion = sum(conversion)) 

plot_ly(data = conversionDateDF) %>% add_trace(x=~date,y=~conversion, name = "conversion", type = 'scatter',mode = 'lines',yaxis ="y2") %>%
  add_trace(x=~date,y=~NoConversion,type = 'scatter',mode = 'lines', name = "No conversion",type = 'scatter',mode = 'lines') %>%
  layout( yaxis = list(title = "no conversion count", rangemode = "tozero"),
          yaxis2 = list(rangemode = "tozero", title = "conversion count",overlaying = "y",side = "right"))

plot_ly(data = conversionDateDF %>% dplyr::mutate(rate = 100*conversion / (conversion + NoConversion) )) %>% 
  add_trace(x=~date,y=~rate, name = "conversion rate", type = 'scatter',mode = 'lines')

# * channel over time ----------------------------------
conversionDateDF = data_df %>% #dplyr::filter(session_duration_seconds>0 ) %>% 
                               dplyr::mutate(date=as.Date(session_start_time)) %>% dplyr::group_by(channel,date) %>% 
                               dplyr::summarise(NoConversion = sum(!conversion),
                                                conversion = sum(conversion)) 

conversionDateDF %>% dplyr::filter(date > as.Date("2019-11-06")) %>% 
  ggplot() + geom_bar(aes(x=date,y=conversion,fill=channel),position = "stack",stat = "identity") + theme_minimal()

conversionDateDF %>% dplyr::filter(date > as.Date("2019-11-06")) %>%  ungroup()%>%
  dplyr::mutate(date = format(date, "%y-%m")) %>% 
  dplyr::group_by(date,channel ) %>% 
  dplyr::summarise(conversion = sum(conversion), NoConversion = sum(NoConversion))%>%
  dplyr::mutate(rate = 100*conversion / (conversion + NoConversion) ) %>%
  ggplot() + geom_bar(aes(x=date,y=rate,fill=channel),position = "dodge",stat = "identity") + theme_minimal()

 
# Error analysis ---------------------------------

# doubt about the region, sub-region, country data - - - there is a systematic data errors ie. large overweight in that region, subregion, country is NA is it is a non-conversion

# conversion time is before session start --> we log the wrong session
# how many conversion times does not match conversion session indication
totalConversion = nrow(data_df %>% dplyr::filter(conversion==T))

conversionTimeBeforeStart = nrow(data_df %>% dplyr::filter(conversion==T, conversion_time < session_start_time + dseconds(1))) / totalConversion
print(sprintf("%s percent of conversion times are before the labelled session starts", round(conversionTimeBeforeStart*100,2)))

zeroSessions = nrow(data_df %>% dplyr::filter(conversion==T, session_duration_seconds == 0 )) / totalConversion
print(sprintf("%s percent of conversions have a recorded time of 0", round(zeroSessions*100,2)))

zeroSessionWithConversionProblem = nrow(data_df %>% dplyr::filter(conversion==T, session_duration_seconds == 0, conversion_time < session_start_time + dseconds(1))) / totalConversion
print(sprintf("%s percent of conversions have a recorded time of 0 and a convertion time before the session starts", round(zeroSessionWithConversionProblem*100,2)))

# -> all session_duration = 0 also have mismatch between conversion time and session start time

zeroSessionWithSingleEvent = nrow(data_df %>% dplyr::filter(conversion==T, session_duration_seconds == 0,  event_count == 1) )/ totalConversion
print(sprintf("%s percent of conversions have a recorded time of 0 and only a single event", round(zeroSessionWithSingleEvent*100,2)))

# double submissions 
doubleSubmissions = data_df %>% dplyr::filter(conversion==T) %>% 
  dplyr::group_by(user_id,conversion_time ) %>% 
  dplyr::summarise(conversionCount = n()) %>% dplyr::filter(conversionCount > 1)

print(sprintf("%s percent of conversions have two conversions recorded at the same time", round(nrow(doubleSubmissions)/totalConversion*100,2)))

# session duration 0 same day as session duration > 0
zeroSessionDF = data_df %>% dplyr::mutate(isZeroSession = session_duration_seconds == 0,
                                          date = as.Date(session_start_time)) 

zeroSessionDF = left_join(zeroSessionDF, 
                          data_df %>% dplyr::mutate(date = as.Date(session_start_time))%>% 
                            dplyr::group_by(date , user_id) %>% 
                            dplyr::summarise(count = n()) %>% ungroup())

nrow(zeroSessionDF %>% dplyr::filter(isZeroSession == T & count > 1))
nrow(zeroSessionDF %>% dplyr::filter(isZeroSession == T))

nrow(zeroSessionDF %>% dplyr::filter(isZeroSession == T,conversion == T)) / totalConversion
nrow(zeroSessionDF %>% dplyr::filter(isZeroSession == T,conversion == F)) / (nrow(data_df) - totalConversion)
