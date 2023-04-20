
# Goal: Draw four clocks, with location and date, for easy comparison across locatoins
# So if it's 9AM Tokyo, easily show 4 clocks for Tokyo, DC, Berlin, and Moscow
# Showing those locations at the top, a date at the bottom
# If dates for two locations next to each other are the same, show once for both
# If time is between 6AM and 6PM, show black text on white background
# If between 6PM and 6AM, show white text on black background.
library(lubridate)
library(grid)

#
# Function to draw a clock face given an hour and minute
# Got this from stack overflow or some such, will find and attribute properly
#
drawClock <- function(hour, minute) {
  t <- seq(0, 2*pi, length=13)[-13]
  x <- cos(t)
  y <- sin(t)

 # grid.newpage()
  pushViewport(dataViewport(x, y, gp=gpar(lwd=1))) # 4 is line width (lwd)
  # Circle with ticks
  grid.circle(x=0.0, y=0.0, default="native", 
              r=unit(0.5, "native"))
  grid.segments(x, y, x*.9, y*.9, default="native")
  # Hour hand
  hourAngle <- pi/2 - (hour + minute/60)/12*2*pi
  grid.segments(0, 0, 
                .6*cos(hourAngle), .6*sin(hourAngle), 
                default="native", gp=gpar(lex=4))
  # Minute hand
  minuteAngle <- pi/2 - (minute)/60*2*pi
  grid.segments(0, 0, 
                .8*cos(minuteAngle), .8*sin(minuteAngle), 
                default="native", gp=gpar(lex=2))    
  grid.circle(0, 0, default="native", r=unit(1, "mm"),
              gp=gpar(fill="white"))
  upViewport() #pop up to the top of the context, so it ends where it starts
}

#
# Simple function to handle when to show day or night. 
#
day_night <- function(dn_time)
{
  if (hour(dn_time)< 6) 
    { return("Night")}
  else if (hour(dn_time) > 18)
    {return("Night")}
  else 
    {return("Day")}
}

#
# Even simpler function to return a given shading color for night v. day
#
fill_color <- function(fc_dn)
{
  # to find other colors, the function is....colors()
  # lower gray numbers are darker - gray10 is practically black
  # Looks like we could add some gradient on here, 
  # so early in the morning is dark,
  # make it lighter around 6AM, full white at noon onlt
  if(fc_dn=="Night")
    {return("gray50")}
  else if(fc_dn=="Day")
    {return("white")}
  else 
    {return("white")}
}

#
# world_clock takes a posix formatted time as a paramter
# and shows a world clock with four dials, one each for Los Angeles, DC, London, and Tokyo
#
# Dials are shaded if the time there is between 6PM and 6AM.
# Note that the times may be on a different day.
# Above each clock is the day of the month in that locatino
#
world_clock <- function(current_time = Sys.time(), show_tzones = c("US/Pacific","US/Eastern","Europe/London",'Asia/Tokyo'), use_tzones=show_tzones)
{
  # show_tzones is a list of timezones to use, with the given defaults. 
  # use_tzones isn't used yet but is intended to be for showing a different timezone than one of the standard ones
  #
  # The first line here gets timezones that have a slash but don't start with Et (Etc, a bunch of GMT relative ones that I don't way)
  #  Works by allowing anything that starts with something that's NOT an e and eventually has a slash
  #  OR by allowing anything that DOES start with an E but does NOT have a lower-case t after it, followed by anything and a slash
  # I think it's uglier than it should be, ut it works.
 # but so far not using this for anything....  
main_tz <- OlsonNames()[grep('^[^E].+/|^[worlE][^t].+/',OlsonNames())]


show_times <- list(length(show_tzones))
show_hours <- list(length(show_tzones))
for (tzs in show_tzones )
{
 # print(tzs) # 
  #show_times<- with_tz(current_time, tzone = tzs)
  #show_hours<- hour(show_times) #hour(with_tz(current_time, tzone = tzs))
  
  show_times[[tzs]]$time <- with_tz(current_time, tzone = tzs)
  show_times[[tzs]]$hour<- hour(show_times[[tzs]]$time) 
  show_times[[tzs]]$minute= minute(show_times[[tzs]]$time)
  show_times[[tzs]]$day= day(show_times[[tzs]]$time)
  show_times[[tzs]]$month= month(show_times[[tzs]]$time)
  show_times[[tzs]]$dn= day_night(show_times[[tzs]]$time)
  #print (show_times[tzs])
}
#print(show_times)
#print(show_hours)

#all_tz <- c('Europe/London',"EST","Europe/Moscow","Asia/Tokyo")

# Sys.timezone()
# tz_df <-data.frame(time_zone =Sys.timezone(),current_time, current_hour, current_minute)
# tz_df
# for (tz in all_tz)
# {
#   print(tz)
#   tz_time <-with_tz(current_time,tzone=tz)
#   rbind(tz_df, c(tz,tz_time,hour(tz_time), minute(tz_time)  ))
# }

#dc_time <- ny_time

#
# okay, we have it finding the time, and calculating for other time zones
# It's in a data frame, which makes it easy to access them,albeit not quite intuitive
# What I want is for it to genearate all the times for all the zones,
# and then we can show whicher we like
# It seems like if we could include something to say "here's the zones I want"
# then we wouldn't have to worry about telling it how many.
# And, it seems, if we had a list of time zones, we could make that a parameter
# Cycle through all of them and there you go.
# https://en.wikipedia.org/wiki/List_of_tz_database_time_zones

# clock_df <- data.frame(
#  tz =show_tzones,  #c("PST","EST","GMT","JST"),
#  tz_time= show_times, #c(la_time,dc_time, lon_time,jp_time),
#   tzhour=  c(hour(la_time),hour(dc_time), hour(lon_time),hour(jp_time)),#show_hours,  #
#   tzmin=c(minute(la_time), minute(dc_time), minute(lon_time), minute(jp_time)), #show_minutes, #
#   tzday=c(day(la_time), day(dc_time), day(lon_time), day(jp_time)),#show_days, #
#  tz_dn= c(day_night(la_time), day_night(dc_time), day_night(lon_time),day_night(jp_time))
# )
#print(show_hours)


#
# Start displaying the clock faces
#
grid.newpage()
# Start with a rectangle that can be divided into four squares
vp <- viewport(width=1.0, height=0.5) # originally four times as high, now twice as high
pushViewport(vp)
grid.rect(gp=gpar(col="black")) #black around all 
#nrow(clock_df)  #number of records, for if/we we want to automate number of clocks


n<-0 #Probably a cleaner way to use both time zones and a count, but it works.
for (tz in show_tzones)
{
  n<-n+1
 # print(tz)
#print(show_times[[i]]$hour) show_times[[tzs]]$dn
  pushViewport(viewport(x=0.25*(n-1), width=0.25, height=0.5,
                        just="left", name=paste0("clock",n)))
  grid.rect(gp=gpar(col="blue",fill=fill_color(show_times[[tz]]$dn)))
  drawClock(hour = show_times[[tz]]$hour, minute = show_times[[tz]]$minute)
  upViewport()
  pushViewport(viewport(x=0.25*(n-1), width=0.25, height=0.25,y=0.825,
                        just="left", name=paste0("header",n)))
  grid.text(paste(show_times[[tz]]$month,"/",show_times[[tz]]$day))
  upViewport()
  pushViewport(viewport(x=0.25*(n-1), width=0.25, height=0.25,y=0.125, #.825,
                        just="left", name=paste0("footer",n)))
  grid.text(tz) #, gp=gpar(font_size=20))
  upViewport()
}

# world_clock() # gets current local time and adjusts for the others
# world_clock(as.POSIXlt("2023-03-31 09:34:00")) # treats input as local time
# world_clock(as.POSIXlt("2023-03-31 08:15:00", tz="Asia/Tokyo")) # puts input into that timezone
# world_clock(as.POSIXlt("1945-09-02 08:15:00", tz="Asia/Tokyo")) # and this one works, well enough.
# so next steps are
# 1. Get the clock faces more automatic - done
# so we just iterate through the same code for 1, 2, 3, 4 - Done
# 2. Show month along with date in the heeader
# 3. Set up a footer and put the complete time zone in there - Done
# 4. Do a lookup for the three letter timezones so I can use them in the center consistently, still - no longer needed
# 5. Add a parameter with a vector of timezones and show clocks for each one in there.
# 6. Have a way to easily include e.g. Tinian and Los Alamos instead of Guam and Denver
# IF I put in "Tinian" and can later transalate that as being Guam
# Then I can display what I put in and generate times based on Guam (etc.)
#
#Current time, with Moscow instead of London
#world_clock(,c("US/Pacific","US/Eastern","Europe/Moscow",'Asia/Tokyo'))
#Surrender, starting from Tokyo, standard zones
#world_clock(as.POSIXlt("1945-09-02 08:15:00", tz="Asia/Tokyo")) 
# and with time zones specified
#world_clock(as.POSIXlt("1945-09-02 08:15:00", tz="Asia/Tokyo"),c("US/Pacific","US/Eastern","Europe/Moscow",'Asia/Tokyo')) 
}