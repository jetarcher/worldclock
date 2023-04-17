
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
# Note that the times may be on a different day, and that is not currently shown (though can be inferred)
#
world_clock <- function(current_time = Sys.time(), show_tzones = c("America/Los_Angeles","US/Eastern","Europe/London",'Asia/Tokyo'))
{
  # show_tzones isn't used for anything...yet. Just getting the parameter in there and working
  #get timezones that have a slash but don't start with Et (Etc, a bunch of GMT relative ones that I don't way)
  #  Works by allowing anything that starts with something that's NOT an e and eventually has a slash
  #  OR by allowing anything that DOES start with an E but does NOT have a lower-case t after it, followed by anything and a slash
  # I think it's uglier than it should be, ut it works.
  # Takes a time value as the starting point, uses system time if none given
  # It's the "current time" so everything gets adjusted as if it's starting from Los Angeles
# but so far not using this for anything....  
main_tz <- OlsonNames()[grep('^[^E].+/|^[worlE][^t].+/',OlsonNames())]

# The initial time, whether input or Sys.time, will be converted to local
# Which was fine at first, but also need to show it specifically as LA time
# So these current_* variables can probably go at some point
current_hour <- hour(current_time) #format(current_time,'%H')
current_minute <- minute(current_time) #format(current_time,'%M')

la_time <- with_tz(current_time, tzone = "America/Los_Angeles")
lon_time <-with_tz(current_time, tzone = "Europe/London")
ny_time <-with_tz(current_time, tzone = "US/Eastern")
jp_time <- with_tz(current_time,tzone='Asia/Tokyo')
mo_time <- with_tz(current_time,tzone='Europe/Moscow')
show_times <- list(length(show_tzones))
for (tzs in show_tzones )
{
  print(tzs)
  #show_times[tzs]= c(show_times, with_tz(current_time, tzone = tzs))
  #print (show_times[tzs])
}

all_tz <- c('Europe/London',"EST","Europe/Moscow","Asia/Tokyo")

# Sys.timezone()
# tz_df <-data.frame(time_zone =Sys.timezone(),current_time, current_hour, current_minute)
# tz_df
# for (tz in all_tz)
# {
#   print(tz)
#   tz_time <-with_tz(current_time,tzone=tz)
#   rbind(tz_df, c(tz,tz_time,hour(tz_time), minute(tz_time)  ))
# }

dc_time <- ny_time

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

clock_df <- data.frame(
 tz =show_tzones,  #c("PST","EST","GMT","JST"),
 tz_time=c(la_time,dc_time, lon_time,jp_time),
  tzhour=c(hour(la_time),hour(dc_time), hour(lon_time),hour(jp_time)),
  tzmin=c(minute(la_time), minute(dc_time), minute(lon_time), minute(jp_time)),
  tzday=c(day(la_time), day(dc_time), day(lon_time), day(jp_time)),
 tz_dn= c(day_night(la_time), day_night(dc_time), day_night(lon_time),day_night(jp_time))
)
#clock_df # use this to see what's in the data frame, where needed
# Need to clean some of these up, helpful though they are
#day(clock_df$tz_time[4])
#
# Start displaying the clock faces
#
grid.newpage()
# Start with a rectangle that can be divided into four squares
vp <- viewport(width=1.0, height=0.5) # originally four times as high, now twice as high
pushViewport(vp)
grid.rect(gp=gpar(col="black")) #black around all 
nrow(clock_df)  #number of records, for if/we we want to automate number of clocks
# Square inside the main rectangled, 1/4 the width and 1/2 the height
# pushViewport(viewport(x=0.0, width=0.25, height=0.5,
#                       just="left", name=clock_df$tz[1])) #this makes the name the timezone - previously A B C D, and maybe that was better.
# grid.rect(gp=gpar(col="red",fill=fill_color(clock_df$tz_dn[1])))
# #upViewport(2)
# #pushViewport(viewport(x=0.0, width=0.25, height=0.25,
# #                      just="left", name="header1"))
# #grid.rect(gp=gpar(col="purple",fill=fill_color(clock_df$tz_dn[1])))
# #grid.circle(x=0.125, y=0.5, default="native", 
# #            r=unit(0.5, "native"))
# drawClock(hour = clock_df$tzhour[1], minute = clock_df$tzmin[1])
# grid.text(clock_df$tz[1])
# 
# upViewport()
# #upViewport(2)
# pushViewport(viewport(x=0.0, width=0.25, height=0.25,y=0.825,
#                       just="left", name="header1"))
# grid.text(clock_df$tzday[1])
# upViewport()
# # upViewport(3)
# # pushViewport(viewport(x=0.0, width=0.25, height=0.25,
# #                       just="left", name="header1")
# #              )
# # grid.rect(gp=gpar(col="cyan"))
# # #grid.text(clock_df$tzday)
# # grid.text('17') # so this isn't showing,  nor the border
# # It seems to still be part of the first clock viewport, not the one above it.
# #upViewport(1)
# #
# # Second clock face starts here
# #
# pushViewport(viewport(x=0.25, width=0.25, height=0.5,
#                       just="left", name=clock_df$tz[2]))
# grid.rect(gp=gpar(col="green",fill=fill_color(clock_df$tz_dn[2])))
# drawClock(hour = clock_df$tzhour[2], minute = clock_df$tzmin[2])
# grid.text(clock_df$tz[2])
# upViewport()
# pushViewport(viewport(x=0.25, width=0.25, height=0.25,y=0.825,
#                       just="left", name="header2"))
# grid.text(clock_df$tzday[2])
# upViewport()
# 
# #
# # Third clock face starts here
# #
# pushViewport(viewport(x=0.5, width=0.25, height=0.5,
#                       just="left", name=clock_df$tz[3]))
# grid.rect(gp=gpar(col="yellow",fill=fill_color(clock_df$tz_dn[3])))
# drawClock(hour = clock_df$tzhour[3], minute = clock_df$tzmin[3])
# grid.text(clock_df$tz[3])
# upViewport()
# pushViewport(viewport(x=0.5, width=0.25, height=0.25,y=0.825,
#                       just="left", name="header3"))
# grid.text(clock_df$tzday[3])
# upViewport()
# #
# # Fourth clock face starts here
# #
# pushViewport(viewport(x=0.75, width=0.25, height=0.5,
#                       just="left", name=clock_df$tz[4]))
# grid.rect(gp=gpar(col="blue",fill=fill_color(clock_df$tz_dn[4])))
# drawClock(hour = clock_df$tzhour[4], minute = clock_df$tzmin[4])
# grid.text(clock_df$tz[4])
# upViewport()
# pushViewport(viewport(x=0.75, width=0.25, height=0.25,y=0.825,
#                       just="left", name="header4"))
# grid.text(clock_df$tzday[4])
# upViewport()

for (i in 1:4)
{
#print(i)

  pushViewport(viewport(x=0.25*(i-1), width=0.25, height=0.5,
                        just="left", name=clock_df$tz[i]))
  grid.rect(gp=gpar(col="blue",fill=fill_color(clock_df$tz_dn[i])))
  drawClock(hour = clock_df$tzhour[i], minute = clock_df$tzmin[i])
  #grid.text(clock_df$tz[i])
  upViewport()
  pushViewport(viewport(x=0.25*(i-1), width=0.25, height=0.25,y=0.825,
                        just="left", name=paste0("header",i)))
  grid.text(clock_df$tzday[i])
  upViewport()
  pushViewport(viewport(x=0.25*(i-1), width=0.25, height=0.25,y=0.125, #.825,
                        just="left", name=paste0("footer",i)))
  grid.text(clock_df$tz[i]) #, gp=gpar(font_size=20))
  upViewport()
}
#grid.rect(gp=gpar(col="green"))
#world_clock() # gets current local time and adjusts for the others
#world_clock(as.POSIXlt("2023-03-31 09:34:00")) # treats input as local time
# world_clock(as.POSIXlt("2023-03-31 08:15:00", tz="Asia/Tokyo")) #puts input into that timezone
# world_clock(as.POSIXlt("1945-09-02 08:15:00", tz="Asia/Tokyo")) # and this one works, well enough.
# so next steps are
# 1. Get the clock faces more automatic - done
# so we just iterate through the same code for 1, 2, 3, 4
# 2. Show month along with date in the heeader
# 3. Set up a footer and put the complete time zone in there
# 4. Do a lookup for the three letter timezones so I can use them in the center consistently, still
# 5. Add a parameter with a vector of timezones and show clocks for each one in there.
}