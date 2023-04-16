
# Goal: Draw four clocks, with location and date, for easy comparison across locatoins
# So if it's 9AM Tokyo, easily show 4 clocks for Tokyo, DC, Berlin, and Moscow
# Showing those locations at the top, a date at the bottom
# If dates for two locations next to each other are the same, show once for both
# If time is between 6AM and 6PM, show black text on white background
# If between 6PM and 6AM, show white text on black background.
library(lubridate)
library(grid)


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
}

day_night <- function(dn_time)
{
  if (hour(dn_time)< 6) 
    { return("Night")}
  else if (hour(dn_time) > 18)
    {return("Night")}
  else 
    {return("Day")}
}

fill_color <- function(fc_dn)
{
  #to find other colors, the function is....colors()
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
#get timezones that have a slash but don't start with Et (Etc, a bunch of GMT relative ones that I don't way)
#  Works by allowing anything that starts with something that's NOT an e and eventually has a slash
#  OR by allowing anything that DOES start with an E but does NOT have a lower-case t after it, followed by anything and a slash
# I think it's uglier than it should be, ut it works.
main_tz <- OlsonNames()[grep('^[^E].+/|^[E][^t].+/',OlsonNames())]
current_time <- Sys.time() # can adjust it by appending: -hours(3)+minutes(10)
current_hour <- hour(current_time) #format(current_time,'%H')
current_minute <- minute(current_time) #format(current_time,'%M')

lon_time <-with_tz(current_time, tzone = "Europe/London")
ny_time <-with_tz(current_time, tzone = "US/Eastern")
jp_time <- with_tz(current_time,tzone='Asia/Tokyo')
mo_time <- with_tz(current_time,tzone='Europe/Moscow')
all_tz <- c('Europe/London',"EST","Europe/Moscow","Asia/Tokyo")
# all_tz <- OlsonNames() #oddly enough, this is available time zones....
# Sys.timezone()
# tz_df <-data.frame(time_zone =Sys.timezone(),current_time, current_hour, current_minute)
# tz_df
# for (tz in all_tz)
# {
#   print(tz)
#   tz_time <-with_tz(current_time,tzone=tz)
#   rbind(tz_df, c(tz,tz_time,hour(tz_time), minute(tz_time)  ))
# }
# print(tz_df)
#print(current_time, ny_time, mo_time, jp_time)
dc_time <- ny_time
#clock[1]<- c(current_hour, current_minute)
#clock[2] <-c(hour(dc_time),minute(dc_time))
time_1 <- c(current_hour, current_minute)
time_2 <- c(hour(dc_time),minute(dc_time))
time_3 <-c(hour(lon_time),minute(lon_time))
time_4 <- c(hour(jp_time), minute(jp_time))
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
 tz =c("PST","EST","GMT","JST"),
 tz_time=c(current_time,dc_time, lon_time,jp_time),
  tzhour=c(current_hour,hour(dc_time), hour(lon_time),hour(jp_time)),
  tzmin=c(current_minute, minute(dc_time), minute(lon_time), minute(jp_time)),
  tzday=c(day(current_time), day(dc_time), day(lon_time), day(jp_time)),
 tz_dn= c(day_night(current_time), day_night(dc_time), day_night(lon_time),day_night(jp_time))
)
clock_df
day(clock_df$tz_time[4])
#clock_df <- tz_df
#clock_df
grid.newpage()
vp <- viewport(width=1.0, height=0.5) # four times as long as high
pushViewport(vp)
grid.rect(gp=gpar(col="black")) #black around all 

pushViewport(viewport(x=0.0, width=0.25, height=0.5,
                      just="left", name=clock_df$tz[1])) #this makes the name the timezone - previously A B C D, and maybe that was better.
grid.rect(gp=gpar(col="red",fill=fill_color(clock_df$tz_dn[1])))
#grid.circle(x=0.125, y=0.5, default="native", 
#            r=unit(0.5, "native"))
drawClock(hour = clock_df$tzhour[1], minute = clock_df$tzmin[1])
grid.text(clock_df$tz[1])
upViewport(2)
pushViewport(viewport(x=0.25, width=0.25, height=0.5,
                      just="left", name=clock_df$tz[2]))
grid.rect(gp=gpar(col="green",fill=fill_color(clock_df$tz_dn[2])))
drawClock(hour = clock_df$tzhour[2], minute = clock_df$tzmin[2])
grid.text(clock_df$tz[2])
upViewport(2)
pushViewport(viewport(x=0.5, width=0.25, height=0.5,
                      just="left", name=clock_df$tz[3]))
grid.rect(gp=gpar(col="yellow",fill=fill_color(clock_df$tz_dn[3])))
drawClock(hour = clock_df$tzhour[3], minute = clock_df$tzmin[3])
grid.text(clock_df$tz[3])
upViewport(2)
pushViewport(viewport(x=0.75, width=0.25, height=0.5,
                      just="left", name=clock_df$tz[4]))
grid.rect(gp=gpar(col="blue",fill=fill_color(clock_df$tz_dn[4])))
drawClock(hour = clock_df$tzhour[4], minute = clock_df$tzmin[4])
grid.text(clock_df$tz[4])
#grid.rect(gp=gpar(col="green"))