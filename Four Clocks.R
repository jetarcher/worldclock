grid.newpage()
vp <- viewport(width=1.0, height=0.25)
pushViewport(vp)
grid.rect(gp=gpar(col="blue"))
#grid.circle(x=0.0, y=0.0, default="native", 
#            r=unit(0.5, "native"))

grid.circle(x=0.125, y=0.5, default="native", 
            r=unit(0.5, "native"))
grid.circle(x=0.375, y=0.5, default="native", 
            r=unit(0.5, "native"))
grid.circle(x=0.625, y=0.5, default="native", 
            r=unit(0.5, "native"))
grid.circle(x=0.875, y=0.5, default="native", 
            r=unit(0.5, "native"))
#vp <- viewport(width=0.0, height=0.0)
vp <- viewport(width=1.0, height=0.25)
grid.rect(gp=gpar(col="red"))
grid.circle(x=0.0, y=0.0, default="native", 
            r=unit(0.25, "native"))
grid.circle(x=1.0, y=0.0, default="native", 
            r=unit(0.25, "native"))
grid.circle(x=0.0, y=1.0, default="native", 
            r=unit(0.25, "native"))
grid.circle(x=1.0, y=1.0, default="native", 
            r=unit(0.25, "native"))


