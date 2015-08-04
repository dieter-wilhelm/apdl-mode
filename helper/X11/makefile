ALL: xSendReturn xPasteToWin xGetFocusWindow

# -l library, -L search path
xGetFocusWindow : xGetFocusWindow.cpp
	g++ $<  -lX11 -L/usr/X11R6/lib -o $@
xSendReturn : xSendReturn.cpp
	g++ $<  -lX11 -L/usr/X11R6/lib -o $@
xPasteToWin : xPasteToWin.cpp
	g++ $<  -lX11 -L/usr/X11R6/lib -o $@
