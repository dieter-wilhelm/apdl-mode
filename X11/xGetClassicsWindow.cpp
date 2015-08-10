/* g++ xSendReturn.cpp -lX11 -L/usr/X11R6/lib */
// use xev !

// Send a fake keystroke event to an X window.
// by Adam Pierce - http://www.doctort.org/adam/
// This is public domain software. It is free to use by anyone for any purpose.

#include <X11/Xlib.h>
//#include <X11/keysym.h>
// #include<unistd.h>		// for sleep
#include<string.h>
#include <X11/Xatom.h>
// #include <iostream>
// #include <cstring>
#include <cstdlib>
#include <cstdio>

using namespace std;

// // The key code to be sent.
// // A full list of available codes can be found in /usr/include/X11/keysymdef.h
// #define KEYCODE XK_v
// #define KEYCODE2 XK_Control_L
// #define KEYCODE3 XK_Return
// #define KEYCODE4 XK_Linefeed
// #define MODIFIER 4 		// bitwise inclusive OR of ControlMask

const char* win_name = "ANSYS Structural Utility Menu";
//const char* win_name = "wish";
char*  name;

// // Function to create a keyboard event
// XKeyEvent createKeyEvent(Display *display, Window &win,
//                            Window &winRoot, bool press,
//                            int keycode, int modifiers)
// {
//    XKeyEvent event;

//    event.display     = display;
//    event.window      = win;
//    event.root        = winRoot;
//    event.subwindow   = None;
//    event.time        = CurrentTime;
//    event.x           = 1;
//    event.y           = 1;
//    event.x_root      = 1;
//    event.y_root      = 1;
//    event.same_screen = True;
//    event.keycode     = XKeysymToKeycode(display, keycode);
//    event.state       = modifiers; // unsigned int

//    if(press)
//       event.type = KeyPress;
//    else
//       event.type = KeyRelease;

//    return event;
// }


Window *winList(Display *disp, unsigned long *len) {
    Atom prop = XInternAtom(disp,"_NET_CLIENT_LIST",False), type;
    int form;
    unsigned long remain;
    unsigned char *list;

    if (XGetWindowProperty(disp,XDefaultRootWindow(disp),prop,0,1024,False,XA_WINDOW,
                &type,&form,len,&remain,&list) != Success) {
        return 0;
    }

    return (Window*)list;
}


/* XFetchName? */
//    XGetWMName ()
char *winName( Display *disp, Window win) {
    Atom prop = XInternAtom(disp,"WM_NAME",False), type;
    int form;
    unsigned long remain, len;
    unsigned char *list;

    if (XGetWindowProperty(disp,win,prop,0,1024,False,XA_STRING,
                &type,&form,&len,&remain,&list) != Success) {
        return NULL;
    }

    return (char*)list;
}


main()
{
  // Obtain the X11 display.
   Display *display = XOpenDisplay(0);
   if(display == NULL)
      return -1;

   // Get the root window for the current display.
   Window winRoot = XDefaultRootWindow(display);

   // // Find the window which has the current keyboard focus.
   // Window winFocus;
   // int    revert;
   // XGetInputFocus(display, &winFocus, &revert);

   // here we are searching for the respective window
   /* search for another window and hand it the input focus */
   int i;
   unsigned long len;
   Window *list;

   list = (Window*)winList(display,&len);
   for (i=0;i<(int)len;i++) {
     name = winName(display,list[i]);
     // printf("%d :  %s, window %d \n",i,name,list[i]);
     if( strcmp(name,win_name) == 0){
       printf ("    -> found %s window after %d windows\n",win_name,i);
       //      XSendEvent(display,list[i],True,)
       break;
     }
   }
   if (i == (int)len){
     printf("\nWindow not found\n\n");
     exit(1);
   }

   /* XSync(display,True); */
   printf ("%d\n",list[i]);
   // Window winFocus2 = list[i];
   //  XSetInputFocus ( display, list[i], RevertToPointerRoot, CurrentTime);

   //   Window winFocus2 = 50331665;	// we know it ;-)
   // XSetInputFocus (display, winFocus2,RevertToPointerRoot,CurrentTime);

  //  // Send a Ctrl key press event to the window.
  //  XKeyEvent event = createKeyEvent(display, winFocus2, winRoot, True, KEYCODE2, 0);
  //  XSendEvent(event.display, event.window, True, KeyPressMask, (XEvent *)&event);
  //  //   XFlush(display);

  //  // Send a v key press event to the window.
  //  event = createKeyEvent(display, winFocus2, winRoot, True, KEYCODE, MODIFIER);
  //  XSendEvent(event.display, event.window, True, KeyPressMask, (XEvent *)&event);
  //  //   XFlush(display);

  //  // Send a v key release event to the window.
  //  event = createKeyEvent(display, winFocus2, winRoot, False, KEYCODE, MODIFIER);
  //  XSendEvent(event.display, event.window, True, KeyPressMask, (XEvent *)&event);
  //  //   XFlush(display);

  //  // Send a ctrl key release event to the window.
  //  event = createKeyEvent(display, winFocus2, winRoot, False, KEYCODE2, 0);
  //  XSendEvent(event.display, event.window, True, KeyPressMask, (XEvent *)&event);


  //  //XFlush(display);		// waiting until events are processed
  //  //  XSync(display,False);		// waiting until events are processed
  //  i = XPending (display);
  //  printf ("%d events in queue\n",i);
   
  //  //  printf("Sleeping ...\n");
  // //   sleep(5);				// sleep seconds
 
  //  printf ("Sending a line feed\n");
  //  // Send a fake key press event to the window.
  //  event = createKeyEvent(display, winFocus2, winRoot, True, KEYCODE4, 0);
  //  XSendEvent(event.display, event.window, True, KeyPressMask, (XEvent *)&event);
  //  //   XFlush(display);
  //  // Send a fake key release event to the window.
  //  event = createKeyEvent(display, winFocus2, winRoot, False, KEYCODE4, 0);
  //  XSendEvent(event.display, event.window, True, KeyPressMask, (XEvent *)&event);
  //  //   XFlush(display);

  //  //  XSync(display,True);		// waiting until events are processed
  //  //   XFlush(display);		// waiting until events are processed
 
  //  printf("Sending Return\n");
  //  // Send a fake key press event to the window.
  // //  XKeyEvent event = createKeyEvent(display, winFocus2, winRoot, True, KEYCODE2, 0);
  //  XKeyEvent event = createKeyEvent(display, winFocus2, winRoot, True, KEYCODE3, 0);
  //  XSendEvent(event.display, event.window, True, KeyPressMask, (XEvent *)&event);
  //  //   XFlush(display);
  //  // Send a fake key release event to the window.
  //  event = createKeyEvent(display, winFocus2, winRoot, False, KEYCODE3, 0);
  //  XSendEvent(event.display, event.window, True, KeyPressMask, (XEvent *)&event);
  //  // Send a fake key press event to the window.
  //  event = createKeyEvent(display, winFocus2, winRoot, True, KEYCODE3, 0);
  //  XSendEvent(event.display, event.window, True, KeyPressMask, (XEvent *)&event);
  //  //   XFlush(display);
  //  // Send a fake key release event to the window.
  //  event = createKeyEvent(display, winFocus2, winRoot, False, KEYCODE3, 0);
  //  XSendEvent(event.display, event.window, True, KeyPressMask, (XEvent *)&event);
   //   XFlush(display);

   //   XFlush(display);		// waiting until events are processed
   //   XSync(display,False);		// waiting until events are processed

   // XSync(display,False);		// waiting until events are processed
   // printf("waiting for return\n");

   // sleep(1);				// microseconds

   // // returning the focus to the caller window
   // XSetInputFocus (display, winFocus,RevertToPointerRoot,CurrentTime);
   // //   XFlush(display);


   // Done.
   XCloseDisplay(display);
   return 0;
}
