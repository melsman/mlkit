/*  Experiment with the Form loading library */

#include <Common.h>
#include <System/SysAll.h>
#include <UI/UIAll.h>
#include "oneform.h"
//#include "pp_event.c"

/* Prototypes */
static void getEvent(EventType* event, FormPtr frm);
static void form1Setup(Ptr state);
static void form1EventHandler(Ptr state, FormPtr frm);

static void getEvent(EventType* event, FormPtr frm){
  Word error;
  Boolean ready = false;

  do {
    EvtGetEvent(event, evtWaitForever);
    if (!SysHandleEvent(event))
      if (!MenuHandleEvent(NULL, event, &error))
	if (! FrmHandleEvent(frm, event)) //Will translate pen to ctl
	  ready = true;
  } while (ready == false);
}


static void form1Setup(Ptr state) {
  FormPtr frm;

  frm = FrmInitForm(form1);
  FrmSetActiveForm(frm);
  FrmDrawForm(frm);
  form1EventHandler(state, frm);
}

static void form1EventHandler(Ptr state, FormPtr frm) {  
  EventType e;
  
  getEvent(&e, frm);
  switch(e.eType) {

  case appStopEvent:
    return ;

  case ctlSelectEvent:
    break; // do nothing when the button is pressed.

  default:
    form1EventHandler(state, frm);
  }
}

DWord PilotMain(Word cmd, Ptr cmdPBP, Word launchFlags){
  if (cmd == sysAppLaunchCmdNormalLaunch) {
    form1Setup(NULL);
  }
  return 0;
}
