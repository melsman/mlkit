#include <Common.h>
#include <System/SysAll.h>
#include <UI/UIAll.h>

#include "ri_sim.h"
#include "Region.h"

static Word cardNo = 1;
#define NUM_REGIONS 80
#define MAX_ALLOC (256*80)         // Words
static Regiondesc region_stack[NUM_REGIONS];
static ULong region_stat[NUM_REGIONS];
static ULong region_stack_idx = 0;

// Region statistics
static ULong num_alloc_reg = 0;     // Number of calls to alloc_region
static ULong num_dealloc_reg = 0;   // Number of calls to dealloc_region
static ULong num_alloc_in_reg = 0;  // Number of calls to alloc
static ULong num_reset_reg = 0;     // Number of calls to region_region
static ULong num_dealloc_until = 0; // Number of calls to dealloc_regions_until
static ULong total_alloc = 0;       // Total number of allocated words in regions

// Drawing histograms
static ULong from_idx=0;                   // Low watermark om change (region index).
static ULong to_idx=0;                     // High watermark on change (region index).
#define DEFAULT_SCALING (90.0)             // One pixel pr. byte.
static double scaling=DEFAULT_SCALING;     // Max. bytes shown.
static double old_scaling=DEFAULT_SCALING; // Old scaling.
static ULong max_reg_idx=0;                // Index of largest region.
static ULong old_max_reg_idx=0;            // Old index of largest region.

ULong tilf(ULong a, ULong b) {
  if (b < a)
    panic("b < a in tilf");
  return a + (SysRandom(0) % (b-a+1));
}

void recalc_stat() {
  int i, max=0;

  old_max_reg_idx = max_reg_idx;
  max_reg_idx = 0;
  total_alloc = 0;
  old_scaling = scaling;
  scaling = DEFAULT_SCALING;
  for (i=0;i<region_stack_idx;i++) {
    // Largest region idx.
    if (max<region_stat[i]) {
      max = region_stat[i];
      max_reg_idx = i;
    }

    // Total allocated.
    total_alloc += region_stat[i];

    // Find new scaling.
    if (scaling < region_stat[i])
      for (;scaling<region_stat[i];)
	scaling *= 2.0;
  }
}

ULong choose_region() {
  ULong seed = 0;
  if (region_stack_idx > 0) {
    seed = tilf(0,region_stack_idx-1);
    return seed + (region_stack_idx-1-seed) / 2;
  } else
    panic("No region to choose");
}

void alloc_reg() {
  if (region_stack_idx < NUM_REGIONS) {
    alloc_region(&region_stack[region_stack_idx]);
    region_stat[region_stack_idx] = 0;
    from_idx = region_stack_idx;
    to_idx = region_stack_idx;
    num_alloc_reg++;
    region_stack_idx++;
  }
}

void dealloc_reg() {
  if (region_stack_idx > 0) {
    dealloc_region();
    region_stack_idx--;
    from_idx = region_stack_idx;
    to_idx = region_stack_idx;
    region_stat[region_stack_idx] = 0;
    recalc_stat();
    num_dealloc_reg++;
  }
}

void alloc_in_reg() {
  int n = tilf(1,60);
  ULong idx;
  if (region_stack_idx > 0) {
    idx = choose_region();
    alloc((ULong)&region_stack[idx],n);
    region_stat[idx] += n;
    num_alloc_in_reg++;
    from_idx = idx;
    to_idx = idx;
    total_alloc += n;
    if (region_stat[idx] > region_stat[max_reg_idx] ||
	region_stat[idx] > scaling)
      recalc_stat();
  }
}

void reset_reg() {
  ULong idx;
  if (region_stack_idx > 0) {
    idx = choose_region();
    reset_region((ULong)&region_stack[idx]);
    total_alloc -= region_stat[idx];
    region_stat[idx] = 0;
    num_reset_reg++;
    from_idx = idx;
    to_idx = idx;
    if (idx = max_reg_idx)
      recalc_stat();
  }
}

void dealloc_reg_until() {
  int i;
  ULong idx;
  if (region_stack_idx > 0) {
    idx = choose_region();
    from_idx = idx;
    to_idx = region_stack_idx-1;
    for (i=from_idx;i<=to_idx;i++)
      region_stat[i] = 0;
    dealloc_regions_until((ULong)&region_stack[idx]);
    region_stack_idx = idx; // Region idx is also deallocated!
    recalc_stat();
    num_dealloc_until++;
  }
}

// We do 80% alloc and 20% alloc_region.
void sim_alloc() {
  ULong rnd = tilf(0,99);
  if (rnd < 80)
    alloc_in_reg();
  else
    alloc_reg();
}

// We do 2% dealloc regions until, 38% reset regions and 60% dealloc region
void sim_dealloc(int do_dealloc_reg_until) {
  ULong rnd = tilf(0,99);

  if (rnd < 2 && do_dealloc_reg_until)
    dealloc_reg_until();
  else
    if (rnd < 40)
      reset_reg();
    else
      dealloc_reg();
}

// The strategy we impose has two parameters: The maximun number of
// allocations MAX_ALLOC and the maximum number of regions
// NUM_REGIONS.  We do more allocations when we have not reached 90%
// of MAX_ALLOC and do most deallocations when we have allocated more
// than 90% of MAX_ALLOC.  We can never allocate more than MAX_ALLOC
// and NUM_REGIONS.  The region we allocate in is chosen as follows:
// Pick a region r randomly. The we allocate in the region between r
// and the top most region. This strategy is taken from Mads Toftes
// region simulator, see the homepage for the ML Kit version 2.
void sim_regs() {
  ULong rnd = tilf(0,99);
  if ((double)total_alloc < MAX_ALLOC*0.9) {
    // Do 95% allocations and 5% de-allocations.
    if (rnd < 5)
      sim_dealloc(0);
    else
      sim_alloc();
  } else {
    // Do 60% de-allocations and 40% allocations.
    if (rnd < 60)
      sim_dealloc(1);
    else
      sim_alloc();
  }
}


// make sure that The romversion parameter should be 0x03003000 or greater in call
// err = FtrGet(sysFtrCreator, sysFtrNumROMVersion, &romversion);

// Show a dynamic label on the form.
void show_lab(FormPtr form, Word labelID, CharPtr tmp_text) {
  Word label_index;

  label_index = FrmGetObjectIndex(form, labelID);
  FrmHideObject(form, label_index);
  FrmCopyLabel(form, labelID, tmp_text);
  FrmShowObject(form, label_index);

  return;
}

// Return the id of the button pressed.
static Word display_modal_form(FormPtr form) {
  FormPtr prev_form = FrmGetActiveForm();
  Word button_id;

  FrmSetActiveForm(form);
  
  // We do not need an eventhandler
  button_id = FrmDoDialog(form);
  
  // We do not read any values from the form.
  if (prev_form)
    FrmSetActiveForm(prev_form);
  FrmDeleteForm(form);
  
  return button_id;
}

Err show_heap_info() {

  Err err;
  UInt numHeaps, numRAMHeaps, version;
  UInt heapNo, heapId;
  ULong free, max, heapSize;
  Char tmp_text[100], tmp_textd1[100], tmp_textd2[100];
  FormPtr form;

  // Initialize the form.
  form = FrmInitForm(formID_heap_info);

  // Initialize the form.
  if (cardNo > MemNumCards()) {
    FrmAlert(alertID_no_card);
    return err;
  }

  numHeaps = MemNumHeaps(cardNo-1);
  StrPrintF(tmp_text, "Num. heaps: %u", numHeaps);
  show_lab(form, labelID_num_heaps, tmp_text);
  numRAMHeaps = MemNumRAMHeaps(cardNo-1);
  StrPrintF(tmp_text, "Num. RAM heaps: %u", numRAMHeaps);
  show_lab(form, labelID_num_RAM_heaps, tmp_text);

  for (heapNo=0; heapNo < numHeaps && heapNo < 3; heapNo++) {
      heapId = MemHeapID(cardNo-1,heapNo);
      heapSize = MemHeapSize(heapId);
      err = MemHeapFreeBytes(heapId, &free, &max);
      if (err) return err;
      if (heapNo < numRAMHeaps) {
	StrPrintF(tmp_text,
		  "RAM Heap %d is %s", 
		  heapNo, (MemHeapDynamic(heapId))?("dynamic"):("non-dynamic"));
	StrPrintF(tmp_textd1, "Free: %lu Kb.", free);
	StrPrintF(tmp_textd2, "Max. chunk: %lu Kb.", max);
      }
      else {
	StrPrintF(tmp_text, "ROM Heap %d", heapNo);
	StrPrintF(tmp_textd1, "Free: %lu Kb.", free);
	StrPrintF(tmp_textd2, "Max chunk: %lu Kb.", max);
      }
      switch (heapNo) {
      case 0: 
	show_lab(form, labelID_heap1, tmp_text);
	show_lab(form, labelID_heap1d1, tmp_textd1);
	show_lab(form, labelID_heap1d2, tmp_textd2);	
      case 1: 
	show_lab(form, labelID_heap2, tmp_text);
	show_lab(form, labelID_heap2d1, tmp_textd1);
	show_lab(form, labelID_heap2d2, tmp_textd2);
      default: 
	show_lab(form, labelID_heap3, tmp_text);
	show_lab(form, labelID_heap3d1, tmp_textd1);
	show_lab(form, labelID_heap3d2, tmp_textd2);
      }
  }
  display_modal_form(form);
  return err;
}

// Show card info in a modal form. Use global variable cardNo.
Err show_card_info() {
  Char cardName[32];
  Char manufName[32];
  ULong crDate, romSize, ramSize, freeBytes;
  Err err;
  UInt version;
  ULong free, max;
  Char tmp_text[100];

  // Initialize the form.
  FormPtr form = FrmInitForm(formID_card_info);

  // Initialize the form.
  if (cardNo > MemNumCards()) {
    FrmAlert(alertID_no_card);
    return err;
  }
  err = MemCardInfo (cardNo-1, cardName, manufName, &version, 
		     &crDate, &romSize, &ramSize, &freeBytes);
  if (err) {
    FrmAlert(alertID_no_card);
    return err;
  }

  StrPrintF(tmp_text, "Card no.: %u", cardNo);
  show_lab(form, labelID_card_info_no, tmp_text);
  StrPrintF(tmp_text, "Card name: %s", cardName);
  show_lab(form, labelID_card_name, tmp_text);
  StrPrintF(tmp_text, "Manu name: %s", manufName);
  show_lab(form, labelID_manu_name, tmp_text);
  StrPrintF(tmp_text, "Version: %u", version);
  show_lab(form, labelID_version, tmp_text);
  StrPrintF(tmp_text, "Cration date: %lu", crDate);
  show_lab(form, labelID_cr_date, tmp_text);
  StrPrintF(tmp_text, "ROM size: %lu Kb.", romSize / 1024);
  show_lab(form, labelID_ROM_size, tmp_text);
  StrPrintF(tmp_text, "RAM size: %lu Kb.", ramSize / 1024);
  show_lab(form, labelID_RAM_size, tmp_text);
  StrPrintF(tmp_text, "Free bytes: %lu Kb.", freeBytes / 1024);
  show_lab(form, labelID_free_mem, tmp_text);

  display_modal_form(form);

  return err;
}

// Get Memory card info.
Err mem_card_info(FormPtr form) {
  UInt numCards;
  Err err;
  Ptr startStack, endStack;
  Boolean stackOk;
  Char tmp_text[100];

  // Update the stack label text.
  stackOk = SysGetStackInfo(&startStack, &endStack);
  StrPrintF(tmp_text, "Addr: [%x,...,%x]", startStack, endStack);
  show_lab(form, labelID_stack_addr, tmp_text);

  if (stackOk)
    StrPrintF(tmp_text, "Size: %lu Kb.", (((ULong)endStack)-((ULong)startStack))/1024);
  else
    StrPrintF(tmp_text, "Size: %lu Kb. OVERFLOW", (((ULong)endStack)-((ULong)startStack)+1)/1024);
  show_lab(form, labelID_stack_size, tmp_text);

  // Show number of cards
  numCards = MemNumCards();
  StrPrintF(tmp_text, "NumCards: %u.", numCards);
  show_lab(form, labelID_card_no, tmp_text);

  return err;
}

void draw_histogram(int from_idx, int to_idx) {
  int i, y;
  char tmp_text[200];

  // Draw everything if scaling has changed.
  if (old_scaling != scaling) {
    from_idx = 0;
    to_idx = region_stack_idx;
  }
    
  // Draw regions as horizontal lines.
  for (i=from_idx;i<=to_idx;i=i+1) {
    y = 160-(int)(90.0*region_stat[i]/scaling);
    if (y<70) {
	StrPrintF(tmp_text, "scaling with y=%i, region_sta[i]=%lu and scaling %lu",y, region_stat[i],(ULong)scaling);
	panic(tmp_text);
    }
    if (i == max_reg_idx)
      WinDrawLine (i*2, y, i*2, 159);
    else
      WinDrawGrayLine (i*2, y, i*2, 159);
    WinEraseLine(i*2, 70, i*2, y-1);
  }
  if (old_max_reg_idx != max_reg_idx) {
    // Draw old max idx
    y = 160-(90*region_stat[old_max_reg_idx]/scaling);
    WinDrawGrayLine (old_max_reg_idx*2, y, old_max_reg_idx*2, 159);
    WinEraseLine(old_max_reg_idx*2, 70, old_max_reg_idx*2, y-1);
    // Draw new max idx
    y = 160-(90*region_stat[max_reg_idx]/scaling);
    WinDrawLine (max_reg_idx*2, y, max_reg_idx*2, 159);
    WinEraseLine(max_reg_idx*2, 70, max_reg_idx*2, y-1);
  }
}

void update_form_main_ri_sim() {
  Char tmp_text[100];
  FormPtr form;
  

  form = FrmGetActiveForm(); // FrmGetFormPtr(formID_main_ri_sim);
  StrPrintF(tmp_text, "Alloc: %lu b. #Reg: %lu.", total_alloc*4, region_stack_idx);
  show_lab(form, labelID_total_alloc, tmp_text);
  StrPrintF(tmp_text, "#AllocReg: %lu #DeallocReg: %lu", num_alloc_reg, num_dealloc_reg);
  show_lab(form, labelID_stat1, tmp_text);  
  StrPrintF(tmp_text, "#AllocInReg: %lu #ResetReg: %lu", num_alloc_in_reg, num_reset_reg);
  show_lab(form, labelID_stat2, tmp_text);
  StrPrintF(tmp_text, "#DeallocUntil: %lu", num_dealloc_until);
  show_lab(form, labelID_stat3, tmp_text);

  draw_histogram(from_idx, to_idx);
}

// All form-related events are handled here.
static Boolean sys_info_event(EventPtr event){
  FormPtr form;
  int handled = 0;

  switch (event->eType)   {
  case frmOpenEvent:
    form = FrmGetActiveForm();
    FrmDrawForm(form);
    mem_card_info(form);
    handled = 1;
    break;

  case ctlSelectEvent:  // A control button was pressed and released.
    switch (event->data.ctlSelect.controlID) {
    case pushbottomID_card1: cardNo = 1; handled = 1; break;
    case pushbottomID_card2: cardNo = 2; handled = 1; break;
    case pushbottomID_card3: cardNo = 3; handled = 1; break;
    case pushbottomID_card4: cardNo = 4; handled = 1; break;
    case pushbottomID_card5: cardNo = 5; handled = 1; break;
    case pushbottomID_card6: cardNo = 6; handled = 1; break;
    case buttonID_show_card_info: show_card_info(); handled = 1; break;
    case buttonID_show_heap_info: show_heap_info(); handled = 1; break;
    case buttonID_sys_info_done: 
      // form = FrmGetActiveForm();
      // FrmDeleteForm(form); Done automatically by FrmGotoForm via event frmCloseEvent
      FrmGotoForm(formID_main_ri_sim);
      break;
    default: break;
    }

  case nilEvent:
    // continue region simulation
    handled = 1;
    break;
  }
  return handled;
}

// All form-related events are handled here.
static Boolean ri_sim_event(EventPtr event){
  FormPtr form;
  int handled = 0;

  switch (event->eType)   {
  case frmOpenEvent:
    form = FrmGetActiveForm();
    FrmDrawForm(form);
    draw_histogram(0,region_stack_idx-1);
    handled = 1;
    break;

  case ctlSelectEvent:  // A control button was pressed and released.
    switch (event->data.ctlSelect.controlID) {
    default: break;
    }

  case menuEvent:
    switch (event->data.menu.itemID)    {
    case menuitemID_about:
      FrmAlert(alertID_about);
      handled = 1;
      break;
    case menuitemID_card_info:
      // form = FrmGetActiveForm();
      // FrmDeleteForm(form);
      FrmGotoForm(formID_sys_info);
      break;
    }
    break;

  case nilEvent:
    // continue region simulation
    sim_regs();
    update_form_main_ri_sim();
    handled = 1;
    break;
  }
  return handled;
}

void pp_event(EventType* event) {

  printf("Event: %d(", event->eType);
  switch (event->eType) {
  case nilEvent: printf("nilEvent"); break;
  case penDownEvent: printf("penDownEvent"); break;
  case penUpEvent: printf("penUpEvent"); break;
  case penMoveEvent: printf("penMoveEvent"); break;
  case keyDownEvent: printf("keyDownEvent"); break;
  case winEnterEvent: printf("winEnterEvent"); break;
  case winExitEvent: printf("winExitEvent"); break;
  case ctlEnterEvent: printf("ctlEnterEvent"); break;
  case ctlExitEvent: printf("ctlExitEvent"); break;
  case ctlSelectEvent: printf("ctlSelectEvent"); break;
  case ctlRepeatEvent: printf("ctlRepeatEvent"); break;
  case lstEnterEvent: printf("lstEnterEvent"); break;
  case lstSelectEvent: printf("lstSelectEvent"); break;
  case lstExitEvent: printf("lstExitEvent"); break;
  case popSelectEvent: printf("popSelectEvent"); break;
  case fldEnterEvent: printf("fldEnterEvent"); break;
  case fldHeightChangedEvent: printf("fldHeightChangedEvent"); break;
  case fldChangedEvent: printf("fldChangedEvent"); break;
  case tblEnterEvent: printf("tblEnterEvent"); break;
  case tblSelectEvent: printf("tblSelectEvent"); break;
  case daySelectEvent: printf("daySelectEvent"); break;
  case menuEvent: printf("menuEvent"); break;
  case appStopEvent: printf("appStopEvent"); break;
  case frmLoadEvent: printf("frmLoadEvent"); break;
  case frmOpenEvent: printf("frmOpenEvent"); break;
  case frmGotoEvent: printf("frmGotoEvent"); break;
  case frmUpdateEvent: printf("frmUpdateEvent"); break;
  case frmSaveEvent: printf("frmSaveEvent"); break;
  case frmCloseEvent: printf("frmCloseEvent"); break;
  case frmTitleEnterEvent: printf("frmTitleEnterEvent"); break;
  case frmTitleSelectEvent: printf("frmTitleSelectEvent"); break;
  case tblExitEvent: printf("tblExitEvent"); break;
  case sclEnterEvent: printf("sclEnterEvent"); break;
  case sclExitEvent: printf("sclExitEvent"); break;
  case sclRepeatEvent: printf("sclRepeatEvent"); break;
  default: printf("****unspecified****");
  }
  printf(")\n");
  return;
}

int event_loop(void) {
  Word error;
  EventType event;
  FormPtr form;
  int form_id;
  Regiondesc rd;

  do {
    EvtGetEvent(&event, 0); // No timeout - return immediately
    if (!SysHandleEvent(&event))
      if (!MenuHandleEvent(NULL, &event, &error))
	if (event.eType == frmLoadEvent) {
	  // Load and activate form.
	  form_id = event.data.frmLoad.formID;
	  form = FrmInitForm(form_id);
	  FrmSetActiveForm(form);
	  switch (form_id) {
	  case formID_main_ri_sim: FrmSetEventHandler(form, (FormEventHandlerPtr) ri_sim_event); break;
	  case formID_sys_info: FrmSetEventHandler(form, (FormEventHandlerPtr) sys_info_event); break;
	  }
	} else
	    FrmDispatchEvent(&event);
  } while (event.eType != appStopEvent);
}

int start_application(void) {

  int error = 0;

  // Initialize the runtime system
  init_runtime_system();

  // Get system-wide preferences. ri_sim does not need that.

  // Find the application's data file. If it doesn't exist, create it.

  // Get application-specific preferences.

  // Initialize other global variables.

  FrmGotoForm(formID_main_ri_sim);

  return error;
}

int stop_application(void) {

}

DWord PilotMain(Word cmd, Ptr cmdPBP, Word launchFlags) {

  EventType event;
  int error;

  if (cmd == sysAppLaunchCmdNormalLaunch) {

    error = start_application();
    if (error)
      return error;

    event_loop();
    stop_application();
  }
  return 0;
}
