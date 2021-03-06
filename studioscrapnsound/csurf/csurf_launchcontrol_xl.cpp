/*
** reaper_csurf
** Novation LaunchControl XL support (Based on MCU implementation)
** Author: Fredrik Lundstr�m, Studio Scrap 'N' Sound
** e-mail: fredrik.lundstrom.1974@gmail.com
** Copyright (C) 2006-2009 Cockos Incorporated
** Copyright (C) 2015 Fredrik Lundstr�m
** License: LGPL.
*/


#include "csurf.h"
#include "../../WDL/ptrlist.h"
#include "TrackFromGUID.h"

#ifdef _DEBUG
#define _FLU_DEBUG
#endif

#define _FLU_DEBUG_ONMIDIEVENT 0
#define _FLU_DEBUG_ONFADERMOVE 0
#define _FLU_DEBUG_SENDSETLEDCOLOR 0
#define _FLU_DEBUG_SETSENDLEDCOLORS 0
#define _FLU_DEBUG_HAS_SEND 0

#ifdef _FLU_DEBUG
static void ShowConsoleMsgF(const char *fmt, ...)
{
	char buffer[512];
	va_list(ap);
	va_start(ap, fmt);
	strcpy(buffer, "LCXL" _FLU_ARCH_S ": ");
	vsprintf(buffer + strlen(buffer), fmt, ap);
	ShowConsoleMsg(buffer);
}
#else
#define ShowConsoleMsgF(...) do { } while (0)
#endif

#define ELEMENTSOF(v) (sizeof(v) / sizeof(v[0]))

// Mute buttons			Amber
// Solo buttons			Green
// Record ARM buttons	Red

typedef enum {
	TRACKCONTROLSTATE_MUTE,
	TRACKCONTROLSTATE_SOLO,
	TRACKCONTROLSTATE_ARM,

	TRACKCONTROLSTATE_LAST // Last in list
} TrackControlState_e;

const char *g_track_control_state_s[TRACKCONTROLSTATE_LAST] = {
	"MUTE",
	"SOLO",
	"ARM"
};

/*
							GREEN	RED
off			0C	0000 1100	0		0

red low		0D	0000 1101	0		1
RED			0F	0000 1111	0		3
amber low	1D	0001 1101	1		1
AMBER		3F	0011 1111	3		3
yellow low	2D	0010 1101	2		1
YELLOW		3E	0011 1110	3		2
green low	1C	0001 1100	1		0
GREEN		3C	0011 1100	3		0

orange low	1E	00001 1110	1		2
ORANGE		2F	00010 1111	2		3
*/

typedef enum {
	LED_COLOR_OFF = 0x0C,
	LED_COLOR_RED_LOW = 0x0D,
	LED_COLOR_RED_FULL = 0x0F,
	LED_COLOR_RED_FULL_FLASH = ((LED_COLOR_RED_FULL & ~0x0C) | 0x08),
	LED_COLOR_ORANGE_LOW = 0x1E,
	LED_COLOR_ORANGE_FULL = 0x2F,
	LED_COLOR_ORANGE_FULL_FLASH = ((LED_COLOR_ORANGE_FULL & ~0x0C) | 0x08),
	LED_COLOR_AMBER_LOW = 0x1D,
	LED_COLOR_AMBER_FULL = 0x3F,
	LED_COLOR_AMBER_FULL_FLASH = ((LED_COLOR_AMBER_FULL & ~0x0C) | 0x08),
	LED_COLOR_YELLOW_LOW = 0x2D,
	LED_COLOR_YELLOW_FULL = 0x3E,
	LED_COLOR_YELLOW_FULL_FLASH = ((LED_COLOR_YELLOW_FULL & ~0x0C) | 0x08),
	LED_COLOR_GREEN_LOW = 0x1C,
	LED_COLOR_GREEN_FULL = 0x3C,
	LED_COLOR_GREEN_FULL_FLASH = ((LED_COLOR_GREEN_FULL & ~0x0C) | 0x08)
} led_color_e;

// The following LEDs only implements yellow color
#define LED_COLOR_MUTE   LED_COLOR_YELLOW_FULL
#define LED_COLOR_SOLO   LED_COLOR_YELLOW_FULL
#define LED_COLOR_ARM    LED_COLOR_YELLOW_FULL
#define LED_COLOR_DEVICE LED_COLOR_YELLOW_FULL

#define LED_TRACK_CONTROL_MUTE_ON	LED_COLOR_ORANGE_FULL
#define LED_TRACK_CONTROL_MUTE_OFF	LED_COLOR_AMBER_LOW
#define LED_TRACK_CONTROL_SOLO_ON	LED_COLOR_GREEN_FULL
#define LED_TRACK_CONTROL_SOLO_OFF	LED_COLOR_GREEN_LOW
#define LED_TRACK_CONTROL_ARM_ON	LED_COLOR_RED_FULL
#define LED_TRACK_CONTROL_ARM_OFF	LED_COLOR_RED_LOW

#define LED_TRACK_FOCUS_ON	LED_COLOR_YELLOW_FULL
#define LED_TRACK_FOCUS_OFF	LED_COLOR_AMBER_LOW

#define LED_TRACK_FOCUS_DEVICE_ON	LED_COLOR_RED_FULL
#define LED_TRACK_FOCUS_DEVICE_OFF	LED_COLOR_RED_LOW

#define LED_PAN_ON			LED_COLOR_GREEN_FULL

#define LED_TRACK_SELECT_OFF	LED_COLOR_OFF
#define LED_TRACK_SELECT_ON		LED_COLOR_RED_FULL


const char *g_led_names[] = {
	"SEND_A_1",
	"SEND_A_2",
	"SEND_A_3",
	"SEND_A_4",
	"SEND_A_5",
	"SEND_A_6",
	"SEND_A_7",
	"SEND_A_8",

	"SEND_B_1",
	"SEND_B_2",
	"SEND_B_3",
	"SEND_B_4",
	"SEND_B_5",
	"SEND_B_6",
	"SEND_B_7",
	"SEND_B_8",

	"PAN_1",
	"PAN_2",
	"PAN_3",
	"PAN_4",
	"PAN_5",
	"PAN_6",
	"PAN_7",
	"PAN_8",

	"TRACK_FOCUS_1",
	"TRACK_FOCUS_2",
	"TRACK_FOCUS_3",
	"TRACK_FOCUS_4",
	"TRACK_FOCUS_5",
	"TRACK_FOCUS_6",
	"TRACK_FOCUS_7",
	"TRACK_FOCUS_8",

	"TRACK_CONTROL_1",
	"TRACK_CONTROL_2",
	"TRACK_CONTROL_3",
	"TRACK_CONTROL_4",
	"TRACK_CONTROL_5",
	"TRACK_CONTROL_6",
	"TRACK_CONTROL_7",
	"TRACK_CONTROL_8",

	"DEVICE",
	"MUTE",
	"SOLO",
	"ARM",

	"SEND_SELECT_UP",
	"SEND_SELECT_DOWN",

	"TRACK_SELECT_LEFT",
	"TRACK_SELECT_RIGHT"
};

typedef enum {
	LED_SEND_A_1,
	LED_SEND_A_2,
	LED_SEND_A_3,
	LED_SEND_A_4,
	LED_SEND_A_5,
	LED_SEND_A_6,
	LED_SEND_A_7,
	LED_SEND_A_8,

	LED_SEND_B_1,
	LED_SEND_B_2,
	LED_SEND_B_3,
	LED_SEND_B_4,
	LED_SEND_B_5,
	LED_SEND_B_6,
	LED_SEND_B_7,
	LED_SEND_B_8,

	LED_PAN_1,
	LED_PAN_2,
	LED_PAN_3,
	LED_PAN_4,
	LED_PAN_5,
	LED_PAN_6,
	LED_PAN_7,
	LED_PAN_8,

	LED_TRACK_FOCUS_1,
	LED_TRACK_FOCUS_2,
	LED_TRACK_FOCUS_3,
	LED_TRACK_FOCUS_4,
	LED_TRACK_FOCUS_5,
	LED_TRACK_FOCUS_6,
	LED_TRACK_FOCUS_7,
	LED_TRACK_FOCUS_8,

	LED_TRACK_CONTROL_1,
	LED_TRACK_CONTROL_2,
	LED_TRACK_CONTROL_3,
	LED_TRACK_CONTROL_4,
	LED_TRACK_CONTROL_5,
	LED_TRACK_CONTROL_6,
	LED_TRACK_CONTROL_7,
	LED_TRACK_CONTROL_8,

	LED_DEVICE,
	LED_MUTE,
	LED_SOLO,
	LED_ARM,

	LED_SEND_SELECT_UP,
	LED_SEND_SELECT_DOWN,

	LED_TRACK_SELECT_LEFT,
	LED_TRACK_SELECT_RIGHT,

	LED_LAST
} led_e;


typedef enum {
	BUTTON_EVENT_NOTE_PRESSED	= 1 << 0,
	BUTTON_EVENT_NOTE_RELEASED	= 1 << 1,
	BUTTON_EVENT_CC_PRESSED		= 1 << 2,
	BUTTON_EVENT_CC_RELEASED	= 1 << 3,

	BUTTON_EVENT_INVALID		= 0x80
} button_event_e;

const char *getButtonEvent(button_event_e buttonevent) {
	switch (buttonevent) {
		case BUTTON_EVENT_NOTE_PRESSED:		return "NOTE_PRESSED";
		case BUTTON_EVENT_NOTE_RELEASED:	return "NOTE_RELEASED";
		case BUTTON_EVENT_CC_PRESSED:		return "CC_PRESSED";
		case BUTTON_EVENT_CC_RELEASED:		return "CC_RELEASED";
		default:
			return "BITMAP";
	}
}

typedef enum {
	// Note codes
	BUTTON_TRACK_FOCUS_1 = 0x29,
	BUTTON_TRACK_FOCUS_2 = 0x2A,
	BUTTON_TRACK_FOCUS_3 = 0x2B,
	BUTTON_TRACK_FOCUS_4 = 0x2C,
	BUTTON_TRACK_FOCUS_5 = 0x39,
	BUTTON_TRACK_FOCUS_6 = 0x3A,
	BUTTON_TRACK_FOCUS_7 = 0x3B,
	BUTTON_TRACK_FOCUS_8 = 0x3C,

	BUTTON_TRACK_CONTROL_1 = 0x49,
	BUTTON_TRACK_CONTROL_2 = 0x4A,
	BUTTON_TRACK_CONTROL_3 = 0x4B,
	BUTTON_TRACK_CONTROL_4 = 0x4C,
	BUTTON_TRACK_CONTROL_5 = 0x59,
	BUTTON_TRACK_CONTROL_6 = 0x5A,
	BUTTON_TRACK_CONTROL_7 = 0x5B,
	BUTTON_TRACK_CONTROL_8 = 0x5C,

	BUTTON_DEVICE = 0x69,
	BUTTON_MUTE = 0x6A,
	BUTTON_SOLO = 0x6B,
	BUTTON_ARM = 0x6C,
} button_note_e;

typedef enum {
	// CC code Bt bb vv, vv=7F for button down, vv=00 for button up
	BUTTON_SEND_SELECT_UP = 0x68,
	BUTTON_SEND_SELECT_DOWN = 0x69,

	BUTTON_TRACK_SELECT_LEFT = 0x6A,
	BUTTON_TRACK_SELECT_RIGHT = 0x6B,
} button_cc_e;

const char *getButtonName(button_event_e buttonevent, char bb)
{
	switch(buttonevent) {
		case BUTTON_EVENT_NOTE_PRESSED:
		case BUTTON_EVENT_NOTE_RELEASED:
			switch (bb) {
				case BUTTON_TRACK_FOCUS_1:		return "TRACK_FOCUS_1";
				case BUTTON_TRACK_FOCUS_2:		return "TRACK_FOCUS_2";
				case BUTTON_TRACK_FOCUS_3:		return "TRACK_FOCUS_3";
				case BUTTON_TRACK_FOCUS_4:		return "TRACK_FOCUS_4";
				case BUTTON_TRACK_FOCUS_5:		return "TRACK_FOCUS_5";
				case BUTTON_TRACK_FOCUS_6:		return "TRACK_FOCUS_6";
				case BUTTON_TRACK_FOCUS_7:		return "TRACK_FOCUS_7";
				case BUTTON_TRACK_FOCUS_8:		return "TRACK_FOCUS_8";

				case BUTTON_TRACK_CONTROL_1:	return "TRACK_CONTROL_1";
				case BUTTON_TRACK_CONTROL_2:	return "TRACK_CONTROL_2";
				case BUTTON_TRACK_CONTROL_3:	return "TRACK_CONTROL_3";
				case BUTTON_TRACK_CONTROL_4:	return "TRACK_CONTROL_4";
				case BUTTON_TRACK_CONTROL_5:	return "TRACK_CONTROL_5";
				case BUTTON_TRACK_CONTROL_6:	return "TRACK_CONTROL_6";
				case BUTTON_TRACK_CONTROL_7:	return "TRACK_CONTROL_7";
				case BUTTON_TRACK_CONTROL_8:	return "TRACK_CONTROL_8";

				case BUTTON_DEVICE:				return "DEVICE";
				case BUTTON_MUTE:				return "MUTE";
				case BUTTON_SOLO:				return "SOLO";
				case BUTTON_ARM:				return "ARM";
			}
			break;
		case BUTTON_EVENT_CC_PRESSED:
		case BUTTON_EVENT_CC_RELEASED:
			switch (bb) {
				case BUTTON_SEND_SELECT_UP:				return "SEND_SELECT_UP";
				case BUTTON_SEND_SELECT_DOWN:			return "SEND_SELECT_DOWN";
				case BUTTON_TRACK_SELECT_LEFT:			return "TRACK_SELECT_LEFT";
				case BUTTON_TRACK_SELECT_RIGHT:			return "TRACK_SELECT_RIGHT";
			}
			break;
	}
	return "INVALID";
}

const led_color_e g_send_color[] = {
	LED_COLOR_RED_FULL,
	LED_COLOR_ORANGE_FULL,
	LED_COLOR_GREEN_FULL,
	LED_COLOR_AMBER_FULL,
	LED_COLOR_YELLOW_FULL,

	LED_COLOR_RED_LOW,
	LED_COLOR_ORANGE_LOW,
	LED_COLOR_GREEN_LOW,
	LED_COLOR_AMBER_LOW,
	LED_COLOR_YELLOW_LOW
};

#define NUM_SEND_COLORS ELEMENTSOF(g_send_color)

bool hasSend(MediaTrack *tr, int send)
{
	int numSends = GetTrackNumSends(tr, 0); // Non-HW sends
	int numHWSends = GetTrackNumSends(tr, 1); // HW sends
#if _FLU_DEBUG_HAS_SEND > 0
	FIXID(id)
	ShowConsoleMsgF("hasSend id=%d numSends=%d numHWSends=%d\n", id, numSends, numHWSends);
#endif

	if (send < numSends + numHWSends) {
		return true;
	} else {
		return false;
	}
}

/*
C: double CSurf_OnSendVolumeChange(MediaTrack* trackid, int send_index, double volume, bool relative)
---
C: bool SetTrackSendUIVol(MediaTrack* track, int send_idx, double vol, int isend)
send_idx<0 for receives, isend = 1 for end of edit, -1 for an instant edit(such as reset), 0 for normal tweak.
---
C: bool GetTrackSendName(MediaTrack* track, int send_index, char* buf, int buf_sz)
---
C: bool GetTrackSendUIVolPan(MediaTrack* track, int send_index, double* volumeOut, double* panOut)
---
*/



#define SPLASH_MESSAGE "REAPER! Initializing... Please wait..."

static double charToVol(unsigned char val)
{
  double pos=((double)val*1000.0)/127.0;
  pos=SLIDER2DB(pos);
  return DB2VAL(pos);

}

static double int14ToVol(unsigned char msb, unsigned char lsb)
{
  int val=lsb | (msb<<7);
  double pos=((double)val*1000.0)/16383.0;
  pos=SLIDER2DB(pos);
  return DB2VAL(pos);
}
static double int7ToVol(unsigned char lsb)
{
  int val=lsb;
  double pos=((double)val*1000.0)/127.0;
  pos=SLIDER2DB(pos);
  return DB2VAL(pos);
}
static double int14ToPan(unsigned char msb, unsigned char lsb)
{
  int val=lsb | (msb<<7);
  return 1.0 - (val/(16383.0*0.5));
}

static double int7ToPan(unsigned char lsb)
{
  int val=lsb;
  return -(1.0 - (val/(127.0*0.5)));
}

static int volToInt14(double vol)
{
  double d=(DB2SLIDER(VAL2DB(vol))*16383.0/1000.0);
  if (d<0.0)d=0.0;
  else if (d>16383.0)d=16383.0;

  return (int)(d+0.5);
}
static  int panToInt14(double pan)
{
  double d=((1.0-pan)*16383.0*0.5);
  if (d<0.0)d=0.0;
  else if (d>16383.0)d=16383.0;

  return (int)(d+0.5);
}
static  unsigned char volToChar(double vol)
{
  double d=(DB2SLIDER(VAL2DB(vol))*127.0/1000.0);
  if (d<0.0)d=0.0;
  else if (d>127.0)d=127.0;

  return (unsigned char)(d+0.5);
}

static double charToPan(unsigned char val)
{
  double pos=((double)val*1000.0+0.5)/127.0;

  pos=(pos-500.0)/500.0;
  if (fabs(pos) < 0.08) pos=0.0;

  return pos;
}

static unsigned char panToChar(double pan)
{
  pan = (pan+1.0)*63.5;

  if (pan<0.0)pan=0.0;
  else if (pan>127.0)pan=127.0;

  return (unsigned char)(pan+0.5);
}

/*
static unsigned int get_midi_evt_code( MIDI_event_t *evt ) {
  unsigned int code = 0;
  code |= (evt->midi_message[0]<<24);
  code |= (evt->midi_message[1]<<16);
  code |= (evt->midi_message[2]<<8);
  code |= evt->size > 3 ? evt->midi_message[3] : 0;
  return code;
}
*/

class CSurf_LaunchControl_XL;
static WDL_PtrList<CSurf_LaunchControl_XL> m_launchcontrol_xl_list;
static bool g_csurf_mcpmode;
static int m_flipmode;
static int m_alllaunchcontrol_xls_bank_offset;
static const double g_panadj = (1.0/4); // Used by LCXLOnRotaryEncoder
static const double g_voladj = 11.0;// Used by LCXLOnRotaryEncoder

typedef void (CSurf_LaunchControl_XL::*ScheduleFunc)();

struct ScheduledAction {
  ScheduledAction( DWORD time, ScheduleFunc func ) {
    this->next = NULL;
    this->time = time;
    this->func = func;
  }
  
  ScheduledAction *next;
  DWORD time;
  ScheduleFunc func;
};

#define CONFIG_FLAG_FADER_TOUCH_MODE 1
#define CONFIG_FLAG_MAPF1F8TOMARKERS 2

#define DOUBLE_CLICK_INTERVAL 250 /* ms */

struct SelectedTrack {
  SelectedTrack( MediaTrack *tr ) {
    this->next = NULL;
    this->guid = *GetTrackGUID(tr);
  }
  MediaTrack *track() {
    return TrackFromGUID( this->guid );
  }
  SelectedTrack *next;
  GUID guid;
};

class CSurf_LaunchControl_XL : public IReaperControlSurface
{
    int m_midi_in_dev,m_midi_out_dev;
    int m_offset, m_size; // Size seems to be related to maximum number of strips on the MCU.
    midi_Output *m_midiout;
    midi_Input *m_midiin;

	TrackControlState_e m_track_control_state;
	TrackControlState_e m_track_control_state_only;
	bool m_device_mode;
	char m_current_template;

	double m_pan_lastchanges[256];
    int m_vol_lastpos[256];
    int m_pan_lastpos[256];
    char m_mackie_lasttime[10];
    int m_mackie_lasttime_mode;
    int m_mackie_modifiers;
    int m_cfg_flags;  //CONFIG_FLAG_FADER_TOUCH_MODE etc

    char m_fader_touchstate[256];
    unsigned int m_fader_lasttouch[256]; // m_fader_touchstate changes will clear this, moves otherwise set it. if set to -1, then totally disabled
    unsigned int m_pan_lasttouch[256];

    WDL_String m_descspace;
    char m_configtmp[1024];

    double m_launchcontrol_xl_meterpos[8];
    DWORD m_launchcontrol_xl_timedisp_lastforce, m_launchcontrol_xl_meter_lastrun;
    int m_mackie_arrow_states;
    unsigned int m_buttonstate_lastrun;
    unsigned int m_frameupd_lastrun;
    ScheduledAction *m_schedule;
    SelectedTrack *m_selected_tracks;
    
    // If user accidentally hits fader, we want to wait for user
    // to stop moving fader and then reset it to it's orginal position
    #define FADER_REPOS_WAIT 250
    bool m_repos_faders;
    DWORD m_fader_lastmove;
    
    int m_button_last;
    DWORD m_button_last_time;
    
    void scheduleAction( DWORD time, ScheduleFunc func ) {
      ScheduledAction *action = new ScheduledAction( time, func );
      if ( m_schedule == NULL ) {
        m_schedule = action;
      }
      else if ( action->time < m_schedule->time ) {
        action->next = m_schedule;
        m_schedule = action;
      }
      else {
        ScheduledAction *curr = m_schedule;
        while( curr->next != NULL && curr->next->time < action->time )
          curr = curr->next;
        action->next = curr->next;
        curr->next = action;
      }
    }


	const char *getLedName(led_e led) 
	{
		static char buffer[4][20];
		static int bufferidx;
		if (led < LED_LAST)
			return g_led_names[led];
		char *bufptr = &buffer[bufferidx][0];
		sprintf(bufptr, "INVALID led 0x%02X", led);
		bufferidx++;
		bufferidx %= 4;
		return bufptr;
	}

	const char *colorToString(char color)
	{
		const char *colors = NULL;
		static char buffers[8][40];
		static int bufferidx = 0;
		switch (color) {
			case LED_COLOR_OFF:					colors = "OFF"; break;

			case LED_COLOR_RED_LOW:				colors = "red"; break;
			case LED_COLOR_RED_FULL:			colors = "RED"; break;
			case LED_COLOR_RED_FULL_FLASH:		colors = "RED [FLASH]"; break;
			case LED_COLOR_ORANGE_LOW:			colors = "orange low"; break;
			case LED_COLOR_ORANGE_FULL:			colors = "ORANGE"; break;
			case LED_COLOR_ORANGE_FULL_FLASH:	colors = "ORANGE [FLASH]"; break;
			case LED_COLOR_AMBER_LOW:			colors = "amber"; break;
			case LED_COLOR_AMBER_FULL:			colors = "AMBER"; break;
			case LED_COLOR_AMBER_FULL_FLASH:	colors = "AMBER [FLASH]"; break;
			case LED_COLOR_YELLOW_LOW:			colors = "yellow low"; break;
			case LED_COLOR_YELLOW_FULL:			colors = "YELLOW"; break;
			case LED_COLOR_YELLOW_FULL_FLASH:	colors = "YELLOW [FLASH]"; break;
			case LED_COLOR_GREEN_LOW:			colors = "green"; break;
			case LED_COLOR_GREEN_FULL:			colors = "GREEN"; break;
			case LED_COLOR_GREEN_FULL_FLASH:	colors = "GREEN [FLASH]"; break;
			default:
				sprintf(buffers[bufferidx], "CUSTOM color 0x%02X", color);
				colors = buffers[bufferidx];
				bufferidx++;
				bufferidx %= 4;
		}
		sprintf(buffers[bufferidx], "%s", colors);
		const char *buffer = buffers[bufferidx];
		bufferidx ++;
		bufferidx %= 8;

		return buffer;
	}

	void LCXLSendSetLedColor(led_e led, char color)
	{
#if _FLU_DEBUG_SENDSETLEDCOLOR > 0
		ShowConsoleMsgF("LCXLSendSetLedColor %s %s\n",
			getLedName(led),
			colorToString(color));
#endif

		// Set LED color
		struct
		{
		  MIDI_event_t evt;
		  char data[11];
		}
		poo;
		poo.evt.frame_offset=0;
		poo.evt.size=11;
		poo.evt.midi_message[0]=0xF0;
		poo.evt.midi_message[1]=0x00;
		poo.evt.midi_message[2]=0x20;
		poo.evt.midi_message[3]=0x29;
		poo.evt.midi_message[4]=0x02;
		poo.evt.midi_message[5]=0x11;
		poo.evt.midi_message[6]=0x78;
		poo.evt.midi_message[7]=0x08;
		poo.evt.midi_message[8]=led;
		poo.evt.midi_message[9]=color;
		poo.evt.midi_message[10]=0xF7;
		Sleep(5);
		m_midiout->SendMsg(&poo.evt,-1);
	}

	void setTrackSelectColor(void)
	{
		int min = m_offset + m_alllaunchcontrol_xls_bank_offset;
		int max = m_offset + m_alllaunchcontrol_xls_bank_offset + m_size - 1;
		ShowConsoleMsgF("setTrackSelectColor: min=%d max=%d\n",
			min,
			max);
		int numTracks = GetNumTracks();
		bool hasVisible = false;
		for (int tidc = 1; tidc < min; tidc++) {
			if (isTrackVisible(tidc)) {
				LCXLSendSetLedColor(LED_TRACK_SELECT_LEFT, LED_TRACK_SELECT_ON);
				hasVisible = true;
				break;
			}
		}
		if (!hasVisible) {
			LCXLSendSetLedColor(LED_TRACK_SELECT_LEFT, LED_TRACK_SELECT_OFF);
		}

		hasVisible = false;
		for (int tidc = max+1; tidc <= numTracks; tidc++) {
			if (isTrackVisible(tidc)) {
				LCXLSendSetLedColor(LED_TRACK_SELECT_RIGHT, LED_TRACK_SELECT_ON);
				hasVisible = true;
				break;
			}
		}
		if (!hasVisible) {
			LCXLSendSetLedColor(LED_TRACK_SELECT_RIGHT, LED_TRACK_SELECT_OFF);
		}
	}

	// C: double GetMediaTrackInfo_Value(MediaTrack* tr, const char* parmname)
	// bool * B_SHOWINTCP : show track panel in tcp -- do not use on master
	//MediaTrack* GetTrack(ReaProject* proj, int trackidx)
	void setTrackControlStripColor(const bool states[256], char oncolor, char offcolor)
	{
		int min =  m_offset+m_alllaunchcontrol_xls_bank_offset;
		int max = m_offset+m_alllaunchcontrol_xls_bank_offset + m_size-1;
		if((m_track_control_state == m_track_control_state_only) || (m_track_control_state >= TRACKCONTROLSTATE_LAST))
			ShowConsoleMsgF("setTrackControlStripColor: min=%d max=%d on=%s off=%s\n",
					  min,
					  max,
					  colorToString(oncolor),
					  colorToString(offcolor)
					  );

		setTrackSelectColor();

		int numTracks = GetNumTracks();

		for (int id = 0; id < numTracks+8; id++) {
		  if(id >= min && id < max) {
			  int tid = id - min;
			  int tidc = id + 1;
			  led_e led_control = (led_e)(LED_TRACK_CONTROL_1 + tid);
			  led_e led_focus = (led_e)(LED_TRACK_FOCUS_1 + tid);
			  led_e led_pan = (led_e)(LED_PAN_1 + tid);
			  if ((m_track_control_state == m_track_control_state_only) || (m_track_control_state >= TRACKCONTROLSTATE_LAST))
					  ShowConsoleMsgF("setTrackControlStripColor: id=%d tid=%d tidc=%d %s=%s\n",
						  id, 
						  tid,
						  tidc,
						  getLedName(led_control),
						  (states[id] ? "ON" : "off"));
			  bool b_show = isTrackVisible(tidc);
			  if(id >= numTracks) {
				  b_show = false;
			  }
			  if (b_show) {
				  if (states[id]) {
					  LCXLSendSetLedColor(led_control, oncolor);
				  } else {
					  LCXLSendSetLedColor(led_control, offcolor);
				  }
				  if (!m_device_mode) {
					  LCXLSendSetLedColor(led_pan, LED_PAN_ON);
				  }
				  MediaTrack *tr = GetTrack(NULL, id);
				  if (tr) {
					  led_color_e led_color_on;
					  led_color_e led_color_off;
					  bool show_selected;
					  if (!m_device_mode) {
						  led_color_on = LED_TRACK_FOCUS_ON;
						  led_color_off = LED_TRACK_FOCUS_OFF;
						  show_selected = IsTrackSelected(tr);
					  }
					  else {
						  led_color_on = LED_TRACK_FOCUS_DEVICE_ON;
						  led_color_off = LED_TRACK_FOCUS_DEVICE_OFF;
						  show_selected = (tid == m_current_template) ? true : false;
					  }
					  if (show_selected) {
						  LCXLSendSetLedColor(led_focus, led_color_on);
					  } else {
						  LCXLSendSetLedColor(led_focus, led_color_off);
					  }
				  }
				  else {
					  ShowConsoleMsgF("setTrackControlStripColor: INVALID id=%d\n", id);
				  }
			  } else {
				  ShowConsoleMsgF("setTrackControlStripColor: tidc=%d not shown - HIDING\n", tidc);
				  LCXLSendSetLedColor(led_control, LED_COLOR_OFF);
				  if (!m_device_mode) {
					  LCXLSendSetLedColor(led_focus, LED_COLOR_OFF);
					  LCXLSendSetLedColor(led_pan, LED_COLOR_OFF);
				  } else {
					  led_color_e led_color_on = LED_TRACK_FOCUS_DEVICE_ON;
					  led_color_e led_color_off = LED_TRACK_FOCUS_DEVICE_OFF;
					  bool show_selected = (tid == m_current_template) ? true : false;
					  if (show_selected) {
						  LCXLSendSetLedColor(led_focus, led_color_on);
					  }
					  else {
						  LCXLSendSetLedColor(led_focus, led_color_off);
					  }
				  }
			  }
		  }
		}
	}

	void setTrackControlState(TrackControlState_e track_control_state)
	{
		ShowConsoleMsgF("setTrackControlState: %s m_device_mode=%s\n",
			(track_control_state < ELEMENTSOF(g_track_control_state_s) ? 
				g_track_control_state_s[track_control_state] :
				"INVALID state"
			),
			m_device_mode ? "DEVICE" : "OFF"
			);
		m_track_control_state = track_control_state;

		setSendLedColors();

		if (m_midiout)
		{
			LCXLSendSetLedColor(LED_DEVICE, m_device_mode ? LED_COLOR_DEVICE : LED_COLOR_OFF);
			// Set MUTE/SOLO/ARM function led
			for(int stateidx = 0; stateidx < TRACKCONTROLSTATE_LAST; stateidx++) {
				char color = 0x00;
				switch(stateidx) {
					case (TRACKCONTROLSTATE_MUTE):
						color = (m_track_control_state == stateidx ? LED_COLOR_MUTE : LED_COLOR_OFF);
						LCXLSendSetLedColor(LED_MUTE, color);
						if(m_track_control_state == stateidx) {
							setTrackControlStripColor(m_track_muted, LED_TRACK_CONTROL_MUTE_ON, LED_TRACK_CONTROL_MUTE_OFF);
						}
						break;
					case (TRACKCONTROLSTATE_SOLO):
						color = (m_track_control_state == stateidx ? LED_COLOR_SOLO : LED_COLOR_OFF);
						LCXLSendSetLedColor(LED_SOLO, color);
						if(m_track_control_state == stateidx) {
							setTrackControlStripColor(m_track_soloed, LED_TRACK_CONTROL_SOLO_ON, LED_TRACK_CONTROL_SOLO_OFF);
						}
						break;
					case (TRACKCONTROLSTATE_ARM):
						color = (m_track_control_state == stateidx ? LED_COLOR_ARM : LED_COLOR_OFF);
						LCXLSendSetLedColor(LED_ARM, color);
						if(m_track_control_state == stateidx) {
							setTrackControlStripColor(m_track_armed, LED_TRACK_CONTROL_ARM_ON, LED_TRACK_CONTROL_ARM_OFF);
						}
						break;
				}
			}
		}
	}

	void resetTemplate(void)
	{
		// Reset templates (LCXLOnly factory templates, keep user templates)
		for (int tidx = 8; tidx < 16; tidx++) {
			m_midiout->Send(0xB0 + tidx, 0x00, 0x00, -1);
		}

		// Select template
		struct
		{
			MIDI_event_t evt;
			char data[9];
		}
		poo;
		poo.evt.frame_offset = 0;
		poo.evt.size = 9;
		poo.evt.midi_message[0] = 0xF0;
		poo.evt.midi_message[1] = 0x00;
		poo.evt.midi_message[2] = 0x20;
		poo.evt.midi_message[3] = 0x29;
		poo.evt.midi_message[4] = 0x02;
		poo.evt.midi_message[5] = 0x11;
		poo.evt.midi_message[6] = 0x77;
		poo.evt.midi_message[7] = 0x08; // Factory Template 1.
		poo.evt.midi_message[8] = 0xF7;
		Sleep(5);
		m_midiout->SendMsg(&poo.evt, -1);
	}
    
    void LCXLReset()
    {
	  ShowConsoleMsgF("LCXLReset\n");
      memset(m_mackie_lasttime,0,sizeof(m_mackie_lasttime));
      memset(m_fader_touchstate,0,sizeof(m_fader_touchstate));
      memset(m_fader_lasttouch,0,sizeof(m_fader_lasttouch));
      memset(m_pan_lasttouch,0,sizeof(m_pan_lasttouch));
	  memset(m_pan_lastchanges, 0, sizeof(m_pan_lastchanges));
      m_mackie_lasttime_mode=-1;
      m_mackie_modifiers=0;
      m_buttonstate_lastrun=0;
      m_mackie_arrow_states=0;

      memset(m_vol_lastpos,0xff,sizeof(m_vol_lastpos));
      memset(m_pan_lastpos,0xff,sizeof(m_pan_lastpos));

	  m_device_mode = false;
	  m_current_template = 0;
	  m_track_control_state = TRACKCONTROLSTATE_MUTE;
	  m_track_control_state_only = TRACKCONTROLSTATE_MUTE; // 

      if (m_midiout)
      {
		resetTemplate();

		setTrackControlState(TRACKCONTROLSTATE_ARM);
#if 0

		m_midiout->Send(0x90, 0x32,m_flipmode?1:0,-1); // flip button
		m_midiout->Send(0x90, 0x33,g_csurf_mcpmode?0x7f:0,-1); // global view button

		m_midiout->Send(0x90, 0x64,(m_mackie_arrow_states&64)?0x7f:0,-1); // zoom button
		m_midiout->Send(0x90, 0x65,(m_mackie_arrow_states&128)?0x7f:0,-1); // scrub button

		m_midiout->Send(0xB0,0x40+11,'0'+(((m_alllaunchcontrol_xls_bank_offset+1)/10)%10),-1); // Right to left of LEDs
		m_midiout->Send(0xB0,0x40+10,'0'+((m_alllaunchcontrol_xls_bank_offset+1)%10),-1); // Right to left of LEDs
#endif
#if 0

        UpdateMackieDisplay(0,SPLASH_MESSAGE,56*2);
#endif

#if 0
		// Put tracks in meter mode
        int x;
        for (x = 0; x < 8; x ++)
        {
          struct
          {
            MIDI_event_t evt;
            char data[9];
          }
          poo;
          poo.evt.frame_offset=0;
          poo.evt.size=9;
          poo.evt.midi_message[0]=0xF0;
          poo.evt.midi_message[1]=0x00;
          poo.evt.midi_message[2]=0x00;
          poo.evt.midi_message[3]=0x66;
          poo.evt.midi_message[4]=0x14;
          poo.evt.midi_message[5]=0x20;
          poo.evt.midi_message[6]=0x00+x;
          poo.evt.midi_message[7]=0x03;
          poo.evt.midi_message[8]=0xF7;
          Sleep(5);
          m_midiout->SendMsg(&poo.evt,-1);
        }
        Sleep(5);
#endif 
#if 0
		// Update VU meter
        for (x = 0; x < 8; x ++)
        {
          m_midiout->Send(0xD0,(x<<4)|0xF,0,-1);
        }
#endif
      }

    }



    typedef bool (CSurf_LaunchControl_XL::*MidiHandlerFunc)(MIDI_event_t*);
    typedef bool (CSurf_LaunchControl_XL::*ButtonHandlerFunc)(button_event_e, char, MIDI_event_t*);

    
    bool LCXLOnTemplateChange(MIDI_event_t *evt) {
      const unsigned char onResetMsg[]={0xf0,0x00,0x20,0x29,0x02,0x11,0x77};
      if (evt->midi_message[8]==0xf7 && evt->size == sizeof(onResetMsg)+2 && !memcmp(evt->midi_message,onResetMsg,sizeof(onResetMsg)))
      {
		  if (evt->midi_message[7] != 0x08) {
			  // on reset
			  if (!m_device_mode) {
				  ShowConsoleMsgF("LCXLOnTemplateChange executing - template change from Factory Template 1 not allowed!\n");
				  LCXLReset();
			  }
			  else {
				  ShowConsoleMsgF("LCXLOnTemplateChange executing - In device mode; allowing template change\n");
			  }
		  } else {
			  m_current_template = evt->midi_message[7];
			  setTrackControlState(m_track_control_state);
			  TrackList_UpdateAllExternalSurfaces();
		  }
  		  return true;
      }
      return false;
    }
    
    bool LCXLOnFaderMove(MIDI_event_t *evt) {
	  bool senda_fader_move = false;
	  bool sendb_fader_move = false;
	  bool pan_fader_move = false;
	  bool volume_fader_move = false;
	  int tid= 8;
	  if ((evt->midi_message[0]&0xf0) == 0xB0
		  && evt->midi_message[1] >= 0x0D
		  && evt->midi_message[1] <= 0x14) {
		senda_fader_move = true;
		tid = evt->midi_message[1] - 0x0D;
	  } else if ((evt->midi_message[0]&0xf0) == 0xB0
		  && evt->midi_message[1] >= 0x1D
		  && evt->midi_message[1] <= 0x24) {
		sendb_fader_move = true;
		tid = evt->midi_message[1] - 0x1D;
	  } else if ((evt->midi_message[0]&0xf0) == 0xB0
		  && evt->midi_message[1] >= 0x31
		  && evt->midi_message[1] <= 0x38) {
		pan_fader_move = true;
		tid = evt->midi_message[1] - 0x31;
	  } else if ((evt->midi_message[0]&0xf0) == 0xB0
		  && evt->midi_message[1] >= 0x4D
		  && evt->midi_message[1] <= 0x54) {
		volume_fader_move = true;
		tid = evt->midi_message[1] - 0x4D;
	  }

	  double panlvl = int7ToPan(evt->midi_message[2]);
	  double vollvl = int7ToVol(evt->midi_message[2]);
	  int tidc = tid;
      if (tid == 8) tidc=0; // master offset, master=0
      else tidc = tid + 1+m_offset+m_alllaunchcontrol_xls_bank_offset;

	  if (!m_device_mode && (volume_fader_move || pan_fader_move)) // volume fader move
      {
#if _FLU_DEBUG_ONFADERMOVE > 0 
	  ShowConsoleMsgF("LCXLOnFaderMove %s tid=%u (tidc=%u) %s=%f\n", 
		  (volume_fader_move ? "VOLUME" : "PAN   "),
		  tid,
		  tidc,
		  (volume_fader_move ? "vollvl" : "panlvl"),
		  (volume_fader_move ? vollvl : panlvl)
		  );
#endif
        m_fader_lastmove = timeGetTime();

        if (tid>=0&&tid<9 && m_fader_lasttouch[tid]!=0xffffffff)
          m_fader_lasttouch[tid]=m_fader_lastmove;

        MediaTrack *tr=CSurf_TrackFromID(tidc,g_csurf_mcpmode);

        if (tr)
        {
          if ( (m_cfg_flags&CONFIG_FLAG_FADER_TOUCH_MODE) && !GetTouchState(tr) ) {
            m_repos_faders = true;
#if _FLU_DEBUG_ONFADERMOVE > 0
			ShowConsoleMsgF("LCXLOnFaderMove CONFIG_FLAG_FADER_TOUCH_MODE enabled, skipping\n");
#endif
          }
          else if ((m_flipmode && volume_fader_move) || (!m_flipmode && pan_fader_move))
          {
		    //ShowConsoleMsgF("LCXLOnFaderMove invoking CSurf_SetSurfacePan(CSurf_OnPanChange(panlvl=%f))\n", panlvl);
            CSurf_SetSurfacePan(tr,CSurf_OnPanChange(tr,panlvl,false),NULL);
          }
          else 
		  {
		    //ShowConsoleMsgF("LCXLOnFaderMove invoking CSurf_SetSurfaceVolume(CSurf_OnVolumeChange(vollvl=%f))\n", vollvl);
            CSurf_SetSurfaceVolume(tr,CSurf_OnVolumeChange(tr,vollvl,false),NULL);
		  }
		} else {
			ShowConsoleMsgF("LCXLOnFaderMove INVALID track (but returning true)\n");
		}
        return true;
	  } else {
		  // TODO: bool SetTrackSendUIVol(MediaTrack* track, int send_idx, double vol, int isend)
#if _FLU_DEBUG_ONFADERMOVE > 0
	    ShowConsoleMsgF("LCXLOnFaderMove %s tid=%u (tidc=%u) lvl=%f (CURRENTLY IGNORED)\n", 
			(senda_fader_move ? "SEND A" :
			(sendb_fader_move ? "SEND B" :
			(volume_fader_move ? "VOLUME" :
			(pan_fader_move ? "PAN" : "UNKNOWN")))),
			tid,
		  tidc,
		  panlvl
		  );
#endif
		return false; // Simply ignore so far...
	  }

	  ShowConsoleMsgF("LCXLOnFaderMove ignored\n");
	  return false;
    }

	bool LCXLOnRotaryEncoder( MIDI_event_t *evt ) {
		bool pan = false;
	  if ( (evt->midi_message[0]&0xf0) == 0xb0 && 
	      evt->midi_message[1] >= 0x10 && 
	      evt->midi_message[1] < 0x18 ) // pan
	  {
		  pan = true;
	  }
	  else {
		ShowConsoleMsgF("LCXLOnRotaryEncoder non-pan invoked\n");
	  }
	  if (!m_device_mode && pan) {
	    int tid=evt->midi_message[1]-0x10;
		int tidc = tid;

	    m_pan_lasttouch[tid&7]=timeGetTime();

	    if (tid == 8) tidc=0; // adjust for master
	    else tidc =tid + 1+m_offset+m_alllaunchcontrol_xls_bank_offset;
	    MediaTrack *tr=CSurf_TrackFromID(tidc,g_csurf_mcpmode);
	    if (tr)
	    {
	      double adj=(evt->midi_message[2]&0x3f)/31.0;
		  bool restored = false;

	      if (evt->midi_message[2]&0x40) adj=-adj;
		    double voladjc = 0;
			double vollvl = 0;
			if(m_flipmode) {
  				voladjc = adj*g_voladj;
				vollvl=CSurf_OnVolumeChange(tr,voladjc,true);
			}

  		    // Correct for REAPER center-lock when panning close to center
  		    double panadjc = adj*g_panadj;
		    m_pan_lastchanges[tid] +=panadjc;
			// Note! This will  essentially invoke CSurf_OnPanChange twice, so g_panadj must be 2 times larger 
			double panlvl=CSurf_OnPanChange(tr,panadjc,true);
			// 0.0 indicates centered
			if(panlvl == 0.0 && (m_pan_lastchanges[tid] > 5 * m_pan_lastchanges[tid] || m_pan_lastchanges[tid] < 5 * m_pan_lastchanges[tid&7])) {
				panadjc /= g_panadj; // Restore to original adj
				m_pan_lastchanges[tid] = panadjc;
				restored = true;
			}

// Nu fugerar det hyffsat: encodern blir korrekt linj�r fr�n 100% L till center samt fr�n 100% R till center
// Men efter centreringen s� v�grar CSurf_OnVolumeChange() returnera annat �n 0.0 tills dess att rotaryn g�tt i botten. (?)
// Bug i reaper!?

		  ShowConsoleMsgF("LCXLOnRotaryEncoder executed %s tid=%d (tidc=%d) adj=%f "
			  "(adjc=%f) lvl=%f "
			  "m_flipmode=%s restored=%s\n",
			(pan ? "PAN   " : "volume"),
			tid,
			tidc,
			adj,
			(pan ? panadjc : voladjc),
			(pan ? panlvl : vollvl),
			(m_flipmode ? "TRUE" : "false"),
			(restored ? "TRUE" : "false")
			);
	      if (m_flipmode)
	      {
	        CSurf_SetSurfaceVolume(tr,vollvl,NULL);
	      }
	      else
	      {
	        CSurf_SetSurfacePan(tr,CSurf_OnPanChange(tr,panadjc,true),NULL);
	      }
		} else {
		ShowConsoleMsgF("LCXLOnRotaryEncoder %s INVALID track for tid=%d (tidc=%d)\n",
			(pan ? "PAN   " : "volume"),
			tid,
			tidc
			);
		}
	    return true;
	  } else {
	  ShowConsoleMsgF("LCXLOnRotaryEncoder ignored\n"); 
		  return false;
	  }
	  ShowConsoleMsgF("LCXLOnRotaryEncoder ignored\n"); 

	  return false;
	}

	
	bool LCXLOnBankChannelButton(button_event_e buttonevent, char bb, MIDI_event_t *evt ) {
		ShowConsoleMsgF("LCXLOnBankChannelButton bb=%d (%s)\n",
			bb,
			(bb == BUTTON_TRACK_SELECT_LEFT ? "LEFT" :
			(bb == BUTTON_TRACK_SELECT_RIGHT ? "RIGHT" :  "INVALID")));
	  int maxfaderpos=0;
	  int movesize=8;
	  int x;
	  for (x = 0; x < m_launchcontrol_xl_list.GetSize(); x ++)
	  {
	    CSurf_LaunchControl_XL *item=m_launchcontrol_xl_list.Get(x);
	    if (item)
	    {
	      if (item->m_offset+8 > maxfaderpos)
	        maxfaderpos=item->m_offset+8;
	    }
	  }

//	  if (evt->midi_message[1]>=0x30) movesize=1;
//	  else  movesize=8; // maxfaderpos?
	  movesize=8;


	  if (bb == BUTTON_TRACK_SELECT_RIGHT) // increase by movesize
	  {
	    int msize=CSurf_NumTracks(g_csurf_mcpmode);
		ShowConsoleMsgF("LCXLOnBankChannelButton RIGHT msize=%d movesize=%d m_alllaunchcontrol_xls_bank_offset=%d maxfaderpos=%d\n",
			msize,
			movesize,
			m_alllaunchcontrol_xls_bank_offset,
			maxfaderpos);
	    if (movesize>1)
	    {
//	      if (m_alllaunchcontrol_xls_bank_offset+maxfaderpos >= msize) return true;
	    }
	    
		if (m_alllaunchcontrol_xls_bank_offset+movesize < msize) {
			m_alllaunchcontrol_xls_bank_offset+=movesize;
		} else {
			ShowConsoleMsgF("LCXLOnBankChannelButton attempt to move right of last bank IGNORED\n");
			return true;
		}
	  }
	  else // decrease by movesize
	  {
		ShowConsoleMsgF("LCXLOnBankChannelButton LEFT movesize=%d m_alllaunchcontrol_xls_bank_offset=%d maxfaderpos=%d\n",
			movesize,
			m_alllaunchcontrol_xls_bank_offset,
			maxfaderpos);
	    m_alllaunchcontrol_xls_bank_offset-=movesize;
		if (m_alllaunchcontrol_xls_bank_offset<0){
			ShowConsoleMsgF("LCXLOnBankChannelButton attempt to move left of first bank IGNORED\n");
			m_alllaunchcontrol_xls_bank_offset=0;
			return true;
		}
	  }
	  ShowConsoleMsgF("LCXLOnBankChannelButton New values movesize=%d m_alllaunchcontrol_xls_bank_offset=%d\n",
			movesize,
			m_alllaunchcontrol_xls_bank_offset,
			maxfaderpos);
	  // update all of the sliders
	  setTrackControlState(m_track_control_state);
	  TrackList_UpdateAllExternalSurfaces();

	  for (x = 0; x < m_launchcontrol_xl_list.GetSize(); x ++)
	  {
	    CSurf_LaunchControl_XL *item=m_launchcontrol_xl_list.Get(x);
	    if (item && item->m_midiout)
	    {
#if 0
	      item->m_midiout->Send(0xB0,0x40+11,'0'+(((m_alllaunchcontrol_xls_bank_offset+1)/10)%10),-1);
	      item->m_midiout->Send(0xB0,0x40+10,'0'+((m_alllaunchcontrol_xls_bank_offset+1)%10),-1);
#endif
	    }
	  }
	  return true;
	}
	



	bool LCXLOnRecArmButton( MIDI_event_t *evt ) {
		ShowConsoleMsgF("LCXLOnRecArmButton\n");
	  int tid=evt->midi_message[1];
	  tid+=1+m_alllaunchcontrol_xls_bank_offset+m_offset;
	  MediaTrack *tr=CSurf_TrackFromID(tid,g_csurf_mcpmode);
	  if (tr && isTrackVisible(tid))
	    CSurf_SetSurfaceRecArm(tr, CSurf_OnRecArmChange(tr,-1), NULL);
	  return true;
	}
	
	bool LCXLOnMuteButton( MIDI_event_t *evt ) {
		ShowConsoleMsgF("LCXLOnMuteButton\n");
	  int tid=evt->midi_message[1];
	  tid+=1+m_alllaunchcontrol_xls_bank_offset+m_offset;
	  MediaTrack *tr=CSurf_TrackFromID(tid,g_csurf_mcpmode);
	  if (tr && isTrackVisible(tid))
	    CSurf_SetSurfaceMute(tr,CSurf_OnMuteChange(tr,-1),NULL);
	  return true;
	}
	
	bool LCXLOnSoloButton( MIDI_event_t *evt ) {
		ShowConsoleMsgF("LCXLOnSoloButton\n");
	  int tid=evt->midi_message[1];
	  tid+=1+m_alllaunchcontrol_xls_bank_offset+m_offset;
	  MediaTrack *tr=CSurf_TrackFromID(tid,g_csurf_mcpmode);
	  if (tr && isTrackVisible(tid))
	    CSurf_SetSurfaceSolo(tr,CSurf_OnSoloChange(tr,-1),NULL);
	  return true;
	}



	int m_send_offset = 0;
	int m_num_sends = 2;

	bool LCXLOnDeviceButton(button_event_e buttonevent, char bb, MIDI_event_t *evt) {
		if (buttonevent == BUTTON_EVENT_NOTE_PRESSED) {
			m_device_mode = !m_device_mode;
			if (!m_device_mode) {
				resetTemplate();
			}
			setTrackControlState(m_track_control_state);
		}
		return true;
	}

	void setSendLedColors(void)
	{
		int numTracks = GetNumTracks();
		int min = m_offset + m_alllaunchcontrol_xls_bank_offset;
		int max = m_offset + m_alllaunchcontrol_xls_bank_offset + m_size - 1;
		ShowConsoleMsgF("setSendLedColors min=%d max=%d g_csurf_mcpmode=%s\n",
			min,
			max,
			g_csurf_mcpmode ? "TRUE" : "false");
			
		for (int tidc = 1; tidc <= max; tidc++) {
#if _FLU_DEBUG_SETSENDLEDCOLORS > 0
			ShowConsoleMsgF("setSendLedColors tidc=%d/%d\n",
				tidc,
				numTracks,
				g_csurf_mcpmode ? "TRUE" : "false"
				);
#endif
			if (tidc >= min+1 && tidc <= max) {
				int tid = tidc - min-1;
				MediaTrack *tr = CSurf_TrackFromID(tidc, g_csurf_mcpmode);
				if (tidc <= numTracks) {
					if (tr) {
						for (int row = 0; row < m_num_sends; row++)  {
							led_color_e color = LED_COLOR_OFF;
							led_e led = (led_e)(row * 8 + tid);
							if (!m_device_mode) {
								if (isTrackVisible(tidc)) {
									int sendidx = m_send_offset + row;
									bool has_send = hasSend(tr, sendidx);
									if (has_send) {
										if (sendidx < NUM_SEND_COLORS) {
											color = g_send_color[sendidx];
										}
										else {
											color = LED_COLOR_RED_FULL;
										}
										ShowConsoleMsgF("setSendLedColors: tidc=%d %s %s\n",
											tidc,
											getLedName(led),
											colorToString(color)
											);
									}
								}
								else {
									if (row == 0) {
										// Just one log entry per track
										ShowConsoleMsgF("setSendLedColors tidc=%d not shown - HIDING\n", tidc);
									}
								}
								LCXLSendSetLedColor(led, color);
							} else {
								// TODO: Device mode
							}
						}
					} else {
						ShowConsoleMsgF("setSendLedColors tidc=%d INVALID track!\n", tidc);
					}
				} else {
					for (int row = 0; row < m_num_sends; row++)  {
						led_e led = (led_e)(row * 8 + tid);
						if (!m_device_mode) {
							LCXLSendSetLedColor(led, LED_COLOR_OFF);
						} else {
							// TODO: Device mode
						}
					}
				}
			}
		}
	}


	bool LCXLOnChannelSelectButton(button_event_e buttonevent, char bb, MIDI_event_t *evt ) {
	  int tid = buttonToTrackId(bb);
	  int tidc = tid & 7;
	  tidc+=1+m_alllaunchcontrol_xls_bank_offset+m_offset;
	  ShowConsoleMsgF("LCXLOnChannelSelectButton tid=%d tidc=%d g_csurf_mcpmode=%s\n",
		  tid, 
		  tidc, 
		  g_csurf_mcpmode ? "TRUE" : "false"
	  );
	  
	  MediaTrack *tr=CSurf_TrackFromID(tidc,g_csurf_mcpmode);
	  if (tr) {
		  if (isTrackVisible(tidc)) {
			  CSurf_OnSelectedChange(tr, -1); // this will automatically update the surface
		  } else {
			  ShowConsoleMsgF("LCXLOnChannelSelectButton: HIDDEN track, ignoring...!\n");
		  }
	  } else {
		  ShowConsoleMsgF("LCXLOnChannelSelectButton: INVALID track!\n");
	  }
	  return true;
	}
	










	bool LCXLOnMuteButtonSoloArmButton(button_event_e buttonevent, char bb, MIDI_event_t *evt ) {
		ShowConsoleMsgF("LCXLOnMuteButtonSoloArmButton button=0x%02X (%s)\n", bb, getButtonName(buttonevent, bb));
	  switch (bb) {
		  case BUTTON_MUTE:
			  setTrackControlState(TRACKCONTROLSTATE_MUTE);
			  break;
		  case BUTTON_SOLO:
			  setTrackControlState(TRACKCONTROLSTATE_SOLO);
			  break;
		  case BUTTON_ARM:
			  setTrackControlState(TRACKCONTROLSTATE_ARM);
			  break;
		  default:
			  return false;
	  }
	  return true;
	}

	int buttonToTrackId(char bb)
	{
		switch(bb) {
			case BUTTON_TRACK_FOCUS_1:
			case BUTTON_TRACK_CONTROL_1:	return 0;

			case BUTTON_TRACK_FOCUS_2:
			case BUTTON_TRACK_CONTROL_2:	return 1;

			case BUTTON_TRACK_FOCUS_3:
			case BUTTON_TRACK_CONTROL_3:	return 2;

			case BUTTON_TRACK_FOCUS_4:
			case BUTTON_TRACK_CONTROL_4:	return 3;

			case BUTTON_TRACK_FOCUS_5:
			case BUTTON_TRACK_CONTROL_5:	return 4;

			case BUTTON_TRACK_FOCUS_6:
			case BUTTON_TRACK_CONTROL_6:	return 5;

			case BUTTON_TRACK_FOCUS_7:
			case BUTTON_TRACK_CONTROL_7:	return 6;

			case BUTTON_TRACK_FOCUS_8:
			case BUTTON_TRACK_CONTROL_8:	return 7;

			default:
				return -1;
		}
	}
	bool LCXLOnTrackControlButton(button_event_e buttonevent, char bb, MIDI_event_t *evt )
	{
		ShowConsoleMsgF("LCXLOnTrackControlButton button=0x%02X (%s)\n", bb, getButtonName(buttonevent, bb));

		int tid = buttonToTrackId(bb);
		if(tid == -1) {
			ShowConsoleMsgF("LCXLOnTrackControlButton non-track button\n", bb, getButtonName(buttonevent, bb));
			return false;
		}
		evt->midi_message[1] = tid;

		switch (m_track_control_state) {
			case TRACKCONTROLSTATE_MUTE:
				return LCXLOnMuteButton(evt);
			case TRACKCONTROLSTATE_SOLO:
				return LCXLOnSoloButton(evt);
			case TRACKCONTROLSTATE_ARM:
				return LCXLOnRecArmButton(evt);
				break;
			default:
				ShowConsoleMsgF("LCXLOnTrackControlButton INVALID m_track_control_state\n");
		}
		return false;
	}


	struct ButtonHandler {
	  button_event_e eventmask;
	  unsigned int evt_min;
	  unsigned int evt_max; // inclusive
	  ButtonHandlerFunc func;
	  ButtonHandlerFunc func_dc;
	};

	bool LCXLOnButtonPress( MIDI_event_t *evt ) {
		button_event_e button_event = BUTTON_EVENT_INVALID;
		if ((evt->midi_message[0] & 0xF0) == 0x80) {
			button_event = BUTTON_EVENT_NOTE_RELEASED;
		} else if ((evt->midi_message[0] & 0xF0) == 0x90) {
			button_event = BUTTON_EVENT_NOTE_PRESSED;
		} else if ((evt->midi_message[0] & 0xF0) == 0xB0 && evt->midi_message[2] == 0x7F) {
			button_event = BUTTON_EVENT_CC_PRESSED;
		} else if ((evt->midi_message[0] & 0xF0) == 0xB0 && evt->midi_message[2] == 0x00) {
			button_event = BUTTON_EVENT_CC_RELEASED;
		} else {
			button_event = BUTTON_EVENT_INVALID;
		}

		ShowConsoleMsgF("LCXLOnButtonPress invoked button_event=0x%02X (%s) button=0x%02X (%s) velocity=%d\n",
			button_event,
			getButtonEvent(button_event),
			evt->midi_message[1],
			getButtonName(button_event, evt->midi_message[1]),
			evt->midi_message[2]);

	    if (button_event == BUTTON_EVENT_INVALID)  
	      return false;

	    static const ButtonHandler handlers[] = {
//	      { BUTTON_EVENT_NOTE_PRESSED, 0x4a, 0x4e, &CSurf_LaunchControl_XL::LCXLOnAutoMode,           NULL },
	      { BUTTON_EVENT_CC_PRESSED,   0x6A, 0x6B, &CSurf_LaunchControl_XL::LCXLOnBankChannelButton,        NULL },
		  { (button_event_e)(BUTTON_EVENT_NOTE_PRESSED | BUTTON_EVENT_NOTE_RELEASED),
									   0x69, 0x69, &CSurf_LaunchControl_XL::LCXLOnDeviceButton, NULL },
//		  { BUTTON_EVENT_NOTE_PRESSED, 0x35, 0x35, &CSurf_LaunchControl_XL::LCXLOnSMPTEBeats,         NULL },
//        { BUTTON_EVENT_NOTE_PRESSED, 0x20, 0x27, &CSurf_LaunchControl_XL::LCXLOnRotaryEncoderPush,  NULL },
//        { BUTTON_EVENT_NOTE_PRESSED, 0x00, 0x07, &CSurf_LaunchControl_XL::LCXLOnRecArmButton,             NULL },
//        { BUTTON_EVENT_NOTE_PRESSED, 0x08, 0x0f, NULL,                                          &CSurf_LaunchControl_XL::LCXLOnSoloButtonDC },
//        { BUTTON_EVENT_NOTE_PRESSED, 0x08, 0x17, &CSurf_LaunchControl_XL::LCXLOnMuteButtonSolo,           NULL },
		  { BUTTON_EVENT_NOTE_PRESSED, 0x29, 0x2C, &CSurf_LaunchControl_XL::LCXLOnChannelSelectButton,/* Track Focus*/      NULL }, // &CSurf_LaunchControl_XL::LCXLOnChannelSelectButtonDC },
          { BUTTON_EVENT_NOTE_PRESSED, 0x39, 0x3C, &CSurf_LaunchControl_XL::LCXLOnChannelSelectButton,      NULL }, // &CSurf_LaunchControl_XL::LCXLOnChannelSelectButtonDC },
//        { BUTTON_EVENT_NOTE_PRESSED, 0x5b, 0x5f, &CSurf_LaunchControl_XL::LCXLOnTransport,          NULL },
//        { BUTTON_EVENT_NOTE_PRESSED, 0x54, 0x54, &CSurf_LaunchControl_XL::LCXLOnMarker,             NULL },
//	      { BUTTON_EVENT_NOTE_PRESSED, 0x56, 0x56, &CSurf_LaunchControl_XL::LCXLOnCycle,              NULL },
//	      { BUTTON_EVENT_NOTE_PRESSED, 0x59, 0x59, &CSurf_LaunchControl_XL::LCXLOnClick,              NULL },
//	      { BUTTON_EVENT_NOTE_PRESSED, 0x50, 0x50, &CSurf_LaunchControl_XL::LCXLOnSave,               NULL },
//	      { BUTTON_EVENT_NOTE_PRESSED, 0x51, 0x51, &CSurf_LaunchControl_XL::LCXLOnUndo,               NULL },
//	      { BUTTON_EVENT_NOTE_PRESSED, 0x64, 0x64, &CSurf_LaunchControl_XL::LCXLOnZoom,               NULL },
//	      { BUTTON_EVENT_NOTE_PRESSED, 0x65, 0x65, &CSurf_LaunchControl_XL::LCXLOnScrub,              NULL },
//	      { BUTTON_EVENT_NOTE_PRESSED, 0x32, 0x32, &CSurf_LaunchControl_XL::LCXLOnFlip,               NULL },
//	      { BUTTON_EVENT_NOTE_PRESSED, 0x33, 0x33, &CSurf_LaunchControl_XL::LCXLOnGlobal,             NULL },
//	      { BUTTON_EVENT_NOTE_PRESSED, 0x36, 0x3d, &CSurf_LaunchControl_XL::LCXLOnFunctionKey,        NULL },
	      { BUTTON_EVENT_NOTE_PRESSED, 0x6A, 0x6C, &CSurf_LaunchControl_XL::LCXLOnMuteButtonSoloArmButton,NULL },
	      { BUTTON_EVENT_NOTE_PRESSED, 0x49, 0x4C, &CSurf_LaunchControl_XL::LCXLOnTrackControlButton,NULL },
	      { BUTTON_EVENT_NOTE_PRESSED, 0x59, 0x5C, &CSurf_LaunchControl_XL::LCXLOnTrackControlButton,NULL },
	      
	      // Press and release events
//	      { BUTTON_EVENT_NOTE_PRESSED | BUTTON_EVENT_NOTE_RELEASED, 0x46, 0x49, &CSurf_LaunchControl_XL::LCXLOnKeyModifier },
//	      { BUTTON_EVENT_NOTE_PRESSED | BUTTON_EVENT_NOTE_RELEASED, 0x60, 0x63, &CSurf_LaunchControl_XL::LCXLOnScroll },
//	      { BUTTON_EVENT_NOTE_PRESSED | BUTTON_EVENT_NOTE_RELEASED, 0x68, 0x70, &CSurf_LaunchControl_XL::LCXLOnTouch },
	  };

	  unsigned int evt_code = evt->midi_message[1];  //get_midi_evt_code( evt );
	  
	  // For these events we only want to track button press
#if 0
	  if ( evt->midi_message[2] >= 0x40 ) {
		ShowConsoleMsgF("LCXLOnButtonPress track button press 0x%02X 0x%02X\n",
			evt->midi_message[1],
			evt->midi_message[2]
			);
#endif
		
	    // Check for double click
	    DWORD now = timeGetTime();
	    bool double_click = (int)evt_code == m_button_last && 
	        now - m_button_last_time < DOUBLE_CLICK_INTERVAL;
	    m_button_last = evt_code;
	    m_button_last_time = now;

		static const int nHandlers = ELEMENTSOF(handlers);

	    // Find event handler
	    for ( int i = 0; i < nHandlers; i++ ) { 
	      ButtonHandler bh = handlers[i];
	      if ( (bh.eventmask & button_event) && bh.evt_min <= evt_code && evt_code <= bh.evt_max ) {
#if 0
	        // Try double click first
			  if ( double_click && bh.func_dc != NULL ) {
				if ( (this->*bh.func_dc)(button_event, evt->midi_message[1], evt) ) {
					ShowConsoleMsgF("LCXLOnButtonPress double click function returned TRUE\n");
					return true;
				}
			  }
#endif

	        // Single click (and unhandled double clicks)
	        if ( bh.func != NULL )
	          if ( (this->*bh.func)(button_event, evt->midi_message[1], evt) ) 
				ShowConsoleMsgF("LCXLOnButtonPress single click (and unhandled double click) function returned TRUE\n");
	            return true;
	      }
	    }
#if 0
	  
	  // For these events we want press and release
	  for ( int i = nPressLCXLOnlyHandlers; i < nHandlers; i++ )
      if ( handlers[i].evt_min <= evt_code && evt_code <= handlers[i].evt_max )
	  {
		  if ( (this->*handlers[i].func)(evt) ) {
			  ShowConsoleMsgF("LCXLOnButtonPress handler returned TRUE\n");
			  return true;
		  }
	  }
#endif

#if 0
	  // Pass thru if not otherwise handled
	   if ( evt->midi_message[2]>=0x40 ) {
		ShowConsoleMsgF("LCXLOnButtonPress passing through event to kbd_LCXLOnMidiEvent\n");
	    int a=evt->midi_message[1];
	    MIDI_event_t evt={0,3,{0xbf-(m_mackie_modifiers&15),a,0}};
	    kbd_LCXLOnMidiEvent(&evt,-1);
	  }
#endif
	  
	  return true;
	}

    void LCXLOnMIDIEvent(MIDI_event_t *evt)
    {
#if _FLU_DEBUG_ONMIDIEVENT >= 1
		// logs a lot ... , hence disabled
		char therest[256];
		therest[0] = '\0';
		for(int idx = 3; idx < evt->size; idx++) {
			sprintf(therest+strlen(therest), " %02X", evt->midi_message[idx]);
		}

		ShowConsoleMsgF("LCXLOnMIDIEvent message sz=%d [%02X %02X %02X%s] %s\n", 
			evt->size,
			evt->midi_message[0],
			evt->midi_message[1],
			evt->midi_message[2],
			therest,
			(evt->size < 3 ? "INVALID size!" : ""));
        #endif

        static const MidiHandlerFunc handlers[] = {
            &CSurf_LaunchControl_XL::LCXLOnTemplateChange,
#if 0
            &CSurf_LaunchControl_XL::LCXLOnLCXLReset,
#endif
            &CSurf_LaunchControl_XL::LCXLOnButtonPress,
            &CSurf_LaunchControl_XL::LCXLOnFaderMove,
            &CSurf_LaunchControl_XL::LCXLOnRotaryEncoder,
            //&CSurf_LaunchControl_XL::LCXLOnJogWheel,
        };

        static const int nHandlers = ELEMENTSOF(handlers);
		for ( int i = 0; i < nHandlers; i++ ) {
#if _FLU_DEBUG_ONMIDIEVENT >= 2
			ShowConsoleMsgF("Testing handler %d/%d...\n", i, nHandlers);
#endif
			if ( (this->*handlers[i])(evt) ) {
#if _FLU_DEBUG_ONMIDIEVENT >= 2
				ShowConsoleMsgF("Handler %d returned true, aborting\n", i);
#endif
				return;
			}
		}

		if (m_device_mode) {
			kbd_OnMidiEvent(evt, 0);
		}
    }

public:

	CSurf_LaunchControl_XL(int offset, int size, int indev, int outdev, int cfgflags, int *errStats) 
    {
		ShowConsoleMsgF("CSurf_LaunchControl_XL offset=%d size=%d indev=%d outdev=%d cfgflags=0x08X\n",
			offset, size, indev, outdev, cfgflags);
      m_cfg_flags=cfgflags;

      m_launchcontrol_xl_list.Add(this);

      m_offset=offset;
      m_size=size;
      m_midi_in_dev=indev;
      m_midi_out_dev=outdev;


      // init locals
      int x;
      for (x = 0; x < sizeof(m_launchcontrol_xl_meterpos)/sizeof(m_launchcontrol_xl_meterpos[0]); x ++)
        m_launchcontrol_xl_meterpos[x]=-100000.0;
      m_launchcontrol_xl_timedisp_lastforce=0;
      m_launchcontrol_xl_meter_lastrun=0;
      memset(m_fader_touchstate,0,sizeof(m_fader_touchstate));
      memset(m_fader_lasttouch,0,sizeof(m_fader_lasttouch));
      memset(m_pan_lasttouch,0,sizeof(m_pan_lasttouch));

  	memset(m_track_armed, 0x00,sizeof(m_track_armed));
	memset(m_track_soloed, 0x00,sizeof(m_track_soloed));
	memset(m_track_muted, 0x00,sizeof(m_track_muted));



      //create midi hardware access
      m_midiin = m_midi_in_dev >= 0 ? CreateMIDIInput(m_midi_in_dev) : NULL;
      m_midiout = m_midi_out_dev >= 0 ? CreateThreadedMIDIOutput(CreateMIDIOutput(m_midi_out_dev,false,NULL)) : NULL;

      if (errStats)
      {
        if (m_midi_in_dev >=0  && !m_midiin) *errStats|=1;
        if (m_midi_out_dev >=0  && !m_midiout) *errStats|=2;
      }

      LCXLReset();

      if (m_midiin)
        m_midiin->start();
      
      m_repos_faders = false;
      m_schedule = NULL;
      m_selected_tracks = NULL;
    }
    
    ~CSurf_LaunchControl_XL() 
    {
      m_launchcontrol_xl_list.Delete(m_launchcontrol_xl_list.Find(this));
      if (m_midiout)
      {

        #if 1 // reset MCU to stock!, fucko enable this in dist builds, maybe?
        struct
        {
          MIDI_event_t evt;
          char data[5];
        }
        poo;
        poo.evt.frame_offset=0;
        poo.evt.size=8;
        poo.evt.midi_message[0]=0xF0;
        poo.evt.midi_message[1]=0x00;
        poo.evt.midi_message[2]=0x00;
        poo.evt.midi_message[3]=0x66;
        poo.evt.midi_message[4]=0x14;
        poo.evt.midi_message[5]=0x08;
        poo.evt.midi_message[6]=0x00;
        poo.evt.midi_message[7]=0xF7;
        Sleep(5);
        m_midiout->SendMsg(&poo.evt,-1);
        Sleep(5);

        #elif 0
        char bla[11]={"          "};
        int x;
        for (x =0 ; x < sizeof(bla)-1; x ++)
          m_midiout->Send(0xB0,0x40+x,bla[x],-1);
        UpdateMackieDisplay(0,"",56*2);
        #endif


      }
      delete m_midiout;
      delete m_midiin;
      while( m_schedule != NULL ) {
        ScheduledAction *temp = m_schedule;
        m_schedule = temp->next;
        delete temp;
      }
      while( m_selected_tracks != NULL ) {
        SelectedTrack *temp = m_selected_tracks;
        m_selected_tracks = temp->next;
        delete temp;
      }
    }
    



    const char *GetTypeString() { return "LAUNCHCONTROL_XL"; }
    const char *GetDescString()
    {
#ifdef _FLU_DEBUG
	  m_descspace.Set("Novation LaunchControl XL " _FLU_ARCH_S " (Debug)");
#else
	  m_descspace.Set("Novation LaunchControl XL" _FLU_ARCH_S) ;
#endif
      char tmp[512];
      sprintf(tmp," (dev %d,%d)",m_midi_in_dev,m_midi_out_dev);
      m_descspace.Append(tmp);
      return m_descspace.Get();     
    }
    const char *GetConfigString() // string of configuration data
    {
      sprintf(m_configtmp,"%d %d %d %d %d",m_offset,m_size,m_midi_in_dev,m_midi_out_dev,m_cfg_flags);      
      return m_configtmp;
    }

    void CloseNoReset() 
    { 
      delete m_midiout;
      delete m_midiin;
      m_midiout=0;
      m_midiin=0;
    }
    void Run() 
    { 
      DWORD now=timeGetTime();

      if (now >= m_frameupd_lastrun+(1000/max((*g_config_csurf_rate),1)) || now < m_frameupd_lastrun-250)
      {
        m_frameupd_lastrun=now;

        while( m_schedule && now >= m_schedule->time ) {
          ScheduledAction *action = m_schedule;
          m_schedule = m_schedule->next;
          (this->*(action->func))();
          delete action;
        }
        

		static int prevNumTracks = 0;
		int numTracks = GetNumTracks();
		if(numTracks != prevNumTracks) {
			prevNumTracks = numTracks;
			setTrackControlState(m_track_control_state); // Force refresh
			TrackList_UpdateAllExternalSurfaces();
		}


        if (m_midiout)
        {
            double pp=(GetPlayState()&1) ? GetPlayPosition() : GetCursorPosition();
            unsigned char bla[10];
      //      bla[-2]='A';//first char of assignment
        //    bla[-1]='Z';//second char of assignment

            // if 0x40 set, dot below item

            memset(bla,0,sizeof(bla));


            int *tmodeptr=(int*)projectconfig_var_addr(NULL,__g_projectconfig_timemode2);

            int tmode=0;
            
            if (tmodeptr && (*tmodeptr)>=0) tmode = *tmodeptr;
            else
            {
              tmodeptr=(int*)projectconfig_var_addr(NULL,__g_projectconfig_timemode);
              if (tmodeptr)
                tmode=*tmodeptr;
            }

            if (tmode==3) // seconds
            {
              double *toptr = (double*)projectconfig_var_addr(NULL,__g_projectconfig_timeoffs);

              if (toptr) pp+=*toptr;
              char buf[64];
              sprintf(buf,"%d %02d",(int)pp, ((int)(pp*100.0))%100);
              if (strlen(buf)>sizeof(bla)) memcpy(bla,buf+strlen(buf)-sizeof(bla),sizeof(bla));
              else
                memcpy(bla+sizeof(bla)-strlen(buf),buf,strlen(buf));

            }
            else if (tmode==4) // samples
            {
              char buf[128];
              format_timestr_pos(pp,buf,sizeof(buf),4);
              if (strlen(buf)>sizeof(bla)) memcpy(bla,buf+strlen(buf)-sizeof(bla),sizeof(bla));
              else
                memcpy(bla+sizeof(bla)-strlen(buf),buf,strlen(buf));
            }
            else if (tmode==5) // frames
            {
              char buf[128];
              format_timestr_pos(pp,buf,sizeof(buf),5);
              char *p=buf;
              char *op=buf;
              int ccnt=0;
              while (*p)
              {
                if (*p == ':')
                {
                  ccnt++;
                  if (ccnt!=3) 
                  {
                    p++;
                    continue;
                  }
                  *p=' ';
                }

                *op++=*p++;
              }
              *op=0;
              if (strlen(buf)>sizeof(bla)) memcpy(bla,buf+strlen(buf)-sizeof(bla),sizeof(bla));
              else
                memcpy(bla+sizeof(bla)-strlen(buf),buf,strlen(buf));
            }
            else if (tmode>0)
            {
              int num_measures=0;
              double beats=TimeMap2_timeToBeats(NULL,pp,&num_measures,NULL,NULL,NULL)+ 0.000000000001;
              double nbeats = floor(beats);

              beats -= nbeats;

              int fracbeats = (int) (1000.0 * beats);

              int *measptr = (int*)projectconfig_var_addr(NULL,__g_projectconfig_measoffs);
              int nm=num_measures+1+(measptr ? *measptr : 0);
              if (nm >= 100) bla[0]='0'+(nm/100)%10;//bars hund
              if (nm >= 10) bla[1]='0'+(nm/10)%10;//barstens
              bla[2]='0'+(nm)%10;//bars

              int nb=(int)nbeats+1;
              if (nb >= 10) bla[3]='0'+(nb/10)%10;//beats tens
              bla[4]='0'+(nb)%10;//beats


              bla[7]='0' + (fracbeats/100)%10;
              bla[8]='0' + (fracbeats/10)%10;
              bla[9]='0' + (fracbeats%10); // frames
            }
            else
            {
              double *toptr = (double*)projectconfig_var_addr(NULL,__g_projectconfig_timeoffs);
              if (toptr) pp+=(*toptr);

              int ipp=(int)pp;
              int fr=(int)((pp-ipp)*1000.0);

              if (ipp >= 360000) bla[0]='0'+(ipp/360000)%10;//hours hundreds
              if (ipp >= 36000) bla[1]='0'+(ipp/36000)%10;//hours tens
              if (ipp >= 3600) bla[2]='0'+(ipp/3600)%10;//hours

              bla[3]='0'+(ipp/600)%6;//min tens
              bla[4]='0'+(ipp/60)%10;//min 
              bla[5]='0'+(ipp/10)%6;//sec tens
              bla[6]='0'+(ipp%10);//sec
              bla[7]='0' + (fr/100)%10;
              bla[8]='0' + (fr/10)%10;
              bla[9]='0' + (fr%10); // frames
            }

            if (m_mackie_lasttime_mode != tmode)
            {
              m_mackie_lasttime_mode=tmode;
              m_midiout->Send(0x90, 0x71, tmode==5?0x7F:0,-1); // set smpte light 
              m_midiout->Send(0x90, 0x72, m_mackie_lasttime_mode>0 && tmode<3?0x7F:0,-1); // set beats light 

            }

            //if (memcmp(m_mackie_lasttime,bla,sizeof(bla)))
            {
              bool force=false;
              if (now > m_launchcontrol_xl_timedisp_lastforce) 
              {
                m_launchcontrol_xl_timedisp_lastforce=now+2000;
                force=true;
              }
              int x;
              for (x =0 ; x < sizeof(bla) ; x ++)
              {
                int idx=sizeof(bla)-x-1;
                if (bla[idx]!=m_mackie_lasttime[idx]||force)
                {
                  m_midiout->Send(0xB0,0x40+x,bla[idx],-1);
                  m_mackie_lasttime[idx]=bla[idx];
                }
              }
            }

            // 0xD0 = level meter, hi nibble = channel index, low = level (F=clip, E=top)
      //      m_midiout->Send(0xD0,0x1E,0);
          if (GetPlayState()&1)
          {
            int x;
      #define VU_BOTTOM 70
            double decay=0.0;
            if (m_launchcontrol_xl_meter_lastrun) 
            {
              decay=VU_BOTTOM * (double) (now-m_launchcontrol_xl_meter_lastrun)/(1.4*1000.0);            // they claim 1.8s for falloff but we'll underestimate
            }
            m_launchcontrol_xl_meter_lastrun=now;
            for (x = 0; x < 8; x ++)
            {
              int idx=m_offset+m_alllaunchcontrol_xls_bank_offset+x+1;
              MediaTrack *tr;
              if ((tr=CSurf_TrackFromID(idx,g_csurf_mcpmode)))
              {
                double pp=VAL2DB((Track_GetPeakInfo(tr,0)+Track_GetPeakInfo(tr,1)) * 0.5);

                if (m_launchcontrol_xl_meterpos[x] > -VU_BOTTOM*2) m_launchcontrol_xl_meterpos[x] -= decay;

                if (pp < m_launchcontrol_xl_meterpos[x]) continue;
                m_launchcontrol_xl_meterpos[x]=pp;
                int v=0xd; // 0xe turns on clip indicator, 0xf turns it off
                if (pp < 0.0)
                {
                  if (pp < -VU_BOTTOM)
                    v=0x0;
                  else v=(int) ((pp+VU_BOTTOM)*13.0/VU_BOTTOM);
                }

                m_midiout->Send(0xD0,(x<<4)|v,0,-1);
              }
            }
          }

        }
      }

      if (m_midiin)
      {
        m_midiin->SwapBufs(timeGetTime());
        int l=0;
        MIDI_eventlist *list=m_midiin->GetReadBuf();
        MIDI_event_t *evts;
        while ((evts=list->EnumItems(&l))) LCXLOnMIDIEvent(evts);

        if (m_mackie_arrow_states)
        {
          DWORD now=timeGetTime();
          if (now >= m_buttonstate_lastrun + 100)
          {
            m_buttonstate_lastrun=now;

            if (m_mackie_arrow_states)
            {
              int iszoom=m_mackie_arrow_states&64;

              if (m_mackie_arrow_states&1) 
                CSurf_OnArrow(0,!!iszoom);
              if (m_mackie_arrow_states&2) 
                CSurf_OnArrow(1,!!iszoom);
              if (m_mackie_arrow_states&4) 
                CSurf_OnArrow(2,!!iszoom);
              if (m_mackie_arrow_states&8) 
                CSurf_OnArrow(3,!!iszoom);

            }
          }
        }
      }
      
      if ( m_repos_faders && now >= m_fader_lastmove + FADER_REPOS_WAIT ) {
        m_repos_faders = false;
        TrackList_UpdateAllExternalSurfaces();
      }
    }
#if 0
    void SetTrackListChange() 
    { 
      ShowConsoleMsgF("SetTrackListChange\n");
      if (m_midiout)
      {
        int x;
        for (x = 0; x < 8; x ++)
        {
          MediaTrack *t=CSurf_TrackFromID(x+m_offset+m_alllaunchcontrol_xls_bank_offset+1,g_csurf_mcpmode);
          if (!t || t == CSurf_TrackFromID(0,false))
          {
            // clear item
            int panint=m_flipmode ? panToInt14(0.0) : volToInt14(0.0);
            unsigned char volch=m_flipmode ? volToChar(0.0) : panToChar(0.0);

            m_midiout->Send(0xe0 + (x&0xf),panint&0x7f,(panint>>7)&0x7f,-1);
            m_midiout->Send(0xb0,0x30+(x&0xf),1+((volch*11)>>7),-1);
            m_vol_lastpos[x]=panint;


            m_midiout->Send(0x90, 0x10+(x&7),0,-1); // reset mute
            m_midiout->Send(0x90, 0x18+(x&7),0,-1); // reset selected

            m_midiout->Send(0x90, 0x08+(x&7),0,-1); //reset solo
            m_midiout->Send(0x90, 0x0+(x&7),0,-1); // reset recarm

            char buf[7]={0,};       
#if 0
            UpdateMackieDisplay(x*7,buf,7); // clear display
#endif

            struct
            {
              MIDI_event_t evt;
              char data[9];
            }
            poo;
            poo.evt.frame_offset=0;
            poo.evt.size=9;
            poo.evt.midi_message[0]=0xF0;
            poo.evt.midi_message[1]=0x00;
            poo.evt.midi_message[2]=0x00;
            poo.evt.midi_message[3]=0x66;
            poo.evt.midi_message[4]=0x14;
            poo.evt.midi_message[5]=0x20;
            poo.evt.midi_message[6]=0x00+x;
            poo.evt.midi_message[7]=0x03;
            poo.evt.midi_message[8]=0xF7;
            Sleep(5);
            m_midiout->SendMsg(&poo.evt,-1);
            Sleep(5);
            m_midiout->Send(0xD0,(x<<4)|0xF,0,-1);
          }
        }
      }
    }
#endif

#if 0
#define FIXID(i) \
	int i=CSurf_TrackToID(tr,g_csurf_mcpmode); \
	do { \
		int oid=i; \
		if (i>0) { \
			i -= m_offset+m_alllaunchcontrol_xls_bank_offset+1; \
			if (i==8) \
				i=-1; \
		} else if (i==0) \
			i=-1; \
	} while (0)
#endif

#define FIXID(i) \
	int i=CSurf_TrackToID(tr,g_csurf_mcpmode); \
	do { \
		int oid=i; \
		i --; \
	} while (0)





    void SetSurfaceMute(MediaTrack *tr, bool mute) 
    {
      FIXID(id);
      if (m_midiout && id>=0 && id < 256 /* && id < m_size */)
      {
		if((m_track_control_state == m_track_control_state_only) || (m_track_control_state_only >= TRACKCONTROLSTATE_LAST))
			ShowConsoleMsgF("SetSurfaceMute id=%d mute=%s\n",
				id,
				(mute ? "MUTE" : "unmute"));
		if(m_track_muted[id] != mute) {
			m_track_muted[id] = mute;
			setTrackControlState(TRACKCONTROLSTATE_MUTE);
		}
	  } else {    
			ShowConsoleMsgF("SetSurfaceMute id=%d mute=%s (IGNORED)\n",
				id,
				(mute ? "MUTE" : "unmute"));
	  }
    }

    void SetSurfaceSelected(MediaTrack *tr, bool selected) 
    { 
      if ( selected ) 
		  selectTrack(tr);
      else
		  deselectTrack(tr);

#if 0
      FIXID(id);
	  int min = m_offset + m_alllaunchcontrol_xls_bank_offset;
	  int max = m_offset + m_alllaunchcontrol_xls_bank_offset + m_size - 1;
	  if (id >= min && id < max) {
		  int tid = id - min;
		  int tidc = id + 1;
		  led_e led = (led_e)(LED_TRACK_FOCUS_1 + tid);
		  led_color_e color;
		  if (isTrackVisible(tidc))
			  color = (selected ? LED_TRACK_FOCUS_ON : LED_TRACK_FOCUS_OFF);
		  else
			color = LED_COLOR_OFF;
		  LCXLSendSetLedColor(led, color);
	  }
#endif

	  setTrackControlState(m_track_control_state);

	}
    
    void selectTrack( MediaTrack *tr ) {
		// TODO: test and/or modify
      const GUID *guid = GetTrackGUID(tr);
	  ShowConsoleMsgF("selectTrack\n");

      // Empty list, start new list
      if ( m_selected_tracks == NULL ) {
        m_selected_tracks = new SelectedTrack(tr);
        return;
      }
      
      // This track is head of list
      if ( guid && !memcmp(&m_selected_tracks->guid,guid,sizeof(GUID)) )
        return;
      
      // Scan for track already selected
      SelectedTrack *i = m_selected_tracks;
      while ( i->next ) {
        i = i->next;
        if ( guid && !memcmp(&i->guid,guid,sizeof(GUID)) )
          return;
      }
      
      // Append at end of list if not already selected
      i->next = new SelectedTrack(tr);
    }
    
    void deselectTrack( MediaTrack *tr ) {
		// TODO: test and/or modify
      const GUID *guid = GetTrackGUID(tr);
	  ShowConsoleMsgF("deselectTrack\n");
      
      // Empty list?
      if ( m_selected_tracks ) {
        // This track is head of list?
        if ( guid && !memcmp(&m_selected_tracks->guid,guid,sizeof(GUID)) ) {
          SelectedTrack *tmp = m_selected_tracks;
          m_selected_tracks = m_selected_tracks->next;
          delete tmp;
        }
        
        // Search for this track
        else {
          SelectedTrack *i = m_selected_tracks;
          while( i->next ) {
            if ( guid && !memcmp(&i->next->guid,guid,sizeof(GUID)) ) {
              SelectedTrack *tmp = i->next;
              i->next = i->next->next;
              delete tmp;
              break;
            }
            i = i->next;
          }
        }
      }
    }
    
    void SetSurfaceSolo(MediaTrack *tr, bool solo) 
    { 
      FIXID(id);
      if (m_midiout && id>=0 && id < 256 /* && id < m_size */)
      {
   	    if((m_track_control_state == m_track_control_state_only) || (m_track_control_state_only >= TRACKCONTROLSTATE_LAST))
			ShowConsoleMsgF("SetSurfaceSolo id=%d solo=%s\n",
				id,
				(solo ? "SOLO" : "unsolo"));
		if(m_track_soloed[id] != solo) {
			m_track_soloed[id] = solo;
			setTrackControlState(TRACKCONTROLSTATE_SOLO);
		}

		if (id < 8) {
		} else if (id == 8) {
          // Hmm, seems to call this with id 8 to tell if any
          // tracks are soloed.

			/* Doesn't exist on MF8
          m_midiout->Send(0x90, 0x73,solo?1:0,-1);     // rude solo light
          m_midiout->Send(0x90, 0x5a,solo?0x7f:0,-1);  // solo button led
		  */
        }
	  } else {
			ShowConsoleMsgF("SetSurfaceSolo id=%d solo=%s IGNORED\n",
				id,
				(solo ? "SOLO" : "unsolo"));
	  }
    }

	bool m_track_armed[256];
	bool m_track_soloed[256];
	bool m_track_muted[256];

    void SetSurfaceRecArm(MediaTrack *tr, bool recarm) 
    { 
      FIXID(id);
      if (m_midiout && id>=0 && id < 256 /* && id < m_size */)
      {
	    if((m_track_control_state == m_track_control_state_only) || (m_track_control_state_only >= TRACKCONTROLSTATE_LAST))
		  ShowConsoleMsgF("SetSurfaceRecArm id=%d arm=%s, m_size=%d\n",
			id,
			(recarm ? "ARM" : "unarm"),
			m_size);
		if(m_track_armed[id] != recarm) {
			m_track_armed[id] = recarm;
			setTrackControlState(TRACKCONTROLSTATE_ARM);
		}
	  } else {
		  ShowConsoleMsgF("SetSurfaceRecArm id=%d arm=%s, m_size=%d IGNORED\n",
			id,
			(recarm ? "ARM" : "unarm"),
			m_size);
	  }
    }




    bool GetTouchState(MediaTrack *tr, int isPan=0)
    {
		// TODO: test and/or modify
      FIXID(id);
      if (!m_flipmode != !isPan) 
      {
        if (id >= 0 && id < 8)
        {
          DWORD now=timeGetTime();
          if (m_pan_lasttouch[id]==1 || (now<m_pan_lasttouch[id]+3000 && now >= m_pan_lasttouch[id]-1000)) // fake touch, go for 3s after last movement
          {
            return true;
          }
        }
        return false;
      }
      if (id>=0 && id < 9)
      {
        if (!(m_cfg_flags&CONFIG_FLAG_FADER_TOUCH_MODE) && !m_fader_touchstate[id] && m_fader_lasttouch[id] && m_fader_lasttouch[id]!=0xffffffff)
        {
          DWORD now=timeGetTime();
          if (now<m_fader_lasttouch[id]+3000 && now >= m_fader_lasttouch[id]-1000) return true;
          return false;
        }

        return !!m_fader_touchstate[id];
      }
  
      return false; 
    }





    void ResetCachedVolPanStates() 
    { 
      memset(m_vol_lastpos,0xff,sizeof(m_vol_lastpos));
      memset(m_pan_lastpos,0xff,sizeof(m_pan_lastpos));
    }
    
    void OnTrackSelection(MediaTrack *tr) 
    { 
      // TODO Test and/or modify
	  ShowConsoleMsgF("OnTrackSelection\n");
      int tid=CSurf_TrackToID(tr,g_csurf_mcpmode);
      int x;
      int movesize=8;
      for (x = 0; x < m_launchcontrol_xl_list.GetSize(); x ++)
      {
        CSurf_LaunchControl_XL *launchcontrol_xl=m_launchcontrol_xl_list.Get(x);
        if (launchcontrol_xl)
        {
          if (launchcontrol_xl->m_offset+8 > movesize)
            movesize=launchcontrol_xl->m_offset+8;
        }
      }

      int newpos=tid-1;
      if (newpos >= 0 && (newpos < m_alllaunchcontrol_xls_bank_offset || newpos >= m_alllaunchcontrol_xls_bank_offset+movesize))
      {
        int no = newpos - (newpos % movesize);

        if (no!=m_alllaunchcontrol_xls_bank_offset)
        {
          m_alllaunchcontrol_xls_bank_offset=no;
          // update all of the sliders
          TrackList_UpdateAllExternalSurfaces();
          for (x = 0; x < m_launchcontrol_xl_list.GetSize(); x ++)
          {
            CSurf_LaunchControl_XL *launchcontrol_xl=m_launchcontrol_xl_list.Get(x);
            if (launchcontrol_xl && launchcontrol_xl->m_midiout)
            {
              launchcontrol_xl->m_midiout->Send(0xB0,0x40+11,'0'+(((m_alllaunchcontrol_xls_bank_offset+1)/10)%10),-1);
              launchcontrol_xl->m_midiout->Send(0xB0,0x40+10,'0'+((m_alllaunchcontrol_xls_bank_offset+1)%10),-1);
            }
          }
        }
      }
    }
    
#if 0
    bool IsKeyDown(int key) 
    { 
      if (m_midiin)
      {
        if (key == VK_SHIFT) return !!(m_mackie_modifiers&1);
        if (key == VK_CONTROL) return !!(m_mackie_modifiers&4);
        if (key == VK_MENU) return !!(m_mackie_modifiers&8);
      }

      return false; 
    }
#endif
};

static void parseParms(const char *str, int parms[5])
{
  parms[0]=0;
  parms[1]=9;
  parms[2]=parms[3]=-1;
  parms[4]=0;

  const char *p=str;
  if (p)
  {
    int x=0;
    while (x<5)
    {
      while (*p == ' ') p++;
      if ((*p < '0' || *p > '9') && *p != '-') break;
      parms[x++]=atoi(p);
      while (*p && *p != ' ') p++;
    }
  }  
}

static IReaperControlSurface *createFunc(const char *type_string, const char *configString, int *errStats)
{
  int parms[5];
  parseParms(configString,parms);

  return new CSurf_LaunchControl_XL(parms[0],parms[1],parms[2],parms[3],parms[4],errStats);
}


static WDL_DLGRET dlgProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  switch (uMsg)
  {
    case WM_INITDIALOG:
      {
        int parms[5];
        parseParms((const char *)lParam,parms);

        LRESULT n=GetNumMIDIInputs();
        LRESULT x=SendDlgItemMessage(hwndDlg,IDC_COMBO2,CB_ADDSTRING,0,(LPARAM)"None");
        SendDlgItemMessage(hwndDlg,IDC_COMBO2,CB_SETITEMDATA,x,-1);
        x=SendDlgItemMessage(hwndDlg,IDC_COMBO3,CB_ADDSTRING,0,(LPARAM)"None");
        SendDlgItemMessage(hwndDlg,IDC_COMBO3,CB_SETITEMDATA,x,-1);
        for (x = 0; x < n; x ++)
        {
          char buf[512];
          if (GetMIDIInputName((int)x,buf,sizeof(buf)))
          {
            LRESULT a=SendDlgItemMessage(hwndDlg,IDC_COMBO2,CB_ADDSTRING,0,(LPARAM)buf);
            SendDlgItemMessage(hwndDlg,IDC_COMBO2,CB_SETITEMDATA,a,x);
            if (x == parms[2]) SendDlgItemMessage(hwndDlg,IDC_COMBO2,CB_SETCURSEL,a,0);
          }
        }
        n=GetNumMIDIOutputs();
        for (x = 0; x < n; x ++)
        {
          char buf[512];
          if (GetMIDIOutputName((int)x,buf,sizeof(buf)))
          {
            LRESULT a=SendDlgItemMessage(hwndDlg,IDC_COMBO3,CB_ADDSTRING,0,(LPARAM)buf);
            SendDlgItemMessage(hwndDlg,IDC_COMBO3,CB_SETITEMDATA,a,x);
            if (x == parms[3]) SendDlgItemMessage(hwndDlg,IDC_COMBO3,CB_SETCURSEL,a,0);
          }
        }
        SetDlgItemInt(hwndDlg,IDC_EDIT1,parms[0],TRUE);
        SetDlgItemInt(hwndDlg,IDC_EDIT2,parms[1],FALSE);
        if (parms[4]&CONFIG_FLAG_FADER_TOUCH_MODE)
          CheckDlgButton(hwndDlg,IDC_CHECK1,BST_CHECKED);
        if (parms[4]&CONFIG_FLAG_MAPF1F8TOMARKERS)
          CheckDlgButton(hwndDlg,IDC_CHECK2,BST_CHECKED);
      }
    break;
    case WM_USER+1024:
      if (wParam > 1 && lParam)
      {
        char tmp[512];

        LRESULT indev=-1, outdev=-1, offs=0, size=9;
        LRESULT r=SendDlgItemMessage(hwndDlg,IDC_COMBO2,CB_GETCURSEL,0,0);
        if (r != CB_ERR) indev = SendDlgItemMessage(hwndDlg,IDC_COMBO2,CB_GETITEMDATA,r,0);
        r=SendDlgItemMessage(hwndDlg,IDC_COMBO3,CB_GETCURSEL,0,0);
        if (r != CB_ERR)  outdev = SendDlgItemMessage(hwndDlg,IDC_COMBO3,CB_GETITEMDATA,r,0);

        BOOL t;
        r=GetDlgItemInt(hwndDlg,IDC_EDIT1,&t,TRUE);
        if (t) offs=r;
        r=GetDlgItemInt(hwndDlg,IDC_EDIT2,&t,FALSE);
        if (t) 
        {
          if (r<1)r=1;
          else if(r>256)r=256;
          size=r;
        }
        int cflags=0;
        if (IsDlgButtonChecked(hwndDlg,IDC_CHECK1))
          cflags|=CONFIG_FLAG_FADER_TOUCH_MODE;
        if (IsDlgButtonChecked(hwndDlg,IDC_CHECK2))
          cflags|=CONFIG_FLAG_MAPF1F8TOMARKERS;

        sprintf(tmp,"%d %d %d %d %d",offs,size,indev,outdev,cflags);
        lstrcpyn((char *)lParam, tmp,(int)wParam);
        
      }
    break;
  }
  return 0;
}

static HWND configFunc(const char *type_string, HWND parent, const char *initConfigString)
{
  return CreateDialogParam(g_hInst,MAKEINTRESOURCE(IDD_SURFACEEDIT_LAUNCHCONTROL_XL1),parent,dlgProc,(LPARAM)initConfigString);
}


reaper_csurf_reg_t csurf_launchcontrol_xl_reg = 
{
#ifdef _FLU_DEBUG
  "LAUNCHCONTROL_XL_DEBUG" _FLU_ARCH_S,
  "Novation LaunchControl XL (Debug) " _FLU_ARCH_S,
#else
  "LAUNCHCONTROL_XL" _FLU_ARCH_S,
  "Novation LaunchControl XL " _FLU_ARCH_S,
#endif
  createFunc,
  configFunc,
};

int CSurf_Impl_Register(reaper_plugin_info_t *rec)
{
  return rec->Register("csurf",&csurf_launchcontrol_xl_reg);
}
