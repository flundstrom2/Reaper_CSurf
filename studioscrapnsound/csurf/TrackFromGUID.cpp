/*
** reaper_csurf
** Author: Fredrik Lundström, Studio Scrap 'N' Sound
** e-mail: fredrik.lundstrom.1974@gmail.com
** Copyright (C) 2006-2009 Cockos Incorporated
** Copyright (C) 2015 Fredrik Lundström
** License: LGPL.
*/

#include "TrackFromGUID.h"

#ifdef _DEBUG
#define _FLU_DEBUG
#endif

#define _FLU_DEBUG_ISTRACKVISIBLE 0

#ifdef _FLU_DEBUG
static void ShowConsoleMsgF(const char *fmt, ...)
{
	char buffer[512];
	va_list(ap);
	va_start(ap, fmt);
	strcpy(buffer, "TrackFromGUID" _FLU_ARCH_S ": ");
	vsprintf(buffer + strlen(buffer), fmt, ap);
	ShowConsoleMsg(buffer);
}
#else
#define ShowConsoleMsgF(...) do { } while (0)
#endif

MediaTrack* TrackFromGUID( const GUID &guid )
{
  for ( TrackIterator ti; !ti.end(); ++ti ) {
    MediaTrack *tr = *ti;
    const GUID *tguid=GetTrackGUID(tr);
    
    if (tr && tguid && !memcmp(tguid,&guid,sizeof(GUID)))
      return tr;
  }
  return NULL;
}

bool isTrackVisible(int tidc)
{
	bool b_show = false;
	bool gotanAPI = false;
	int id = tidc - 1;
#if _FLU_DEBUG_ISTRACKVISIBLE > 0
	ShowConsoleMsgF("isTrackVisible: id=%d tidc=%d\n", id, tidc);
#endif
	MediaTrack *tr = GetTrack(NULL, id);
	if (tr == NULL) {
		ShowConsoleMsgF("isTrackVisible: No track tidc=%d\n", tidc);
		return false;
	}
	if (GetMediaTrackInfo_Value != NULL) {
		bool b_showintcp = (GetMediaTrackInfo_Value(tr, "B_SHOWINTCP") != 0.0 ? true : false);
		bool b_showmixer = (GetMediaTrackInfo_Value(tr, "B_SHOWINMIXER") != 0.0 ? true : false);
		if (b_showintcp || b_showmixer) {
			b_show = true;
		}
		gotanAPI = true;
	}

	if (!gotanAPI) {
		ShowConsoleMsgF("isTrackVisible: No API, assuming visible %d!\n", tidc);
		return true;
	}
	return b_show;
}

