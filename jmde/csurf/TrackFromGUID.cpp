#include "TrackFromGUID.h"

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

bool isTrackVisible(int id)
{
	bool b_show = false;
	if (GetMediaTrackInfo_Value != NULL) {
		MediaTrack *tr = GetTrack(NULL, id);
		if (tr) {
			bool b_showintcp = (GetMediaTrackInfo_Value(tr, "B_SHOWINTCP") != 0.0 ? true : false);
			bool b_showmixer = (GetMediaTrackInfo_Value(tr, "B_SHOWINMIXER") != 0.0 ? true : false);
			if (b_showintcp || b_showmixer) {
				b_show = true;
			}
		}
	}
	return b_show;
}

