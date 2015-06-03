#include "csurf.h"


class TrackIterator {
  int m_index;
  int m_len;
public:
  TrackIterator() {
    m_index = 1;
    m_len = CSurf_NumTracks(false);
  }
  MediaTrack* operator*() {
    return CSurf_TrackFromID(m_index,false);
  }
  TrackIterator &operator++() {
    if ( m_index <= m_len ) ++m_index;
    return *this;
  }
  bool end() {
    return m_index > m_len;
  }
};

MediaTrack* TrackFromGUID( const GUID &guid );
