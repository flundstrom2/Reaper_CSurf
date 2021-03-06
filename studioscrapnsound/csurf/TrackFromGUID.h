/*
** reaper_csurf
** Author: Fredrik Lundstr�m, Studio Scrap 'N' Sound
** e-mail: fredrik.lundstrom.1974@gmail.com
** Copyright (C) 2006-2009 Cockos Incorporated
** Copyright (C) 2015 Fredrik Lundstr�m
** License: LGPL.
*/

#include "csurf.h"

#ifdef _M_X64
#define _FLU_ARCH_S "x64"
#else
#define _FLU_ARCH_S "x86"
#endif

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

bool isTrackVisible(int tidc);
