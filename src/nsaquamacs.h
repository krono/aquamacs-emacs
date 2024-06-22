/* Functions added for Aquamacs to GNU Emacs.

   Copyright (C) 2004-2023.

   This file is part of GNU Emacs.

   Aquamacs Emacs is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or (at
   your option) any later version.

   Aquamacs Emacs is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Aquamacs Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

/*
  External declarations for new functions for Aquamacs. In earlier
  versions of Aquamacs, these functions were either defined statically
  or declared extern in on e of the regular Emacs ns*.[mh] files.
*/

extern void aquamacs_set_edit_menu(NSMenu *appMenu);

/* Panel menu */
extern NSMenu *panelMenu;

/* ODB / XCode interaction support */

/* The following taken from the MacVIM source code, by Björn Winckler */

// ODB Editor Suite Constants (taken from ODBEditorSuite.h)
#define keyFileSender		'FSnd'
#define keyFileSenderToken	'FTok'
#define keyFileCustomPath	'Burl'
#define kODBEditorSuite		'R*ch'
#define kAEModifiedFile		'FMod'
#define keyNewLocation		'New?'
#define kAEClosedFile		'FCls'
#define keySenderToken		'Tokn'

typedef struct
{
  int16_t unused1;	   // 0 (not used)
  int16_t lineNum;	   // line to select (< 0 to specify range)
  int32_t startRange;   // start of selection range (if line < 0)
  int32_t endRange;	   // end of selection range (if line < 0)
  int32_t unused2;	   // 0 (not used)
  int32_t theDate;	   // modification date/time
} MMXcodeSelectionRange;
