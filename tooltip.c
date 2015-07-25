#define  STRICT
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <commctrl.h>

void _stdcall DoToolbarNotify(TBNOTIFY *pHdr)

{
  LPSTR pReply;
  TOOLTIPTEXT *pTT;

  if (pHdr->hdr.code==TTN_NEEDTEXT) {
    pTT = (TOOLTIPTEXT*)pHdr;
	switch (pTT->hdr.idFrom) {
      case(10):
        pReply="Create new record";
		break;
	case(20):
  	    pReply="Open existing record";
		break;
      case(30):
  	    pReply="Close current record";
	    break;
      case(503):
  	    pReply="Save current parameters";
	    break;
      case(60):
  	    pReply="Copy content to clipboard";
	    break;
      case(301):
  	    pReply="Start energy levels calculation";
	    break;
      case(302):
  	    pReply="Start fitting procedure";
	    break;
      case(3000):
  	    pReply="Stop current calculation";
	    break;
      case(601):
  	    pReply="Database of reduced matrix elements";
	    break;
      case(680):
  	    pReply="Database of energy levels";
          break;
      case(101):
  	    pReply="Help";
	    break;

      default:
        pReply="Unknown";
     }
      lstrcpy(pTT->szText, pReply);
    }
}
