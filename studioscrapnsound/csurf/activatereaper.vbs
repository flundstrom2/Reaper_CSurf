'Activate REAPER, based on Switch.vbs from http://www.sevenforums.com/general-discussion/153457-batch-vbs-file-bring-front-help.html
' Define the titles to look for
gTitle1		= "business use"

rc = 0

' If one of the window closes, this variable becomes False and the script exits.
Dim bWindowFound

' Create scripting object
Dim WShell, lRunUninstall
Set WShell = CreateObject("WScript.Shell")

' Activate the window that has a title matching, starting, or ending with gTitle1.
bWindowFound = WShell.AppActivate(gTitle1)

' Check if we activated the window ?
If(bWindowFound) Then

	' Send ALT+SPACEBAR (system menu) M (move) ESCAPE (cancel) to activate window.
	WShell.SendKeys "% M{ESCAPE}"
	rc = 1
else
	rc = 0
End If


' Completed. Stop the script
' WScript.Echo "Quitting script"
Set WShell = Nothing
WScript.quit(rc)
