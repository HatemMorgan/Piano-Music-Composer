Set WshShell = CreateObject("WScript.Shell")
strString = "ryiweyulfadhgpawri5qetyIpfadyi9qwtieti8wtuodIpadIs9qwtuosfDfuoswryIsfukjtIOtietulfoa0wetuogfjkhjsfdh"
WshShell.AppActivate "Firefox"
WScript.sleep 5000
For i=1 To Len(strString)
    WshShell.SendKeys Mid(strString,i,1)
	WScript.sleep 350
Next
