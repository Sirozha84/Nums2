Function kLeft
    kLeft = key$ = Chr$(0) + Chr$(75) Or key$ = "o" Or key$ = "ù"
End Function

Function kRight
    kRight = key$ = Chr$(0) + Chr$(77) Or key$ = "p" Or key$ = "ç"
End Function

Function kUp
    kUp = key$ = Chr$(0) + Chr$(72) Or key$ = "q" Or key$ = "é"
End Function

Function kDown
    kDown = key$ = Chr$(0) + Chr$(80) Or key$ = "a" Or key$ = "ô"
End Function

Function kEnter
    kEnter = key$ = Chr$(13) Or key$ = " " Or key$ = "0"
End Function

Function kEsc
    kEsc = key$ = Chr$(27)
End Function

Function kHome
    kHome = key$ = Chr$(0) + Chr$(71) Or key$ = "W" Or key$ = "Ö"
End Function

Function kEnd
    kEnd = key$ = Chr$(0) + Chr$(79) Or key$ = "S" Or key$ = "Û"
End Function

Function kPgUp
    kPgUp = key$ = Chr$(0) + Chr$(73) Or key$ = "w" Or key$ = "ö"
End Function

Function kPgDn
    kPgDn = key$ = Chr$(0) + Chr$(81) Or key$ = "s" Or key$ = "û"
End Function
