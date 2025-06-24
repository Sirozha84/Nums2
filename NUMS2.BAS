$Debug

Const True = -1
Const False = 0
Const Size = 24
Const MaxRows = 100
Const MaxColumns = 9
Const VisibleRows = 28
Const Border = 10
'Const ColWhite = _RGB32(255, 255, 255)
Dim Shared kLeft$: kLeft$ = Chr$(0) + Chr$(75)
Dim Shared kRight$: kRight$ = Chr$(0) + Chr$(77)
Dim Shared kUp$: kUp$ = Chr$(0) + Chr$(72)
Dim Shared kDown$: kDown$ = Chr$(0) + Chr$(80)
Dim Shared kEnter$: kEnter$ = Chr$(13)
Dim Shared kEsc$: kEsc$ = Chr$(27)
Dim Shared kHome$: kHome$ = Chr$(0) + Chr$(71)
Dim Shared kEnd$: kEnd$ = Chr$(0) + Chr$(79)
Dim Shared kPgUp$: kPgUp$ = Chr$(0) + Chr$(73)
Dim Shared kPgDn$: kPgDn$ = Chr$(0) + Chr$(81)
'Dim Shared kbs$: kbs$ = Chr$(8)

Dim Shared M(MaxRows, MaxColumns) 'Main Field
Dim Shared Nm(MaxRows) 'Row Nums
Dim Shared Cn(MaxRows) 'Rewrites Count
Dim Shared GameWidth 'Gzme size
Dim Shared GameHeight
Dim Shared Cr 'Cursor
Dim Shared Cc
Dim Shared LastRow

Dim Shared Nums&
Dim Shared Background&
Dim Shared Cursor&
Dim Shared Digits&



Init
GameInit
Game





Sub Init
    _Title "Nums2"
    $Resize:Smooth
    Screen _NewImage(1280, 720, 32)

    Background& = _LoadImage("Graphics\background.png", 32)
    Nums& = _LoadImage("Graphics\nums.png", 32)
    Cursor& = _LoadImage("Graphics\cursor.png", 32)
    Digits& = _LoadImage("Graphics\digits.png", 32)
End Sub

Sub GameInit
    GameWidth = 9
    For r = 0 To MaxRows - 1
        For C = 0 To MaxColumns - 1
            M(r, C) = -1
        Next
    Next

    r = 0
    C = 0
    LastRow = 1
    For D = 1 To 19 '19
        s$ = Str$(D)
        s$ = Right$(s$, Len(s$) - 1)
        ok = True
        For i = 1 To Len(s$)
            If Mid$(s$, i, 1) = "0" Then ok = False
        Next
        If ok Then
            For i = 1 To Len(s$)
                M(r, C) = Val(Mid$(s$, i, 1))
                Nm(r) = LastRow
                Cn(r) = 0
                GameHeight = r + 1
                C = C + 1
                If C > GameWidth - 1 Then C = 0: r = r + 1: LastRow = LastRow + 1
            Next
        End If
    Next
    Cc = 0
    Cr = 0
End Sub

Sub Game
    ext = 0 'Exit flag
    sel = False 'First select
    sr = 0 'Selected item
    sc = 0
    scroll = 0 'Scroll Shift
    left = 1280 / 2 - Size * GameWidth / 2 'GameField coordinates
    top = Size
    sbl = left + GameWidth * Size + 2 'ScrollBar coordinates
    sbg = 720 - Size * 2 - 1
    av = Available
    an = 0 'Animation frame
    ans = 0 'Animation ScrollBas
    brs = 0 'Bright ScrollBar
    Do
        'Background
        _Limit 60
        _PutImage (0, 0), Background&

        'Calculating
        oldscroll = scroll
        If scroll > Cr - Border Then scroll = Cr - Border
        If scroll < Cr - VisibleRows + Border + 1 Then scroll = Cr - VisibleRows + Border + 1
        If scroll > GameHeight - VisibleRows Then scroll = GameHeight - VisibleRows
        If scroll < 0 Then scroll = 0
        If oldscroll <> scroll Then ans = 1

        'Cursor
        _PutImage (left + Cc * Size, top + Cr * Size - scroll * Size), Cursor&, 0, (an, 0)-(an + Size - 1, Size - 1)
        an = an + 1
        If an > Size Then an = 0

        'GameField
        For r = 0 To VisibleRows - 1
            For c = 0 To GameWidth - 1
                If M(r, c) >= 0 Then
                    sx = M(r + scroll, c) * Size
                    If sel And c = sc And r = sr - scroll Then s = 1 Else s = 0
                    _PutImage (left + c * Size, top + r * Size), Nums&, 0, (sx, s * Size)-(sx + Size - 1, s * Size + Size - 1)
                End If
            Next
            If M(r + scroll, 0) >= 0 Then
                'Num
                d$ = Str$(Nm(r + scroll))
                l = Len(d$)
                For i = 2 To l
                    n = Val(Mid$(d$, i, 1))
                    _PutImage (left - (l - i) * 8 - 17, top + r * Size + 7), Digits&, 0, (n * 8, 0)-(n * 8 + 7, 9)
                Next
                'Count
                If Cn(r + scroll) > 0 Then
                    d$ = Str$(Cn(r + scroll))
                    l = Len(d$)
                    For i = 2 To l
                        n = Val(Mid$(d$, i, 1))
                        _PutImage (sbl + i * 8 - 7, top + r * Size + 7), Digits&, 0, (n * 8, 0)-(n * 8 + 7, 9)
                    Next
                End If
            End If
        Next

        'Tips
        For c = 0 To GameWidth - 1
            If scroll > 0 Then
                d = 0
                For r = scroll - 1 To 0 Step -1
                    If M(r, c) > 0 Then d = M(r, c): Exit For
                Next
                _PutImage (left + c * Size, 0), Nums&, 0, (d * Size, 2 * Size)-(d * Size + Size - 1, 3 * Size - 1)
            End If
            If scroll < GameHeight - VisibleRows Then
                d = 0
                For r = scroll + VisibleRows To GameHeight
                    If M(r, c) > 0 Then d = M(r, c): Exit For
                Next
                _PutImage (left + c * Size, 719 - Size), Nums&, 0, (d * Size, 2 * Size)-(d * Size + Size - 1, 3 * Size - 1)
            End If
        Next

        'Scrollbar
        If GameHeight > VisibleRows Then
            l = VisibleRows / GameHeight * sbg
            t = Size + scroll / GameHeight * sbg
            If ans > 0 Then brs = brs + 10
            If brs > 512 Then ans = -1
            If ans < 0 Then brs = brs - 5
            If brs < 0 Then ans = 0
            br = brs
            If br > 255 Then br = 255
            col& = _RGBA32(255, 255, 255, br / 2)
            Line (sbl, t + 1)-(sbl, t + l - 1), col&
            Line (sbl + 1, t)-(sbl + 1, t + l), col&
            Line (sbl + 2, t)-(sbl + 2, t + l), col&
            Line (sbl + 3, t + 1)-(sbl + 3, t + l - 1), col&
        End If

        'Table
        Locate 10, 10: Print "Available:"; av
        Locate 20, 10: Print "scroll:"; scroll

        _Display


        'Check Available and rewrite
        If av = 0 Then
            If Not Rewrite Then GameOver: ext = True
            av = Available
        End If

        'Keyboard
        k$ = InKey$
        If k$ = kLeft$ Or k$ = "o" Then Cc = Cc - 1: If Cc < 0 Then Cc = 0
        If k$ = kRight$ Or k$ = "p" Then Cc = Cc + 1: If Cc > GameWidth - 1 Then Cc = GameWidth - 1
        If k$ = kUp$ Or k$ = "q" Then Cr = Cr - 1
        If k$ = kPgUp$ Or k$ = "w" Then Cr = Cr - VisibleRows
        If Cr < 0 Then Cr = 0
        If k$ = kDown$ Or k$ = "a" Then Cr = Cr + 1
        If k$ = kPgDn$ Or k$ = "s" Then Cr = Cr + VisibleRows
        If Cr > GameHeight - 1 Then Cr = GameHeight - 1
        If k$ = kHome$ Or k$ = "W" Then Cr = 0
        If k$ = kEnd$ Or k$ = "S" Then Cr = GameHeight - 1
        If k$ = kEsc$ Then ext = -1
        If k$ = " " Or k$ = "0" Then
            If Not sel Then
                'First select
                If M(Cr, Cc) > 0 Then sr = Cr: sc = Cc: sel = True
            Else
                'Second select
                If sr = Cr And sc = Cc Or M(Cr, Cc) < 1 Then
                    sel = False
                Else
                    'Match
                    If CheckPare(Cr, Cc, sr, sc) Then
                        M(Cr, Cc) = 0
                        M(sr, sc) = 0
                        Collapse
                    End If
                    sel = False

                    'Recheck Available
                    av = Available
                End If
            End If
        End If

        'Debug...
        If k$ = kEnter$ Then r = Rewrite
        'If Len(k$) > 0 Then Print Asc(Mid$(k$, 1, 1)), Asc(Mid$(k$, 2, 1)): End

    Loop While Not ext
End Sub

Function CheckPare (r1, c1, r2, c2)
    'Debug
    CheckPare = True: Exit Function

    If M(r1, c1) <> M(r2, c2) And M(r1, c1) + M(r2, c2) <> 10 Then CheckPare = False: Exit Function
    If r1 * GameWidth + c1 < r2 * GameWidth + c2 Then
        rf = r1
        cf = c1
        rt = r2
        ct = c2
    Else
        rf = r2
        cf = c2
        rt = r1
        ct = c1
    End If

    'String
    r = rf
    C = cf
    ex = False
    Do
        C = C + 1
        If C > GameWidth - 1 Then C = 0: r = r + 1
        If r = rt And C = ct Then CheckPare = True: Exit Function
        If M(r, C) > 0 Then ex = True
    Loop While Not ex

    'Column
    r = rf
    C = cf
    ex = False
    Do
        r = r + 1
        If r = rt And C = ct Then CheckPare = True: Exit Function
        If M(r, C) > 0 Or C <> ct Then ex = True
    Loop While Not ex

    CheckPare = False

End Function

Function Available
    cn = 0

    'String
    r = 0
    C = 0
    D = 0
    Do
        If D = 0 Then
            If M(r, C) > 0 Then D = M(r, C)
        Else
            If M(r, C) > 0 Then
                If M(r, C) = D Or M(r, C) + D = 10 Then cn = cn + 1 Else D = M(r, C)
            End If
        End If
        C = C + 1
        If C > GameWidth - 1 Then C = 0: r = r + 1
    Loop While r < GameHeight

    'Column
    For C = 0 To GameWidth - 1
        r = 0
        D = 0
        Do
            If D = 0 Then
                If M(r, C) > 0 Then D = M(r, C)
            Else
                If M(r, C) > 0 Then
                    If M(r, C) = D Or M(r, C) + D = 10 Then cn = cn + 1 Else D = M(r, C)
                End If
            End If
            r = r + 1
        Loop While r < GameHeight
    Next

    Available = cn
End Function

Function Rewrite
    _AutoDisplay
    Locate 20, 60
    Print "No more maches... Rewrite..."
    _Delay 1


    'Find end point
    rt = GameHeight - 1
    ct = GameWidth - 1
    Do While M(rt, ct) < 0
        ct = ct - 1
    Loop
    rw = rt
    cw = ct
    ct = ct + 1
    If ct > GameWidth - 1 Then ct = 0: rt = rt + 1

    'Rewrite
    r = 0
    c = 0
    Do
        If M(r, c) > 0 Then
            cw = cw + 1
            If cw > GameWidth - 1 Then
                cw = 0
                rw = rw + 1
                Nm(rw) = LastRow
                LastRow = LastRow + 1
                Cn(rw) = 0
            End If
            If rw > MaxRows - 1 Then Rewrite = False: Exit Function
            M(rw, cw) = M(r, c)
            lr = r
        End If
        c = c + 1
        If c > GameWidth - 1 Then c = 0: r = r + 1
    Loop Until r = rt And c = ct
    For r = 0 To lr
        Cn(r) = Cn(r) + 1
    Next
    GameHeight = rw + 1
    Rewrite = True
End Function

Sub Collapse
    For r = GameHeight - 1 To 0 Step -1
        p = True
        For c = 0 To GameWidth - 1
            If M(r, c) <> 0 Then p = False: Exit For
        Next
        If p Then
            For r2 = r To GameHeight - 1
                For c = 0 To GameWidth - 1
                    M(r2, c) = M(r2 + 1, c)
                Next
                Nm(r2) = Nm(r2 + 1)
                Cn(r2) = Cn(r2 + 1)
            Next
            GameHeight = GameHeight - 1
        End If
    Next
End Sub

Sub GameOver
    _AutoDisplay
    Locate 20, 60
    Print "Game Over"
End Sub
