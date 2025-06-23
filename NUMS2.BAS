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
'Dim Shared kbs$: kbs$ = Chr$(8)

Dim Shared M(MaxRows, MaxColumns) 'Main Field
Dim Shared GameWidth 'Gzme size
Dim Shared GameHeight
Dim Shared Cr 'Cursor
Dim Shared Cc

Dim Shared Nums&
Dim Shared Background&
Dim Shared Cursor&



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
End Sub

Sub GameInit
    GameWidth = 9
    For r = 0 To MaxRows - 1
        For c = 0 To MaxColumns - 1
            M(r, c) = -1
        Next
    Next

    r = 0
    c = 0
    For d = 1 To 19 '19
        s$ = Str$(d)
        s$ = Right$(s$, Len(s$) - 1)
        ok = True
        For i = 1 To Len(s$)
            If Mid$(s$, i, 1) = "0" Then ok = False
        Next
        If ok Then
            For i = 1 To Len(s$)
                M(r, c) = Val(Mid$(s$, i, 1))
                GameHeight = r + 1
                c = c + 1
                If c > GameWidth - 1 Then c = 0: r = r + 1
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
        If k$ = kUp$ Or k$ = "q" Then Cr = Cr - 1: If Cr < 0 Then Cr = 0
        If k$ = kDown$ Or k$ = "a" Then Cr = Cr + 1: If Cr > GameHeight - 1 Then Cr = GameHeight - 1
        If k$ = kEsc$ Then ext = -1
        If k$ = " " Then
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
                    End If
                    sel = False

                    'Recheck Available
                    av = Available
                End If
            End If
        End If

        'Debug...
        'If k$ = kEnter$ Then r = Rewrite
        'If Len(k$) > 0 Then Print Asc(Mid$(k$, 1, 1)),Asc(Mid$(k$, 2, 1)):end

    Loop While Not ext
End Sub

Function CheckPare (r1, c1, r2, c2)
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
    c = cf
    ex = False
    Do
        c = c + 1
        If c > GameWidth - 1 Then c = 0: r = r + 1
        If r = rt And c = ct Then CheckPare = True: Exit Function
        If M(r, c) > 0 Then ex = True
    Loop While Not ex

    'Column
    r = rf
    c = cf
    ex = False
    Do
        r = r + 1
        If r = rt And c = ct Then CheckPare = True: Exit Function
        If M(r, c) > 0 Or c <> ct Then ex = True
    Loop While Not ex

    CheckPare = False

End Function

Function Available
    cn = 0

    'String
    r = 0
    c = 0
    d = 0
    Do
        If d = 0 Then
            If M(r, c) > 0 Then d = M(r, c)
        Else
            If M(r, c) > 0 Then
                If M(r, c) = d Or M(r, c) + d = 10 Then cn = cn + 1 Else d = M(r, c)
            End If
        End If
        c = c + 1
        If c > GameWidth - 1 Then c = 0: r = r + 1
    Loop While r < GameHeight

    'Column
    For c = 0 To GameWidth - 1
        r = 0
        d = 0
        Do
            If d = 0 Then
                If M(r, c) > 0 Then d = M(r, c)
            Else
                If M(r, c) > 0 Then
                    If M(r, c) = d Or M(r, c) + d = 10 Then cn = cn + 1 Else d = M(r, c)
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
            If cw > GameWidth - 1 Then cw = 0: rw = rw + 1
            If rw > MaxRows - 1 Then Rewrite = False: Exit Function
            M(rw, cw) = M(r, c)
        End If
        c = c + 1
        If c > GameWidth - 1 Then c = 0: r = r + 1
    Loop Until r = rt And c = ct
    GameHeight = rw + 1
    Rewrite = True
End Function

Sub GameOver
    _AutoDisplay
    Locate 20, 60
    Print "Game Over"
End Sub
