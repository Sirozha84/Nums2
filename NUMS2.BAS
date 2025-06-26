$Debug

Const True = -1
Const False = 0
Const Size = 24
Const MaxRows = 100
Const MaxColumns = 9
Const VisibleRows = 28
Const Border = 9
Const MaxExpPices = 100
'Const ColWhite = _RGB32(255, 255, 255)

'KeyCodes
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

Type Cell
    R As Integer
    C As Integer
    Enable As Integer
End Type

Type StatsT
    Score As Integer
    Stay As Integer
    Available As Integer
End Type

Type ScrollBarT
    AN As Integer
    Left As Integer
    Height As Integer
    Bright As Integer
End Type

Type ExpAnimation
    AN As Integer
    X As Single
    Y As Single
    Xt As Single
    Yt As Single
    Fall As Single
End Type

'InGame Vars
Dim Shared M(MaxRows, MaxColumns) As Integer 'Main Field
Dim Shared Nm(MaxRows) As Integer 'Row Nums
Dim Shared Cn(MaxRows) As Integer 'Rewrites Count
Dim Shared GameWidth As Integer 'Game size
Dim Shared GameHeight As Integer
Dim Shared Cur As Cell
Dim Shared Sel As Cell
Dim Shared Stats As StatsT
Dim Shared Scroll As Integer
Dim Shared ScrollBar As ScrollBarT
Dim Shared LastRow As Integer

'Graphics
Dim Shared Nums&
Dim Shared Background&
Dim Shared Cursor&
Dim Shared Digits&

'Other
Dim Shared ExpAns(MaxExpPices) As ExpAnimation
Dim Shared ExpAnN As Integer

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
        For c = 0 To MaxColumns - 1
            M(r, c) = -1
        Next
    Next

    Stats.Score = 0
    r = 0
    c = 0
    LastRow = 1
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
                Nm(r) = LastRow
                Cn(r) = 0
                GameHeight = r + 1
                c = c + 1
                If c > GameWidth - 1 Then c = 0: r = r + 1: LastRow = LastRow + 1
            Next
        End If
    Next
    Cur.R = 0
    Cur.C = 0
End Sub

Sub Game
    left = 1280 / 2 - Size * GameWidth / 2 'GameField coordinates
    top = Size
    ext = 0 'Exit flag
    Sel.R = 0
    Sel.C = 0
    Sel.Enable = False
    Scroll = 0 'Scroll Shift
    an = 0 'Animation frame
    ScrollBar.AN = 0
    ScrollBar.Left = left + GameWidth * Size + 2
    ScrollBar.Height = 720 - Size * 2 - 1
    ScrollBar.Bright = 0
    AvailableCalc
    StayCalc
    Do
        'Background
        _Limit 60
        _PutImage (0, 0), Background&

        'Calculating scrollbar
        oldscroll = Scroll
        If Scroll > Cur.R - Border Then Scroll = Cur.R - Border
        If Scroll < Cur.R - VisibleRows + Border + 1 Then Scroll = Cur.R - VisibleRows + Border + 1
        If Scroll > GameHeight - VisibleRows Then Scroll = GameHeight - VisibleRows
        If Scroll < 0 Then Scroll = 0
        If oldscroll <> Scroll Then ScrollBar.AN = 1

        'Cursor
        _PutImage (left + Cur.C * Size, top + Cur.R * Size - Scroll * Size), Cursor&, 0, (an, 0)-(an + Size - 1, Size - 1)
        an = an + 1
        If an > Size Then an = 0

        'GameField
        DrawGameField left, top

        'Scrollbar
        DrawScrollbar

        'Stats
        DrawStats

        'Check Stats
        If Stats.Stay = 0 Then
            ExplodeClear
            GameWin
            ext = True
        ElseIf Stats.Available = 0 Then
            ExplodeClear
            If Not Rewrite Then GameOver: ext = True
            AvailableCalc
        End If

        'Explode particles
        ExplodeDraw

        _Display

        'Keyboard
        k$ = InKey$
        If k$ = kLeft$ Or k$ = "o" Then Cur.C = Cur.C - 1
        If k$ = kRight$ Or k$ = "p" Then Cur.C = Cur.C + 1
        If k$ = kUp$ Or k$ = "q" Then Cur.R = Cur.R - 1
        If k$ = kPgUp$ Or k$ = "w" Then Cur.R = Cur.R - VisibleRows
        If k$ = kDown$ Or k$ = "a" Then Cur.R = Cur.R + 1
        If k$ = kPgDn$ Or k$ = "s" Then Cur.R = Cur.R + VisibleRows
        If k$ = kHome$ Or k$ = "W" Then Cur.R = 0
        If k$ = kEnd$ Or k$ = "S" Then Cur.R = GameHeight - 1
        If Cur.R < 0 Then Cur.R = 0
        If Cur.R > GameHeight - 1 Then Cur.R = GameHeight - 1
        If Cur.C < 0 Then Cur.C = 0
        If Cur.C > GameWidth - 1 Then Cur.C = GameWidth - 1
        If k$ = kEsc$ Then ext = -1
        If k$ = " " Or k$ = "0" Then
            If Not Sel.Enable Then
                'First select
                If M(Cur.R, Cur.C) > 0 Then Sel.R = Cur.R: Sel.C = Cur.C: Sel.Enable = True
            Else
                'Second select
                If Sel.R = Cur.R And Sel.C = Cur.C Or M(Cur.R, Cur.C) < 1 Then
                    Sel.Enable = False
                Else
                    'Match
                    If CheckPare(Cur.R, Cur.C, Sel.R, Sel.C) Then
                        M(Sel.R, Sel.C) = 0
                        M(Cur.R, Cur.C) = 0
                        Stats.Score = Stats.Score + 2
                        ExplodeAdd left + Sel.C * Size, top + (Sel.R - Scroll) * Size
                        ExplodeAdd left + Cur.C * Size, top + (Cur.R - Scroll) * Size
                        If Collapse Then ScrollBar.AN = 1
                        AvailableCalc
                        StayCalc
                    End If
                    Sel.Enable = False

                    'Recheck Available
                End If
            End If
        End If

        'Debug...
        If k$ = kEnter$ Then If Not Rewrite Then GameOver: ext = True
        'If Len(k$) > 0 Then Print Asc(Mid$(k$, 1, 1)), Asc(Mid$(k$, 2, 1)): End

    Loop While Not ext
End Sub

Sub DrawGameField (left As Integer, top As Integer)
    'Main Field
    For r = 0 To VisibleRows - 1
        For c = 0 To GameWidth - 1
            If M(r, c) >= 0 Then
                sx = M(r + Scroll, c) * Size
                If Sel.Enable And r = Sel.R - Scroll And c = Sel.C Then S = 1 Else S = 0
                _PutImage (left + c * Size, top + r * Size), Nums&, 0, (sx, S * Size)-(sx + Size - 1, S * Size + Size - 1)
            End If
        Next
        If M(r + Scroll, 0) >= 0 Then
            'Num
            d$ = Str$(Nm(r + Scroll))
            l = Len(d$)
            For i = 2 To l
                n = Val(Mid$(d$, i, 1))
                _PutImage (left - (l - i) * 8 - 17, top + r * Size + 7), Digits&, 0, (n * 8, 0)-(n * 8 + 7, 9)
            Next
            If r > 0 Then
                If Nm(r + Scroll) > Nm(r + Scroll - 1) + 1 Then
                    Line (left - 51, top + r * Size)-(left - 11, top + r * Size - 1), _RGB(64, 64, 64), BF
                End If
            End If
            'Count
            If Cn(r + Scroll) > 0 Then
                d$ = Str$(Cn(r + Scroll))
                l = Len(d$)
                For i = 2 To l
                    n = Val(Mid$(d$, i, 1))
                    _PutImage (ScrollBar.Left + i * 8 - 7, top + r * Size + 7), Digits&, 0, (n * 8, 0)-(n * 8 + 7, 9)
                Next
            End If
        End If
    Next
    'Up and Down Tips
    For c = 0 To GameWidth - 1
        If Scroll > 0 Then
            d = 0
            For r = Scroll - 1 To 0 Step -1
                If M(r, c) > 0 Then d = M(r, c): Exit For
            Next
            _PutImage (left + c * Size, 0), Nums&, 0, (d * Size, 2 * Size)-(d * Size + Size - 1, 3 * Size - 1)
        End If
        If Scroll < GameHeight - VisibleRows Then
            d = 0
            For r = Scroll + VisibleRows To GameHeight
                If M(r, c) > 0 Then d = M(r, c): Exit For
            Next
            _PutImage (left + c * Size, 719 - Size), Nums&, 0, (d * Size, 2 * Size)-(d * Size + Size - 1, 3 * Size - 1)
        End If
    Next
End Sub

Sub DrawScrollbar
    If GameHeight <= VisibleRows Or ScrollBar.AN = 0 Then Exit Sub
    l = VisibleRows / GameHeight * ScrollBar.Height
    t = Size + Scroll / GameHeight * ScrollBar.Height
    If ScrollBar.AN > 0 Then ScrollBar.Bright = ScrollBar.Bright + 10
    If ScrollBar.Bright > 512 Then ScrollBar.AN = -1
    If ScrollBar.AN < 0 Then ScrollBar.Bright = ScrollBar.Bright - 5
    If ScrollBar.Bright < 0 Then ScrollBar.AN = 0
    br = ScrollBar.Bright
    If br > 255 Then br = 255
    col& = _RGBA32(255, 255, 255, br / 2)
    Line (ScrollBar.Left, t + 1)-(ScrollBar.Left, t + l - 1), col&
    Line (ScrollBar.Left + 1, t)-(ScrollBar.Left + 1, t + l), col&
    Line (ScrollBar.Left + 2, t)-(ScrollBar.Left + 2, t + l), col&
    Line (ScrollBar.Left + 3, t + 1)-(ScrollBar.Left + 3, t + l - 1), col&
End Sub

Sub DrawStats
    Locate 10, 10: Print "Available:"; Stats.Available
    Locate 15, 10: Print "Score:"; Stats.Score
    Locate 20, 10: Print "Stay:"; Stats.Stay
End Sub

Function CheckPare (r1 As Integer, c1 As Integer, r2 As Integer, c2 As Integer)
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
    Ex = False
    Do
        C = C + 1
        If C > GameWidth - 1 Then C = 0: r = r + 1
        If r = rt And C = ct Then CheckPare = True: Exit Function
        If M(r, C) > 0 Then Ex = True
    Loop While Not Ex

    'Column
    r = rf
    C = cf
    Ex = False
    Do
        r = r + 1
        If r = rt And C = ct Then CheckPare = True: Exit Function
        If M(r, C) > 0 Or C <> ct Then Ex = True
    Loop While Not Ex

    CheckPare = False

End Function

Sub AvailableCalc
    Stats.Available = 0

    'String
    r = 0
    C = 0
    D = 0
    Do
        If D = 0 Then
            If M(r, C) > 0 Then D = M(r, C)
        Else
            If M(r, C) > 0 Then
                If M(r, C) = D Or M(r, C) + D = 10 Then Stats.Available = Stats.Available + 1 Else D = M(r, C)
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
                    If M(r, C) = D Or M(r, C) + D = 10 Then Stats.Available = Stats.Available + 1 Else D = M(r, C)
                End If
            End If
            r = r + 1
        Loop While r < GameHeight
    Next
End Sub

Sub StayCalc
    Stats.Stay = 0
    For r = 0 To GameHeight - 1
        For c = 0 To GameWidth - 1
            If M(r, c) > 0 Then Stats.Stay = Stats.Stay + 1
        Next
    Next
End Sub

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
    StayCalc
End Function

Function Collapse
    Collapse = False
    For r = GameHeight - 1 To 0 Step -1
        p = True
        For c = 0 To GameWidth - 1
            If M(r, c) <> 0 Then p = False: Exit For
        Next
        If p Then
            Collapse = True
            For r2 = r To GameHeight - 1
                For c = 0 To GameWidth - 1
                    M(r2, c) = M(r2 + 1, c)
                Next
                Nm(r2) = Nm(r2 + 1)
                Cn(r2) = Cn(r2 + 1)
            Next
            GameHeight = GameHeight - 1
            If Cur.R > r Then Cur.R = Cur.R - 1
        End If
    Next
End Function

Sub ExplodeDraw
    For i = 0 To MaxExpPices
        If ExpAns(i).AN > 0 Then
            x = ExpAns(i).X
            y = ExpAns(i).Y + ExpAns(i).Fall
            Line (x, y)-(x + 2, y + 2), _RGBA(255, 255, 255, ExpAns(i).AN), BF
            ExpAns(i).AN = ExpAns(i).AN - 3
            ExpAns(i).X = ExpAns(i).X + ExpAns(i).Xt
            ExpAns(i).Y = ExpAns(i).Y + ExpAns(i).Yt
            ExpAns(i).Fall = ExpAns(i).Fall * 1.1
        End If
    Next
End Sub

Sub ExplodeAdd (x As Single, y As Single)
    For i = 1 To 15
        ExpAns(ExpAnN).AN = 255
        ExpAns(ExpAnN).X = x + Size / 2 + Rnd * 8 - 4
        ExpAns(ExpAnN).Y = y + Size / 2 + Rnd * 20 - 10
        ExpAns(ExpAnN).Xt = Rnd * 6 - 3
        ExpAns(ExpAnN).Yt = Rnd * 6 - 3
        ExpAns(ExpAnN).Fall = 5
        ExpAnN = ExpAnN + 1
        If ExpAnN > MaxExpPices Then ExpAnN = 0
    Next
End Sub

Sub ExplodeClear
    For i = 0 To MaxExpPices
        ExpAns(i).AN = 0
    Next
End Sub

Sub GameOver
    _AutoDisplay
    Locate 20, 60
    Print "Game Over"
End Sub

Sub GameWin
    _AutoDisplay
    Locate 20, 60
    Print "Game Win"
End Sub

