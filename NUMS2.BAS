$Debug

Const True = -1
Const False = 0
Const Size = 24
Const MaxLines = 1000
Const MaxRows = 9
Dim Shared kLeft$: kLeft$ = Chr$(0) + Chr$(75)
Dim Shared kRight$: kRight$ = Chr$(0) + Chr$(77)
Dim Shared kUp$: kUp$ = Chr$(0) + Chr$(72)
Dim Shared kDown$: kDown$ = Chr$(0) + Chr$(80)
Dim Shared kEnter$: kEnter$ = Chr$(13)
Dim Shared kEsc$: kEsc$ = Chr$(27)
'Dim Shared kbs$: kbs$ = Chr$(8)

Dim Shared M(MaxLines, MaxRows) 'Main Field
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
    For r = 0 To MaxLines - 1
        For c = 0 To MaxRows - 1
            M(r, c) = -1
        Next
    Next

    r = 0
    c = 0
    For d = 1 To 19
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
    a = 0 'Animation frame

    Do
        'Background
        _Limit 60
        _PutImage (0, 0), Background&

        'GameField
        left = 1280 / 2 - Size * GameWidth / 2
        _PutImage (left + Cc * Size, Cr * Size), Cursor&, 0, (a, 0)-(a + Size - 1, Size - 1)
        For r = 0 To 29
            For c = 0 To GameWidth - 1
                If M(r, c) >= 0 Then
                    sx = M(r, c) * Size
                    If sel And c = sc And r = sr Then s = 1 Else s = 0
                    _PutImage (left + c * Size, r * Size), Nums&, 0, (sx, s * Size)-(sx + Size - 1, s * Size + Size - 1)
                End If
            Next
        Next

        _Display

        'Animation
        a = a + 1
        If a > Size Then a = 0

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
                    If ChechPare(Cr, Cc, sr, sc) Then
                        M(Cr, Cc) = 0
                        M(sr, sc) = 0
                    End If
                    sel = False
                End If
            End If
        End If
        'If Len(k$) > 0 Then Print Asc(Mid$(k$, 1, 1)),Asc(Mid$(k$, 2, 1)):end
    Loop While Not ext
End Sub

Function ChechPare (r1, c1, r2, c2)
    If M(r1, c1) <> M(r2, c2) And M(r1, c1) + M(r2, c2) <> 10 Then ChechPare = False: Exit Function
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
        If r = rt And c = ct Then ChechPare = True: Exit Function
        If M(r, c) > 0 Then ex = True
    Loop While Not ex

    'Column
    r = rf
    c = cf
    ex = False
    Do
        r = r + 1
        If r = rt And c = ct Then ChechPare = True: Exit Function
        If M(r, c) > 0 Or r <> rt Then ex = True
    Loop While Not ex

    ChechPare = False

End Function
