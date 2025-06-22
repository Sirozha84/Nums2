$Debug

Const Size = 24
Const MaxLines = 1000
Const MaxRows = 9

Dim Shared GameWidth
Dim Shared M(MaxLines, MaxRows)
Dim Shared Nums&
Dim Shared Background&
Dim Shared Cursor&

Init
GameInit

_PutImage (0, 0), Background&
left = 1280 / 2 - Size * GameWidth / 2
For i = 0 To GameWidth - 1
    For j = 0 To 29
        'x = i * Size
        'y = j * Size
        sx = M(j, i) * Size
        _PutImage (left + i * Size, j * Size), Nums&, 0, (sx, 0)-(sx + Size - 1, Size - 1)
    Next
Next


'_Display



Sub Init
    $Resize:Smooth
    Screen _NewImage(1280, 720, 32)
    Background& = _LoadImage("Graphics\background.png", 32)
    Nums& = _LoadImage("Graphics\nums.png", 32)
    Cursor& = _LoadImage("Graphics\cursor.png", 32)
End Sub

Sub GameInit
    GameWidth = 9
    For i = 0 To MaxLines - 1
        For j = 0 To MaxRows - 1
            M(i, j) = 0
        Next
    Next
    x = 0
    y = 0
    For d = 1 To 19
        s$ = Str$(d)
        s$ = Right$(s$, Len(s$) - 1)
        ok = -1
        For i = 1 To Len(s$)
            If Mid$(s$, i, 1) = "0" Then ok = 0
        Next
        If ok Then
            For i = 1 To Len(s$)
                M(x, y) = Val(Mid$(s$, i, 1))
                y = y + 1
                If y > GameWidth - 1 Then y = 0: x = x + 1
            Next
        End If
    Next
End Sub
