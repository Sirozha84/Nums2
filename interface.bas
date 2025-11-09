'Необходимые общие переменные
'ScrWidth - ширина экрана
'ScrHeight - высота экрана
'Font - графика со шрифтом
'FntW - ширина символа
'FntH - высота символа

'Печать в центре экрана по порядковому номеру строки из общего их количества
'n - номер строки (начиная с 0)
'max - последний индекс
's$ - строка
Sub PrintLn (n, max, s$)
    PrintCn ScrHeight / 2 - FntH / 2 - max * FntH + n * FntH * 2, s$
End Sub

'Печать в центре экрана в указанной высоте
'y - координата
's$ - строка
Sub PrintCn (y, s$)
    PrintAt ScrWidth / 2 - Len(s$) * FntW / 2, y, s$
End Sub

'Печать в чётко указанных координатах
'x, y - координаты
's$ - строка
Sub PrintAt (x, y, s$)
    For i = 0 To Len(s$) - 1
        c = Asc(Mid$(s$, i + 1, 1))
        Select Case c
            Case 32 To 127: n = c - 32
            Case 192 To 255: n = c - 96
        End Select
        xi = (n Mod 32) * FntW
        yi = Int(n / 32) * FntH
        _PutImage (x + i * FntW, y), Font, 0, (xi, yi)-(xi + FntW - 1, yi + FntH - 1)
    Next
End Sub

'Временное сообщение
'mode - 0-нейтральное, 1-хорошее, 2-плохое
Sub MsgBox (m$, mode)
    PCopy 0, 1
    DrawWindow 1, mode
    PrintLn 0, 0, m$
    'PrintCn ScrHeight / 2 - FntH / 2, m$
    _Display
    _Delay 1
    If mode > 0 Then _Delay 3
    PCopy 1, 0
End Sub

'Анимированное появление окошка
'strings - Количество строк
'mode - 0-нейтральное, 1-хорошее, 2-плохое
Sub DrawWindow (strings, mode)
    If mode = 0 Then r = 64: g = 64: b = 64
    If mode = 1 Then r = 0: g = 128: b = 0
    If mode = 2 Then r = 128: g = 0: b = 0
    For s = 0.2 To 1.01 Step 0.1
        _Limit 60
        x1 = ScrWidth / 2 - s * FntW * 22
        y1 = ScrHeight / 2 - s * (strings + 2) * FntH / 2
        x2 = ScrWidth / 2 + s * FntW * 22
        y2 = ScrHeight / 2 + s * (strings + 2) * FntH / 2
        For i = 0 To 7
            c = 1 - i * 0.1
            Line (x1 + i, y1 + i)-(x2 - i, y2 - i), _RGB(r * c, g * c, b * c), BF
        Next
        _Display
    Next
End Sub

'Выведение экрана
'В начале сцены Fade устанавливается в 255 и каждый раз перед _DISPLAY вызывается FadeIn
Sub FadeIn
    If Fade > 0 Then
        Fade = Fade - 8
        Line (0, 0)-(ScrWidth - 1, ScrHeight - 1), _RGBA(0, 0, 0, Fade), BF
    End If
End Sub

'Затухание экрана
'Запускается перед выходом из сцены
Sub FadeOut
    PCopy 0, 1
    For b = 0 To 255 Step 8
        _Limit 60
        PCopy 1, 0
        Line (0, 0)-(ScrWidth - 1, ScrHeight - 1), _RGBA(0, 0, 0, b), BF
        _Display
    Next
End Sub
