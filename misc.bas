'Второстепенные функции

'Перевод числа в строку с лидирующим нулём (2 знака)
Function ToString$ (i)
    s$ = LTrim$(Str$(i))
    If Len(s$) < 2 Then s$ = "0" + s$
    ToString$ = s$
End Function

'Вычисление цвета курсора
Function CurColor~& (a)
    CurColor~& = _RGBA(84, 97, 252, 128 + Sin(a) * 128)
    a = a + 0.1
    If a > 6.28 Then a = a - 6.28
End Function

'Сборка имени профиля по его индексу
Function ProFileName$ (n)
    ProFileName$ = "Profile" + LTrim$(Str$(n + 1)) + ".sav"
End Function
