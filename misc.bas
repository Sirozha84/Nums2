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

'Установка громкости
Sub SetVolume
    _SndVol SndClick, VolumeSnd / 10
    _SndVol SndExplode, VolumeSnd / 10
    For i = 0 To MusicCount
        _SndVol Music(i), VolumeMus / 10
    Next
End Sub

'Запуск музыки
Sub MusicStart
    SetVolume
    NrInPlaylist = 0
    _SndPlay Music(NrInPlaylist)
End Sub

'Проверка что музыка не закончилась, а если закончилась - играть следующий трек
Sub MusicPlay
    '    _SndPlay Music(NrInPlaylist)
    If Not _SndPlaying(Music(NrInPlaylist)) Then
        NrInPlaylist = NrInPlaylist + 1
        If NrInPlaylist > MusicCount Then NrInPlaylist = 0
        _SndPlay Music(NrInPlaylist)
    End If
End Sub

