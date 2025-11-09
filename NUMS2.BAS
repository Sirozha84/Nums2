$Debug

Type ProfileT
    Name As String
    Start As String
    Last As String
    Score As Long
    Rewrites As Integer
    LastRow As Integer
    Hrs As Integer
    Min As Integer
    Sec As Integer
End Type

Type Cell
    R As Integer
    C As Integer
    Enable As Integer
    AnyPair As Integer
End Type

Type StatsT
    Stay As Integer
    StayRows As Integer
    Available As Integer
End Type

Type ScrollBarT
    AN As Integer
    Left As Integer
    Height As Integer
    Bright As Integer
End Type

Type ExpPartT 'Частица взрыва
    AN As Integer
    X As Single
    Y As Single
    Xt As Single
    Yt As Single
    Fall As Single
End Type

Const True = -1
Const False = 0
Const ScrWidth = 1280 'Screen Size
Const ScrHeight = 720
Const FntW = 12 'Font Size
Const FntH = 24
Const Size = 24 'Cells Size
Const MaxRows = 999
Const MaxColumns = 9
Const VisibleRows = 28
Const Border = 9
Const MaxExpPices = 999 'Максимальное число частиц взрыва
Const MaxStatistic = 99
Const MaxProfiles = 5
Const PropFile = "config.sav"

'Общие переменные для меню
Dim Shared key$
Dim Shared Fade As Integer 'FadeIn Grade (255 in start, 0 - normal)
ReDim Shared prs(4) As String 'Список профилей (для выбора в меню)
Dim Shared ProfileFile As String
Dim Shared LastName As String
Dim Shared ColGray&: ColGray& = _RGB32(64, 64, 64)

'Внутриигровые переменные
Dim Shared Profile As ProfileT
Dim Shared M(MaxRows, MaxColumns) As Integer 'Main Field
Dim Shared Nm(MaxRows) As Integer 'Row Nums
Dim Shared Cn(MaxRows) As Integer 'Rewrites Count
Dim Shared GameWidth As Integer 'Game size
Dim Shared GameHeight As Integer
Dim Shared Cur As Cell
Dim Shared Sel As Cell
Dim Shared Stats As StatsT
Dim Shared Left As Integer 'Point for draw field
Dim Shared Top As Integer
Dim Shared Scroll As Integer 'Scroll Shift
Dim Shared ScrollBar As ScrollBarT
Dim Shared Statistic(MaxStatistic) As Integer
Dim Shared ExpAns(MaxExpPices) As ExpPartT 'Массив частиц взрыва
Dim Shared ExpAnN As Integer 'Индекс для следующей частицы

'Графика
Dim Shared Font
Dim Shared Nums
Dim Shared Background
Dim Shared Cursor
Dim Shared Digits

Init
MainMenu

Sub Init
    'Экран
    _Title "Nums2"
    $Resize:Smooth
    Screen _NewImage(ScrWidth, ScrHeight, 32)
    'Загрузка графики
    Font = _LoadImage("Graphics\font.png", 32)
    Background = _LoadImage("Graphics\background.png", 32)
    Nums = _LoadImage("Graphics\nums.png", 32)
    Cursor = _LoadImage("Graphics\cursor.png", 32)
    Digits = _LoadImage("Graphics\digits.png", 32)
    'Загрузка параметров
    If Not _FileExists(PropFile) Then Exit Sub
    Open PropFile For Input As #1
    Input #1, LastName
    Close #1
End Sub

'Сохранение настроек
Sub SaveOptions
    Open PropFile For Output As #1
    Write #1, LastName
    Close #1
End Sub

Sub MainMenu
    ext = False
    max = 5
    Dim m(max) As String
    m(0) = "Играть"
    m(1) = "Настройки"
    m(2) = "История"
    m(3) = "Инструкция"
    m(4) = "Об игре"
    m(5) = "Выход"
    k = 0
    Fade = 255
    a = 0
    Do
        _Limit 60
        _PutImage (0, 0), Background
        PrintCn 50, "NUMS 2"
        For i = 0 To max
            y = 228 + i * FntH * 2
            If k = i Then
                Line (ScrWidth / 2 - 100, y - FntH / 2)-_
                    Step(200, FntH * 2 - 1), CurColor~&(a), BF
            End If
            PrintCn y, m(i)
        Next
        PrintCn 680, "Enter - выбор, Esc - выход"
        FadeIn
        _Display
        key$ = InKey$
        If kUp Then a = 0: k = k - 1: If k < 0 Then k = max
        If kDown Then a = 0: k = k + 1: If k > max Then k = 0
        If kEsc Then ext = True
        If kEnter Then
            If k = 0 Then SelectProfile
            'if k = 1 then
            'if k = 2 then
            'if k = 3 then
            'if k = 4 then
            If k = 5 Then ext = True
        End If
    Loop While Not ext
    FadeOut
    System
End Sub

Sub SelectProfile
    LoadProfiles
    k = 0
    a = 0
    Do
        _Limit 60
        _PutImage (0, 0), Background
        PrintCn 50, "Выбери профиль"

        For i = 0 To MaxProfiles - 1
            y = 252 + i * FntH * 2
            If k = i Then
                Line (ScrWidth / 2 - 360, y - FntH / 2)-_
                    Step(740, FntH * 2 - 1), CurColor~&(a), BF
            End If
            If prs(i) <> "" Then n$ = prs(i) Else n$ = "Новая игра"
            PrintCn y, n$
        Next
        PrintCn 680, "Enter - выбор, Esc - назад, D - удалить"
        FadeIn
        _Display
        key$ = InKey$
        If kUp Then a = 0: k = k - 1: If k < 0 Then k = MaxProfiles - 1
        If kDown Then a = 0: k = k + 1: If k > MaxProfiles - 1 Then k = 0
        If kEsc Then ext = True
        If kEnter Then
            ProfileFile = ProFileName(k)
            If prs(k) = "" Then
                If NewProfile Then
                    NewGame
                    ext = True
                End If
            Else
                LoadGame
                ext = True
            End If
            If ext Then
                FadeOut
                Game
                Fade = 255
            End If
        End If
        If (key$ = "d" Or key$ = "в") And prs(k) <> "" Then DeleteProfile (ProFileName(k))
    Loop While Not ext
    'FadeOut

End Sub

Sub LoadProfiles
    ReDim prs(MaxProfiles) As String
    n = 0
    For i = 0 To MaxProfiles - 1
        ProfileFile = ProFileName(n)
        n$ = ProFileName(i)
        If _FileExists(n$) Then
            If n <> i Then Name n$ As ProfileFile
            LoadGame
            prs(n) = Profile.Name + "   " + Profile.Start + " - " + Profile.Last
            n = n + 1
        End If
    Next
End Sub

Function NewProfile
    DrawWindow 3, 0
    PrintCn 324, "Имя профиля"
    PCopy 0, 1
    n$ = LastName
    x = ScrWidth / 2 - 10 * FntW / 2
    y = 372
    a = 0
    ext = False
    Do
        _Limit 60
        PCopy 1, 0
        PrintAt x, y, n$
        Line (x + Len(n$) * FntW, y)-Step(FntW - 1, FntH - 1), CurColor(a), BF
        _Display
        key$ = InKey$
        If key$ <> "" Then
            If Asc(key$) >= 32 And Len(n$) < 10 Then n$ = n$ + key$: a = 0
        End If
        If key$ = Chr$(8) And Len(n$) > 0 Then n$ = Left$(n$, Len(n$) - 1): a = 0
        If key$ = Chr$(13) Then
            LastName = n$
            SaveOptions
            Do While Len(n$) < 10: n$ = n$ + " ": Loop
            Profile.Name = n$
            NewProfile = True
            ext = True
        End If
        If kEsc Then
            NewProfile = False
            ext = True
        End If
    Loop While Not ext
End Function

'Диалог удаления профиля
Sub DeleteProfile (pf$)
    DrawWindow 3, 0
    PrintCn 324, "Уверены?"
    PCopy 0, 1
    k = 1
    x = ScrWidth / 2 - 7 * FntW / 2
    a = 0
    ext = False
    Do
        _Limit 60
        PCopy 1, 0
        Line (x + k * 54 - 18, 360)-Step(60, 47), CurColor(a), BF
        PrintCn 372, "Да  Нет"
        _Display
        key$ = InKey$
        If kLeft Or kRight Then k = 1 - k: a = 0
        If kEsc Then ext = True
        If kEnter Then
            If k = 0 Then
                Kill pf$
                LoadProfiles
            End If
            ext = True
        End If
    Loop While Not ext
End Sub

'Инициализация новой игры
Sub NewGame
    'Профиль
    Profile.Start = Date$ + " " + Time$
    Profile.Last = Date$ + " " + Time$
    Profile.Score = 0
    Profile.Rewrites = 0
    Profile.Hrs = 0
    Profile.Min = 0
    Profile.Sec = 0
    'Cursor
    Cur.R = 0
    Cur.C = 0
    Cur.AnyPair = False
    'Field
    GameWidth = 9
    For r = 0 To MaxRows - 1
        For c = 0 To MaxColumns - 1
            M(r, c) = -1
        Next
    Next
    r = 0
    c = 0
    Profile.LastRow = 1
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
                Nm(r) = Profile.LastRow
                Cn(r) = 0
                GameHeight = r + 1
                c = c + 1
                If c > GameWidth - 1 Then
                    c = 0
                    r = r + 1
                    Profile.LastRow = Profile.LastRow + 1
                End If
            Next
        End If
    Next
    'Statistic
    For i = 0 To MaxStatistic
        Statistic(i) = 0
    Next
End Sub

Sub LoadGame
    Dim b As Integer
    Open ProfileFile For Random As 1 Len = 4
    i = 1
    'Profile
    Profile.Name = ""
    For j = 1 To 10
        Get 1, i, b: i = i + 1
        Profile.Name = Profile.Name + Chr$(b)
    Next
    Profile.Start = ""
    For j = 1 To 19
        Get 1, i, b: i = i + 1
        Profile.Start = Profile.Start + Chr$(b)
    Next
    Profile.Last = ""
    For j = 1 To 19
        Get 1, i, b: i = i + 1
        Profile.Last = Profile.Last + Chr$(b)
    Next
    'Stats
    Get 1, i, Profile.Score: i = i + 1
    Get 1, i, Profile.Rewrites: i = i + 1
    Get 1, i, Profile.LastRow: i = i + 1
    Get 1, i, Profile.Hrs: i = i + 1
    Get 1, i, Profile.Min: i = i + 1
    Get 1, i, Profile.Sec: i = i + 1
    'Cursor
    Get 1, i, Cur.R: i = i + 1
    Get 1, i, Cur.C: i = i + 1
    Get 1, i, Cur.AnyPair: i = i + 1
    'Field
    For r = 0 To MaxRows - 1
        For c = 0 To MaxColumns - 1
            M(r, c) = -1
        Next
    Next
    Get 1, i, GameHeight: i = i + 1
    Get 1, i, GameWidth: i = i + 1
    For r = 0 To GameHeight - 1
        For c = 0 To GameWidth - 1
            Get 1, i, M(r, c): i = i + 1
        Next
    Next
    'Numbers and counts
    For r = 0 To GameHeight - 1
        Get 1, i, Nm(r): i = i + 1
    Next
    For r = 0 To GameHeight - 1
        Get 1, i, Cn(r): i = i + 1
    Next
    'Statistic
    For j = 0 To MaxStatistic
        Get 1, i, Statistic(j): i = i + 1
    Next
    Close
End Sub

Sub SaveGame
    Dim b As Integer
    If _FileExists(ProfileFile) Then Kill ProfileFile
    Open ProfileFile For Random As 1 Len = 4
    i = 1
    'Profile
    For j = 1 To 10
        b = Asc(Profile.Name, j)
        Put 1, i, b: i = i + 1
    Next
    For j = 1 To 19
        b = Asc(Profile.Start, j)
        Put 1, i, b: i = i + 1
    Next
    Profile.Last = Date$ + " " + Time$
    For j = 1 To 19
        b = Asc(Profile.Last, j)
        Put 1, i, b: i = i + 1
    Next
    'Stats
    Put 1, i, Profile.Score: i = i + 1
    Put 1, i, Profile.Rewrites: i = i + 1
    Put 1, i, Profile.LastRow: i = i + 1
    Put 1, i, Profile.Hrs: i = i + 1
    Put 1, i, Profile.Min: i = i + 1
    Put 1, i, Profile.Sec: i = i + 1
    'Cursor
    Put 1, i, Cur.R: i = i + 1
    Put 1, i, Cur.C: i = i + 1
    Put 1, i, Cur.AnyPair: i = i + 1
    'Field
    Put 1, i, GameHeight: i = i + 1
    Put 1, i, GameWidth: i = i + 1
    For r = 0 To GameHeight - 1
        For c = 0 To GameWidth - 1
            Put 1, i, M(r, c): i = i + 1
        Next
    Next
    'Numbers and counts
    For r = 0 To GameHeight - 1
        Put 1, i, Nm(r): i = i + 1
    Next
    For r = 0 To GameHeight - 1
        Put 1, i, Cn(r): i = i + 1
    Next
    'Statistic
    For j = 0 To MaxStatistic
        Put 1, i, Statistic(j): i = i + 1
    Next
    Close
End Sub

Sub Game
    Left = ScrWidth / 2 - Size * GameWidth / 2 'GameField coordinates
    Top = Size
    Sel.R = 0
    Sel.C = 0
    Sel.Enable = False
    Scroll = 0
    ScrollBar.AN = 0
    ScrollBar.Left = Left + GameWidth * Size + 2
    ScrollBar.Height = ScrHeight - Size * 2 - 1
    ScrollBar.Bright = 0
    an = 0 'Animation frame
    dl = 0 'Delay before messages
    ast = 0 'AutoSaveTimer
    ext = False 'Exit flag
    Fade = 255
    tme = Int(Timer)
    AvailableCalc
    StayCalc
    Do
        'Background
        _Limit 60
        _PutImage (0, 0), Background

        'Calculating scrollbar
        oldscroll = Scroll
        If Scroll > Cur.R - Border Then Scroll = Cur.R - Border
        If Scroll < Cur.R - VisibleRows + Border + 1 Then Scroll = Cur.R - VisibleRows + Border + 1
        If Scroll > GameHeight - VisibleRows Then Scroll = GameHeight - VisibleRows
        If Scroll < 0 Then Scroll = 0
        If oldscroll <> Scroll Then ScrollBar.AN = 1

        'Cursor
        _PutImage (Left + Cur.C * Size, Top + Cur.R * Size - Scroll * Size), Cursor, 0, (an, 0)-(an + Size - 1, Size - 1)
        an = an + 1
        If an > Size Then an = 0

        DrawGameField Left, Top
        DrawScrollbar
        DrawStats an

        'Check game status
        If Stats.Stay = 0 Then
            dl = dl + 1
            If dl > 60 Then
                MsgBox "Победа!!!", 1
                ext = True
            End If
        ElseIf Stats.Available = 0 And Not Cur.AnyPair Then
            dl = dl + 1
            If dl > 60 Then
                dl = 0
                If Rewrite Then
                    MsgBox "Ходов больше нет! Переписываем...", 0
                Else
                    MsgBox "Ходов больше нет! Игра проиграна!", 2
                    ext = True
                End If
                AvailableCalc
                'Check avaliable moves after rewrite
                If Stats.Available = 0 Then Cur.AnyPair = True
                'Check infinitere writes
                For i = 1 To 3
                    if Statistic(MaxStatistic) = Statistic(MaxStatistic - i) and _
                        Statistic(MaxStatistic) = Statistic(MaxStatistic - i - i) then _
                        Cur.AnyPair = True
                Next
            End If
        End If

        ExplodeDraw
        FadeIn
        _Display

        'Keyboard
        key$ = InKey$
        If kLeft Then Cur.C = Cur.C - 1
        If kRight Then Cur.C = Cur.C + 1
        If kUp Then Cur.R = Cur.R - 1
        If kPgUp Then Cur.R = Cur.R - VisibleRows
        If kDown Then Cur.R = Cur.R + 1
        If kPgDn Then Cur.R = Cur.R + VisibleRows
        If kHome Then Cur.R = 0
        If kEnd Then Cur.R = GameHeight - 1
        If Cur.R < 0 Then Cur.R = 0
        If Cur.R > GameHeight - 1 Then Cur.R = GameHeight - 1
        If Cur.C < 0 Then Cur.C = 0
        If Cur.C > GameWidth - 1 Then Cur.C = GameWidth - 1
        If kEsc Then SaveGame: ext = True
        If kEnter Then Fire

        'AautoSave
        ast = ast + 1
        If ast > 3600 Then SaveGame: ast = 0

        'TimeCalc
        If tme <> Int(Timer) Then
            tme = Int(Timer)
            Profile.Sec = Profile.Sec + 1
            If Profile.Sec > 59 Then
                Profile.Sec = 0
                Profile.Min = Profile.Min + 1
                If Profile.Min > 59 Then
                    Profile.Min = 0
                    Profile.Hrs = Profile.Hrs + 1
                End If
            End If
        End If

    Loop While Not ext
    FadeOut
End Sub

Sub Fire
    If Not Sel.Enable Then
        'First select
        If M(Cur.R, Cur.C) > 0 Then Sel.R = Cur.R: Sel.C = Cur.C: Sel.Enable = True
    Else
        'Second select
        If Sel.R = Cur.R And Sel.C = Cur.C Or M(Cur.R, Cur.C) < 1 Then
            Sel.Enable = False
        Else
            'Match
            If CheckPair(Cur.R, Cur.C, Sel.R, Sel.C) Then
                M(Sel.R, Sel.C) = 0
                M(Cur.R, Cur.C) = 0
                Profile.Score = Profile.Score + 2
                ExplodeAdd Left + Sel.C * Size, Top + (Sel.R - Scroll) * Size
                ExplodeAdd Left + Cur.C * Size, Top + (Cur.R - Scroll) * Size
                If Collapse Then ScrollBar.AN = 1
                AvailableCalc
                StayCalc
            End If
            Sel.Enable = False
        End If
    End If
End Sub

Sub DrawGameField (left As Integer, top As Integer)
    'Main Field
    For r = 0 To VisibleRows - 1
        For c = 0 To GameWidth - 1
            If M(r, c) >= 0 Then
                sx = M(r + Scroll, c) * Size
                If Sel.Enable And r = Sel.R - Scroll And c = Sel.C Then S = 1 Else S = 0
                _PutImage (left + c * Size, top + r * Size), Nums, 0, (sx, S * Size)-(sx + Size - 1, S * Size + Size - 1)
            End If
        Next
        If M(r + Scroll, 0) >= 0 Then
            'Num
            DrawNum left - 10, top + r * Size + 7, Nm(r + Scroll), 1
            If r > 0 Then
                If Nm(r + Scroll) > Nm(r + Scroll - 1) + 1 Then _
                    Line (left - 51, top + r * Size)-(left - 11, top + r * Size - 1), ColGray&, BF
            End If
            'Count
            If Cn(r + Scroll) > 0 Then DrawNum left + GameWidth * Size + 10, top + r * Size + 7, Cn(r + Scroll), 0
        End If
    Next
    'Up and Down Tips
    For c = 0 To GameWidth - 1
        If Scroll > 0 Then
            d = 0
            For r = Scroll - 1 To 0 Step -1
                If M(r, c) > 0 Then d = M(r, c): Exit For
            Next
            _PutImage (left + c * Size, 0), Nums, 0, (d * Size, 2 * Size)-(d * Size + Size - 1, 3 * Size - 1)
        End If
        If Scroll < GameHeight - VisibleRows Then
            d = 0
            For r = Scroll + VisibleRows To GameHeight
                If M(r, c) > 0 Then d = M(r, c): Exit For
            Next
            _PutImage (left + c * Size, 719 - Size), Nums, 0, (d * Size, 2 * Size)-(d * Size + Size - 1, 3 * Size - 1)
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
    col = _RGBA32(255, 255, 255, br / 2)
    Line (ScrollBar.Left, t + 1)-(ScrollBar.Left, t + l - 1), col
    Line (ScrollBar.Left + 1, t)-(ScrollBar.Left + 1, t + l), col
    Line (ScrollBar.Left + 2, t)-(ScrollBar.Left + 2, t + l), col
    Line (ScrollBar.Left + 3, t + 1)-(ScrollBar.Left + 3, t + l - 1), col
End Sub

Sub DrawStats (an)
    'MainStats
    If Cur.AnyPair And an < 10 Then PrintAt 120, 80, "Доступна любая пара!"
    PrintAt 50, 228, "         Время: " + _
        ToString$(Profile.Hrs) + ":" + ToString$(Profile.Min) + ":" + ToString$(Profile.Sec)
    PrintAt 50, 276, "          Очки:" + Str$(Profile.Score)
    PrintAt 50, 324, "    Переписано:" + Str$(Profile.Rewrites)
    PrintAt 50, 372, "Осталось строк:" + Str$(Stats.StayRows) + " из" + Str$(Profile.LastRow - 1)
    PrintAt 50, 420, " Осталось цифр:" + Str$(Stats.Stay)
    PrintAt 50, 468, "Ходов доступно:" + Str$(Stats.Available)

    'Statistic
    PrintAt 850, 228, "Статистика"
    x = 850
    y = 276
    w = 400
    h = 216
    Line (x, y)-(x + w, y + h), ColGray&, B
    col& = _RGB(255, 255, 255)
    max = 0
    For i = 0 To MaxStatistic
        If max < Statistic(i) Then max = Statistic(i)
    Next
    max = max - 1
    Select Case max
        Case 0 To 100: m = Int(max / 20) * 20 + 20
        Case 101 To 1000: m = Int(max / 100) * 100 + 100
        Case Else: m = Int(max / 1000) * 1000 + 1000
    End Select
    If m < 20 Then m = 20
    For i = 0 To 1 Step 0.25
        Line (x - 4, y + h * i)-(x + w, y + h * i), ColGray&
        DrawNum x - 6, y + h * i - 5, m * (1 - i), 1
    Next
    Line (x + w, y + h - Statistic(MaxStatistic) / m * h)-_
        (x + w + 4, y + h - Statistic(MaxStatistic) / m * h), ColGray&
    DrawNum x + w + 6, y + h - Statistic(MaxStatistic) / m * h - 5, Statistic(MaxStatistic), 0
    For i = 0 To MaxStatistic - 1
        Line (x + i / MaxStatistic * w, y + h - Statistic(i) / m * h)- _
            (x + (i + 1) / MaxStatistic * w, y + h - Statistic(i + 1) / m * h), col&
    Next
End Sub

Function CheckPair (r1 As Integer, c1 As Integer, r2 As Integer, c2 As Integer)

    'AnyPair
    If Cur.AnyPair Then
        Cur.AnyPair = False
        CheckPair = True
        Exit Function
    End If

    'Prepare pair
    If M(r1, c1) <> M(r2, c2) And M(r1, c1) + M(r2, c2) <> 10 Then CheckPair = False: Exit Function
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

    'String check
    r = rf
    C = cf
    Ex = False
    Do
        C = C + 1
        If C > GameWidth - 1 Then C = 0: r = r + 1
        If r = rt And C = ct Then CheckPair = True: Exit Function
        If M(r, C) > 0 Then Ex = True
    Loop While Not Ex

    'Column check
    r = rf
    C = cf
    Ex = False
    Do
        r = r + 1
        If r = rt And C = ct Then CheckPair = True: Exit Function
        If M(r, C) > 0 Or C <> ct Then Ex = True
    Loop While Not Ex

    CheckPair = False

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
    Stats.StayRows = 0
    For r = 0 To GameHeight - 1
        Stats.StayRows = Stats.StayRows + 1
        For c = 0 To GameWidth - 1
            If M(r, c) > 0 Then Stats.Stay = Stats.Stay + 1
        Next
    Next
End Sub

Function Rewrite
    _AutoDisplay
    For i = 0 To MaxStatistic - 1
        Statistic(i) = Statistic(i + 1)
    Next
    Statistic(MaxStatistic) = Stats.Stay

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
                Nm(rw) = Profile.LastRow
                Profile.LastRow = Profile.LastRow + 1
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
    Profile.Rewrites = Profile.Rewrites + 1
    StayCalc
    Do
    Loop While InKey$ <> ""
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
            For c = 0 To GameWidth - 1
                ExplodeAdd Left + c * Size, Top + (r - Scroll) * Size
                ExplodeAdd Left + c * Size, Top + (r - Scroll) * Size
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

'c - Allign: 0-left, 1-right, 2-centre
Sub DrawNum (x, y, n, c)
    d$ = Str$(n)
    d$ = Right$(d$, Len(d$) - 1)
    l = Len(d$)
    For i = 0 To l - 1
        n = Val(Mid$(d$, i + 1, 1))
        If c = 0 Then
            _PutImage (x + i * 8, y), Digits, 0, (n * 8, 0)-(n * 8 + 7, 9)
        ElseIf c = 1 Then
            _PutImage (x - Len(d$) * 8 + i * 8, y), Digits, 0, (n * 8, 0)-(n * 8 + 7, 9)
        Else
            _PutImage (x - Len(d$) * 4 + i * 8, y), Digits, 0, (n * 8, 0)-(n * 8 + 7, 9)
        End If
    Next
End Sub

'$INCLUDE: 'interface.bas'
'$INCLUDE: 'inkey.bas'
'$INCLUDE: 'misc.bas'
