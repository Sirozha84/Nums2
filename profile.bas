'Необходимые общие переменные'
'ProfileFile - имя файла
'prs - список профилей

'Загрузка списка профилей
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

'Инициализация новой игры
Sub NewGame
    'Профиль
    Profile.Start = Date$ + " " + Time$
    Profile.Last = ""
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

'Загрузка игры
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

'Сохранение игры
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
