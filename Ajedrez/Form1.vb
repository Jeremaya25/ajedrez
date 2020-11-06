Public Class Ventana
    Dim tornBlanques = True
    Dim ferEnroc = True
    Dim ferEnrocN = True
    Dim posicionsP()
    Dim caselles()
    Dim cleanCont = 0
    Dim premuda As Int32
    Private Sub Ventana_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        posicionsP = New PictureBox() {
        torre1, cavall1, alfil1, reina, rei, alfil2, cavall2, torre2,
        peo1, peo2, peo3, peo4, peo5, peo6, peo7, peo8,
        vacio, vacio, vacio, vacio, vacio, vacio, vacio, vacio,
        vacio, vacio, vacio, vacio, vacio, vacio, vacio, vacio,
        vacio, vacio, vacio, vacio, vacio, vacio, vacio, vacio,
        vacio, vacio, vacio, vacio, vacio, vacio, vacio, vacio,
        nPeo1, nPeo2, nPeo3, nPeo4, nPeo5, nPeo6, nPeo7, nPeo8,
        nTorre1, nCavall1, nAlfil1, nReina, nRei, nAlfil2, nCavall2, nTorre2}
        caselles = New PictureBox() {
        a1, b1, c1, d1, e1, f1, g1, h1,
        a2, b2, c2, d2, e2, f2, g2, h2,
        a3, b3, c3, d3, e3, f3, g3, h3,
        a4, b4, c4, d4, e4, f4, g4, h4,
        a5, b5, c5, d5, e5, f5, g5, h5,
        a6, b6, c6, d6, e6, f6, g6, h6,
        a7, b7, c7, d7, e7, f7, g7, h7,
        a8, b8, c8, d8, e8, f8, g8, h8}
    End Sub
    ' Funció moure peces si casella premuda té el backcolor vermell
    Function Moure(destino As Int32)
        If caselles(destino).BackColor = Color.LightGreen Or posicionsP(destino).BackColor = Color.Red And premuda < 64 Then
            If posicionsP(premuda).Equals(rei) Then FerEnroc = False
            If posicionsP(premuda).Equals(nRei) Then FerEnrocN = False
            posicionsP(destino).Location = vacio.Location
            posicionsP(premuda).Location = caselles(destino).Location
            posicionsP(destino) = posicionsP(premuda)
            posicionsP(premuda) = vacio
            TornSeguent()
        End If
        CleanColor(cleanCont)
        premuda = 64
    End Function
    'Torn següent + indicador
    Function TornSeguent()
        If tornBlanques Then
            tornBlanques = False
            juguen.Text = "Negres"
            juguen.ForeColor = Color.Black
            juguen.BackColor = Color.White
        Else
            tornBlanques = True
            juguen.Text = "Blanques"
            juguen.ForeColor = Color.White
            juguen.BackColor = Color.Black
        End If
    End Function
    'Funció que torna totes les caselles a transparent si el nombre establert supera 1
    Function CleanColor(cont As Int32)
        If cont > 0 Then
            For x = 0 To 63
                caselles(x).BackColor = Color.Transparent
                posicionsP(x).BackColor = Color.Transparent
            Next x
        End If
        cleanCont = 0
    End Function
    'Mira si la casella adient és buida, i si ho és la pinta de verd
    'Si hi ha una peça, pintarà de vermell i retornarà true, lo que pararà bucles recorrent el taulell
    Function PintarCasella(x As Int32)
        If x < 64 And x >= 0 Then
            If posicionsP(x).Equals(vacio) Then
                caselles(x).BackColor = Color.LightGreen
            Else
                caselles(x).BackColor = Color.LightGreen
                posicionsP(x).BackColor = Color.Red
                Return True
            End If
        End If
    End Function
    'Buscar peça al array
    Function BuscarPieza(pieza As PictureBox)
        Dim x As Int32
        For x = 0 To 63
            If posicionsP(x).Equals(pieza) Then Exit For
        Next x
        Return x
    End Function
    'Comú entre Reina i Torre
    Function MovimentXY(posicio As Int32)
        'Iluminar posicions verticals
        For y = posicio - 8 To 0 Step -8
            If PintarCasella(y) Then Exit For
        Next
        For y = posicio + 8 To 63 Step 8
            If PintarCasella(y) Then Exit For
        Next
        'Iluminar posicions horitzontals
        Dim limit As Int32
        For x = posicio To 64
            If x Mod 8 = 0 And x <> posicio Then
                limit = x
                Exit For
            End If
        Next x
        limit -= 1
        For x = posicio + 1 To limit
            If PintarCasella(x) Then Exit For
        Next x
        limit -= 7
        For x = posicio - 1 To limit Step -1
            If PintarCasella(x) Then Exit For
        Next
    End Function
    Function MovimentDiagonal(posicio As Int32)
        If posicio Mod 8 > 0 Then
            'Diagonal superior esquerra (segons les blanques)
            For x = posicio + 7 To 63 Step 7
                If PintarCasella(x) Or x Mod 8 = 0 Then Exit For
            Next

            'Diagonal inferior esquerra
            For x = posicio - 9 To 0 Step -9
                If PintarCasella(x) Or x Mod 8 = 0 Then Exit For
            Next
        End If
        If posicio Mod 8 < 7 Then
            'Diagonal superior dreta
            For x = posicio + 9 To 63 Step 9
                If PintarCasella(x) Or (x + 1) Mod 8 = 0 Then Exit For
            Next

            'Diagonal inferior dreta
            For x = posicio - 7 To 0 Step -7
                If PintarCasella(x) Or (x + 1) Mod 8 = 0 Then Exit For
            Next
        End If

    End Function

    'Enroc curt i llarg
    Function Enroc(destino As Int32)
        If caselles(destino).BackColor = Color.LightGreen Then
            'Enroc curt
            If destino = 6 Or destino = 62 Then
                'Rei
                posicionsP(premuda).Location = caselles(destino).Location
                posicionsP(destino) = posicionsP(premuda)
                posicionsP(premuda) = vacio
                'Torre
                posicionsP(destino + 1).Location = caselles(destino - 1).Location
                posicionsP(destino - 1) = posicionsP(destino + 1)
                posicionsP(destino + 1) = vacio
            End If
            'Enroc llarg
            If destino = 2 Or destino = 58 Then
                'Rei
                posicionsP(premuda).Location = caselles(destino).Location
                posicionsP(destino) = posicionsP(premuda)
                posicionsP(premuda) = vacio
                'Torre
                posicionsP(destino - 2).Location = caselles(destino + 1).Location
                posicionsP(destino + 1) = posicionsP(destino - 2)
                posicionsP(destino - 2) = vacio
            End If
        End If
        TornSeguent()
        CleanColor(cleanCont)
    End Function
    'FUNCIONS DE PECES
    Function MovimentPeo(pieza As PictureBox)
        CleanColor(cleanCont)
        Dim x = BuscarPieza(pieza)
        'Si torn blanques o negres i primer moviment del peó
        If tornBlanques Then
            caselles(x + 8).BackColor = Color.LightGreen
            If Math.Truncate(x / 8) = 1 Then caselles(x + 16).BackColor = Color.LightGreen
            'Caselles de atac del peó
            If Not posicionsP(x + 7).Equals(vacio) Then PintarCasella(x + 7)
            If Not posicionsP(x + 9).Equals(vacio) Then PintarCasella(x + 9)
        Else
            caselles(x - 8).BackColor = Color.LightGreen
            If Math.Truncate(x / 8) = 6 Then caselles(x - 16).BackColor = Color.LightGreen
            'Caselles de atac del peó
            If Not posicionsP(x - 7).Equals(vacio) Then PintarCasella(x - 7)
            If Not posicionsP(x - 9).Equals(vacio) Then PintarCasella(x - 9)
        End If
        premuda = x
        cleanCont += 1
        Return False
    End Function
    Function MovimentRei(pieza As PictureBox)
        CleanColor(cleanCont)
        premuda = BuscarPieza(pieza)
        If premuda Mod 8 > 0 Then
            For x = premuda - 9 To premuda + 7 Step 8
                PintarCasella(x)
            Next
        End If
        If premuda Mod 8 < 7 Then
            For x = premuda - 7 To premuda + 9 Step 8
                PintarCasella(x)
            Next
        End If
        'Pintar casella d'enroc (Blanques)
        If ferEnroc And pieza.Equals(rei) Then
            If posicionsP(5).Equals(vacio) And posicionsP(6).Equals(vacio) Then PintarCasella(6)
            If posicionsP(3).Equals(vacio) And posicionsP(2).Equals(vacio) And posicionsP(1).Equals(vacio) Then PintarCasella(2)
        End If
        '(Negres)
        If ferEnrocN And pieza.Equals(nRei) Then
            If posicionsP(61).Equals(vacio) And posicionsP(62).Equals(vacio) Then PintarCasella(62)
            If posicionsP(59).Equals(vacio) And posicionsP(58).Equals(vacio) And posicionsP(57).Equals(vacio) Then PintarCasella(58)
        End If
        PintarCasella(premuda + 8)
        PintarCasella(premuda - 8)
        cleanCont += 1
    End Function
    Function MovimentCavall(pieza As PictureBox)
        CleanColor(cleanCont)
        premuda = BuscarPieza(pieza)
        Dim y = premuda - 10
        Dim x = premuda + 6
        If premuda Mod 8 > 1 Then
            PintarCasella(x)
            PintarCasella(y)
        End If
        y += 4
        x += 4
        If premuda Mod 8 < 6 Then
            PintarCasella(x)
            PintarCasella(y)
        End If
        y -= 11
        x += 5
        If premuda Mod 8 > 0 Then
            PintarCasella(x)
            PintarCasella(y)
        End If
        y += 2
        x += 2
        If premuda Mod 8 < 7 Then
            PintarCasella(x)
            PintarCasella(y)
        End If
        cleanCont += 1
    End Function
    Function MovimentTorre(pieza As PictureBox)
        CleanColor(cleanCont)
        premuda = BuscarPieza(pieza)
        MovimentXY(premuda)
        cleanCont += 1
    End Function
    Function MovimentAlfil(pieza As PictureBox)
        CleanColor(cleanCont)
        premuda = BuscarPieza(pieza)
        MovimentDiagonal(premuda)
        cleanCont += 1
    End Function
    Function MovimentReina(pieza As PictureBox)
        CleanColor(cleanCont)
        premuda = BuscarPieza(pieza)
        MovimentXY(premuda)
        MovimentDiagonal(premuda)
        cleanCont += 1
    End Function
    'CLICK A PECES
    'Reines
    Private Sub reina_Click(sender As Object, e As EventArgs) Handles reina.Click
        If tornBlanques Then MovimentReina(reina) Else Moure(BuscarPieza(reina))
    End Sub
    Private Sub nReina_Click(sender As Object, e As EventArgs) Handles nReina.Click
        If Not tornBlanques Then MovimentReina(nReina) Else Moure(BuscarPieza(nReina))
    End Sub
    'Reis
    Private Sub rei_Click(sender As Object, e As EventArgs) Handles rei.Click
        If tornBlanques Then MovimentRei(rei)
    End Sub
    Private Sub nRei_Click(sender As Object, e As EventArgs) Handles nRei.Click
        If Not tornBlanques Then MovimentRei(nRei)
    End Sub
    'Cavalls
    Private Sub cavall1_Click(sender As Object, e As EventArgs) Handles cavall1.Click
        If tornBlanques Then MovimentCavall(cavall1) Else Moure(BuscarPieza(cavall1))
    End Sub
    Private Sub cavall2_Click(sender As Object, e As EventArgs) Handles cavall2.Click
        If tornBlanques Then MovimentCavall(cavall2) Else Moure(BuscarPieza(cavall2))
    End Sub
    Private Sub nCavall1_Click(sender As Object, e As EventArgs) Handles nCavall1.Click
        If Not tornBlanques Then MovimentCavall(nCavall1) Else Moure(BuscarPieza(nCavall1))
    End Sub
    Private Sub nCavall2_Click(sender As Object, e As EventArgs) Handles nCavall2.Click
        If Not tornBlanques Then MovimentCavall(nCavall2) Else Moure(BuscarPieza(nCavall2))
    End Sub
    'Torres
    Private Sub torre1_Click(sender As Object, e As EventArgs) Handles torre1.Click
        If tornBlanques Then MovimentTorre(torre1) Else Moure(BuscarPieza(torre1))
    End Sub
    Private Sub torre2_Click(sender As Object, e As EventArgs) Handles torre2.Click
        If tornBlanques Then MovimentTorre(torre2) Else Moure(BuscarPieza(torre2))
    End Sub
    Private Sub nTorre1_Click(sender As Object, e As EventArgs) Handles nTorre1.Click
        If Not tornBlanques Then MovimentTorre(nTorre1) Else Moure(BuscarPieza(nTorre1))
    End Sub
    Private Sub nTorre2_Click(sender As Object, e As EventArgs) Handles nTorre2.Click
        If Not tornBlanques Then MovimentTorre(nTorre2) Else Moure(BuscarPieza(nTorre2))
    End Sub
    'Alfils
    Private Sub alfil1_Click(sender As Object, e As EventArgs) Handles alfil1.Click
        If tornBlanques Then MovimentAlfil(alfil1) Else Moure(BuscarPieza(alfil1))
    End Sub

    Private Sub alfil2_Click(sender As Object, e As EventArgs) Handles alfil2.Click
        If tornBlanques Then MovimentAlfil(alfil2) Else Moure(BuscarPieza(alfil2))
    End Sub

    Private Sub nAlfil1_Click(sender As Object, e As EventArgs) Handles nAlfil1.Click
        If Not tornBlanques Then MovimentAlfil(nAlfil1) Else Moure(BuscarPieza(nAlfil1))
    End Sub

    Private Sub nAlfil2_Click(sender As Object, e As EventArgs) Handles nAlfil2.Click
        If Not tornBlanques Then MovimentAlfil(nAlfil2) Else Moure(BuscarPieza(nAlfil2))
    End Sub
    'Peons
    Private Sub peo1_Click(sender As Object, e As EventArgs) Handles peo1.Click
        If tornBlanques Then MovimentPeo(peo1) Else Moure(BuscarPieza(peo1))
    End Sub
    Private Sub peo2_Click(sender As Object, e As EventArgs) Handles peo2.Click
        If tornBlanques Then MovimentPeo(peo2) Else Moure(BuscarPieza(peo2))
    End Sub
    Private Sub peo3_Click(sender As Object, e As EventArgs) Handles peo3.Click
        If tornBlanques Then MovimentPeo(peo3) Else Moure(BuscarPieza(peo3))
    End Sub
    Private Sub peo4_Click(sender As Object, e As EventArgs) Handles peo4.Click
        If tornBlanques Then MovimentPeo(peo4) Else Moure(BuscarPieza(peo4))
    End Sub
    Private Sub peo5_Click(sender As Object, e As EventArgs) Handles peo5.Click
        If tornBlanques Then MovimentPeo(peo5) Else Moure(BuscarPieza(peo5))
    End Sub
    Private Sub peo6_Click(sender As Object, e As EventArgs) Handles peo6.Click
        If tornBlanques Then MovimentPeo(peo6) Else Moure(BuscarPieza(peo6))
    End Sub
    Private Sub peo7_Click(sender As Object, e As EventArgs) Handles peo7.Click
        If tornBlanques Then MovimentPeo(peo7) Else Moure(BuscarPieza(peo7))
    End Sub
    Private Sub peo8_Click(sender As Object, e As EventArgs) Handles peo8.Click
        If tornBlanques Then MovimentPeo(peo8) Else Moure(BuscarPieza(peo8))
    End Sub
    Private Sub nPeo1_Click(sender As Object, e As EventArgs) Handles nPeo1.Click
        If tornBlanques = False Then MovimentPeo(nPeo1) Else Moure(BuscarPieza(nPeo1))
    End Sub
    Private Sub nPeo2_Click(sender As Object, e As EventArgs) Handles nPeo2.Click
        If tornBlanques = False Then MovimentPeo(nPeo2) Else Moure(BuscarPieza(nPeo2))
    End Sub
    Private Sub nPeo3_Click(sender As Object, e As EventArgs) Handles nPeo3.Click
        If tornBlanques = False Then MovimentPeo(nPeo3) Else Moure(BuscarPieza(nPeo3))
    End Sub
    Private Sub nPeo4_Click(sender As Object, e As EventArgs) Handles nPeo4.Click
        If tornBlanques = False Then MovimentPeo(nPeo4) Else Moure(BuscarPieza(nPeo4))
    End Sub
    Private Sub nPeo5_Click(sender As Object, e As EventArgs) Handles nPeo5.Click
        If tornBlanques = False Then MovimentPeo(nPeo5) Else Moure(BuscarPieza(nPeo5))
    End Sub
    Private Sub nPeo6_Click(sender As Object, e As EventArgs) Handles nPeo6.Click
        If tornBlanques = False Then MovimentPeo(nPeo6) Else Moure(BuscarPieza(nPeo6))
    End Sub
    Private Sub nPeo7_Click(sender As Object, e As EventArgs) Handles nPeo7.Click
        If tornBlanques = False Then MovimentPeo(nPeo7) Else Moure(BuscarPieza(nPeo7))
    End Sub
    Private Sub nPeo8_Click(sender As Object, e As EventArgs) Handles nPeo8.Click
        If tornBlanques = False Then MovimentPeo(nPeo8) Else Moure(BuscarPieza(nPeo8))
    End Sub

    'CLICK A CASELLES
    Private Sub a1_Click(sender As Object, e As EventArgs) Handles a1.Click
        Moure(0)
    End Sub
    Private Sub b1_Click(sender As Object, e As EventArgs) Handles b1.Click
        Moure(1)
    End Sub

    'Enroc llarg
    Private Sub c1_Click(sender As Object, e As EventArgs) Handles c1.Click
        If premuda = 4 And posicionsP(0).Equals(torre1) And ferEnroc = True Then
            Enroc(2)
            ferEnroc = False
        Else
            Moure(2)
        End If
    End Sub
    Private Sub d1_Click(sender As Object, e As EventArgs) Handles d1.Click
        Moure(3)
    End Sub
    Private Sub e1_Click(sender As Object, e As EventArgs) Handles e1.Click
        Moure(4)
    End Sub
    Private Sub f1_Click(sender As Object, e As EventArgs) Handles f1.Click
        Moure(5)
    End Sub

    'Enroc Curt!
    Private Sub g1_Click(sender As Object, e As EventArgs) Handles g1.Click
        If premuda = 4 And posicionsP(7).Equals(torre2) And FerEnroc = True Then
            Enroc(6)
            ferEnroc = False
        Else
            Moure(6)
        End If
    End Sub
    Private Sub h1_Click(sender As Object, e As EventArgs) Handles h1.Click
        Moure(7)
    End Sub
    Private Sub a2_Click(sender As Object, e As EventArgs) Handles a2.Click
        Moure(8)
    End Sub
    Private Sub b2_Click(sender As Object, e As EventArgs) Handles b2.Click
        Moure(9)
    End Sub
    Private Sub c2_Click(sender As Object, e As EventArgs) Handles c2.Click
        Moure(10)
    End Sub
    Private Sub d2_Click(sender As Object, e As EventArgs) Handles d2.Click
        Moure(11)
    End Sub
    Private Sub e2_Click(sender As Object, e As EventArgs) Handles e2.Click
        Moure(12)
    End Sub
    Private Sub f2_Click(sender As Object, e As EventArgs) Handles f2.Click
        Moure(13)
    End Sub
    Private Sub g2_Click(sender As Object, e As EventArgs) Handles g2.Click
        Moure(14)
    End Sub
    Private Sub h2_Click(sender As Object, e As EventArgs) Handles h2.Click
        Moure(15)
    End Sub
    Private Sub a3_Click(sender As Object, e As EventArgs) Handles a3.Click
        Moure(16)
    End Sub
    Private Sub b3_Click(sender As Object, e As EventArgs) Handles b3.Click
        Moure(17)
    End Sub
    Private Sub c3_Click(sender As Object, e As EventArgs) Handles c3.Click
        Moure(18)
    End Sub
    Private Sub d3_Click(sender As Object, e As EventArgs) Handles d3.Click
        Moure(19)
    End Sub
    Private Sub e3_Click(sender As Object, e As EventArgs) Handles e3.Click
        Moure(20)
    End Sub
    Private Sub f3_Click(sender As Object, e As EventArgs) Handles f3.Click
        Moure(21)
    End Sub
    Private Sub g3_Click(sender As Object, e As EventArgs) Handles g3.Click
        Moure(22)
    End Sub
    Private Sub h3_Click(sender As Object, e As EventArgs) Handles h3.Click
        Moure(23)
    End Sub
    Private Sub a4_Click(sender As Object, e As EventArgs) Handles a4.Click
        Moure(24)
    End Sub
    Private Sub b4_Click(sender As Object, e As EventArgs) Handles b4.Click
        Moure(25)
    End Sub
    Private Sub c4_Click(sender As Object, e As EventArgs) Handles c4.Click
        Moure(26)
    End Sub
    Private Sub d4_Click(sender As Object, e As EventArgs) Handles d4.Click
        Moure(27)
    End Sub
    Private Sub e4_Click(sender As Object, e As EventArgs) Handles e4.Click
        Moure(28)
    End Sub
    Private Sub f4_Click(sender As Object, e As EventArgs) Handles f4.Click
        Moure(29)
    End Sub
    Private Sub g4_Click(sender As Object, e As EventArgs) Handles g4.Click
        Moure(30)
    End Sub
    Private Sub h4_Click(sender As Object, e As EventArgs) Handles h4.Click
        Moure(31)
    End Sub
    Private Sub a5_Click(sender As Object, e As EventArgs) Handles a5.Click
        Moure(32)
    End Sub
    Private Sub b5_Click(sender As Object, e As EventArgs) Handles b5.Click
        Moure(33)
    End Sub
    Private Sub c5_Click(sender As Object, e As EventArgs) Handles c5.Click
        Moure(34)
    End Sub
    Private Sub d5_Click(sender As Object, e As EventArgs) Handles d5.Click
        Moure(35)
    End Sub
    Private Sub e5_Click(sender As Object, e As EventArgs) Handles e5.Click
        Moure(36)
    End Sub
    Private Sub f5_Click(sender As Object, e As EventArgs) Handles f5.Click
        Moure(37)
    End Sub
    Private Sub g5_Click(sender As Object, e As EventArgs) Handles g5.Click
        Moure(38)
    End Sub
    Private Sub h5_Click(sender As Object, e As EventArgs) Handles h5.Click
        Moure(39)
    End Sub
    Private Sub a6_Click(sender As Object, e As EventArgs) Handles a6.Click
        Moure(40)
    End Sub
    Private Sub b6_Click(sender As Object, e As EventArgs) Handles b6.Click
        Moure(41)
    End Sub
    Private Sub c6_Click(sender As Object, e As EventArgs) Handles c6.Click
        Moure(42)
    End Sub
    Private Sub d6_Click(sender As Object, e As EventArgs) Handles d6.Click
        Moure(43)
    End Sub
    Private Sub e6_Click(sender As Object, e As EventArgs) Handles e6.Click
        Moure(44)
    End Sub
    Private Sub f6_Click(sender As Object, e As EventArgs) Handles f6.Click
        Moure(45)
    End Sub
    Private Sub g6_Click(sender As Object, e As EventArgs) Handles g6.Click
        Moure(46)
    End Sub
    Private Sub h6_Click(sender As Object, e As EventArgs) Handles h6.Click
        Moure(47)
    End Sub
    Private Sub a7_Click(sender As Object, e As EventArgs) Handles a7.Click
        Moure(48)
    End Sub
    Private Sub b7_Click(sender As Object, e As EventArgs) Handles b7.Click
        Moure(49)
    End Sub
    Private Sub c7_Click(sender As Object, e As EventArgs) Handles c7.Click
        Moure(50)
    End Sub
    Private Sub d7_Click(sender As Object, e As EventArgs) Handles d7.Click
        Moure(51)
    End Sub
    Private Sub e7_Click(sender As Object, e As EventArgs) Handles e7.Click
        Moure(52)
    End Sub
    Private Sub f7_Click(sender As Object, e As EventArgs) Handles f7.Click
        Moure(53)
    End Sub
    Private Sub g7_Click(sender As Object, e As EventArgs) Handles g7.Click
        Moure(54)
    End Sub
    Private Sub h7_Click(sender As Object, e As EventArgs) Handles h7.Click
        Moure(55)
    End Sub
    Private Sub a8_Click(sender As Object, e As EventArgs) Handles a8.Click
        Moure(56)
    End Sub
    Private Sub b8_Click(sender As Object, e As EventArgs) Handles b8.Click
        Moure(57)
    End Sub
    Private Sub c8_Click(sender As Object, e As EventArgs) Handles c8.Click
        If premuda = 60 And posicionsP(56).Equals(nTorre1) And ferEnrocN = True Then
            Enroc(58)
            ferEnrocN = False
        Else
            Moure(58)
        End If
    End Sub
    Private Sub d8_Click(sender As Object, e As EventArgs) Handles d8.Click
        Moure(59)
    End Sub
    Private Sub e8_Click(sender As Object, e As EventArgs) Handles e8.Click
        Moure(60)
    End Sub
    Private Sub f8_Click(sender As Object, e As EventArgs) Handles f8.Click
        Moure(61)
    End Sub
    Private Sub g8_Click(sender As Object, e As EventArgs) Handles g8.Click
        If premuda = 60 And posicionsP(63).Equals(nTorre2) And ferEnrocN = True Then
            Enroc(62)
            ferEnrocN = False
        Else
            Moure(62)
        End If
    End Sub
    Private Sub h8_Click(sender As Object, e As EventArgs) Handles h8.Click
        Moure(63)
    End Sub
End Class