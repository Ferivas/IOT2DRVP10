'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
'*  SD_Archivos.bas                                                        *
'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
'*                                                                             *
'*  Variables, Subrutinas y Funciones                                          *
'* WATCHING SOLUCIONES TECNOLOGICAS                                            *
'* 25.06.2015                                                                  *
'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

$nocompile


'*******************************************************************************
'Declaracion de subrutinas
'*******************************************************************************
Declare Sub Inivar()
Declare Sub Procser()
Declare Sub Gendig(byval Valor As Byte , Byval Pos As Byte)
Declare Sub Genpunto(byval Pos As Byte)
Declare Sub Dat2buf(byval Indcol As Byte , Valcol As Word)
Declare Sub Verackesp32()


'*******************************************************************************
'Declaracion de variables
'*******************************************************************************
'Generales
Dim Tmpb As Byte
Dim Tmpb2 As Byte
'Dim Tmpb3 As Byte
Dim Tmpl2 As Long
Dim Tmpcrc32 As Long
Dim Tmpstr As String * 2

Dim Tmpw As Word
'Dim Tmpw2 As Word
Dim Tmpw4 As Word


Dim Newseg As Bit
Dim Cntrseg As Byte

'
'Temperatura
'Matriz
Dim Dato8 As Byte
Dim Buffram(longbuf) As Byte
'Dim Sample As Byte

'TIMER0
Dim Cntr_col As Byte
Dim Kk As Byte
Dim Tmpwtx As Word
Dim T0c As Byte

Dim Cmdtmp As String * 6
Dim Atsnd As String * 255
Dim Cmderr As Byte
Dim Tmpstr52 As String * 52

Dim Cntresp32rst As Word
Dim Cntresp32rsteep As Eram Word
Dim Cntrackesp32 As Word

Dim Preciotab As String * 10
Dim Preciotabeep As Eram String * 10


'Variables SERIAL0
Dim Ser_ini As Bit , Sernew As Bit
Dim Numpar As Byte
Dim Crcsplit(3) As String * 20
Dim Crcbuf(20) As Byte At Crcsplit(1) Overlay
Dim Cmdsplit(8) As String * 20
Dim Serdata As String * 255 , Serrx As Byte , Serproc As String * 255

'Variables SERIAL1



'*******************************************************************************
'* END public part                                                             *
'*******************************************************************************


Goto Loaded_arch

'*******************************************************************************
' INTERRUPCIONES
'*******************************************************************************

'*******************************************************************************
' Subrutina interrupcion de puerto serial 1
'*******************************************************************************
At_ser1:
   Serrx = Udr0

   Select Case Serrx
      Case "$":
         Ser_ini = 1
         Serdata = ""
         Disable Timer0

      Case 13:
         If Ser_ini = 1 Then
            Ser_ini = 0
            Serdata = Serdata + Chr(0)
            Serproc = Serdata
            Sernew = 1
            Enable Timer0
         End If

      Case Is > 31
         If Ser_ini = 1 Then
            Serdata = Serdata + Chr(serrx)
         End If

   End Select

Return

'*******************************************************************************



'*******************************************************************************
' TIMER0
'*******************************************************************************
Tim0_isr:
   Timer0 = &HD3                                            '240hz  CON 11059200
   'Reset Oena2
   Select Case Cntr_col
      Case 0:
         Reset Sela
         Reset Selb
      Case 1:
         Set Sela
         Reset Selb
      Case 2:
         Reset Sela
         Set Selb
      Case 3:
         Set Sela
         Set Selb
   End Select
   Cntr_col = Cntr_col + 1

'      Set Pinbug

   Reset Oena
   Reset Oena2
   For Kk = 1 To Numtxser_2
      Tmpwtx = Lookup(kk , Tbl_txser)
      Tmpwtx = Tmpwtx + Cntr_col
      Dato8 = Buffram(tmpwtx)
      'Dato8 = Dato8 Xor &HFF
      Shiftout Datos , Sck , Dato8 , 1
   Next
   Set Lena
   Reset Lena
   'Set Oena

   'Reset Oena2
   For Kk = 1 To Numtxser_2
      Tmpwtx = Lookup(kk , Tbl_tx2)
      Tmpwtx = Tmpwtx + Cntr_col
      Dato8 = Buffram(tmpwtx)
      'Dato8 = Dato8 Xor &HFF
      Shiftout Datos , Sck , Dato8 , 1
   Next
   Set Lena2
   Reset Lena2
   Set Oena
   Set Oena2
'   Reset Pinbug
   Cntr_col = Cntr_col Mod 4

   Incr T0c
   T0c = T0c Mod 240
   If T0c = 0 Then
      Toggle Ledout
      Set Newseg
   End If

Return

'*******************************************************************************
'TIMER 2



'*******************************************************************************
' SUBRUTINAS
'*******************************************************************************
Sub Inivar()
   Print #1 , "************ DRIVER matriz LEDs P10(1R)-V701C ************"
   Print #1 , Version(1)
   Print #1 , Version(3)
   Print #1 , "Nummatriz=" ; Nummatriz
   Print #1 , "Longbuf=" ; Longbuf
   Print #1 , "Longdat=" ; Longdat
   Print #1 , "Longdat_mas_uno=" ; Longdat_mas_uno
   Print #1 , "Longbuf_mas_uno=" ; Longbuf_mas_uno
   Print #1 , "Numtxser=" ; Numtxser

   For Tmpw4 = 1 To Longbuf
      Buffram(tmpw4) = &HFF
   Next

   Preciotab = Preciotabeep
   Print #1 , "PRECIO=" ; Preciotab

   'Estado_led = 2
   Tmpstr52 = Preciotab
   Tmpstr = Mid(tmpstr52 , 1 , 1)
   Tmpb = Val(tmpstr)
   'Print #1 , Tmpb
   Call Gendig(tmpb , 78)
   Tmpstr = Mid(tmpstr52 , 3 , 1)
   Tmpb = Val(tmpstr)
   'Print #1 , Tmpb
   Call Gendig(tmpb , 48)
   Tmpstr = Mid(tmpstr52 , 4 , 1)
   Tmpb = Val(tmpstr)
   'Print #1 , Tmpb
   Call Gendig(tmpb , 24)
   Tmpstr = Mid(tmpstr52 , 5 , 1)
   Tmpb = Val(tmpstr)
   'Print #1 , Tmpb
   Call Gendig(tmpb , 2)
   Call Genpunto(69)
End Sub


Sub Dat2buf(byval Indcol As Byte , Valcol As Word )
   Local Numbuf As Byte
   Local Ptrbuf As Word
   Local Kd As Byte , Ks As Word

   Valcol = Valcol Xor &HFFFF
   Indcol = Indcol - 1
   Numbuf = Indcol Mod 8
   Ptrbuf = Indcol                                          ' - 1
   Ptrbuf = Ptrbuf \ 8
   Ptrbuf = Ptrbuf * 16
   Incr Ptrbuf
   For Kd = 0 To 15
      Ks = Ptrbuf + Kd
      Buffram(ks).numbuf = Valcol.kd
      'Print #1 , "KS=" ; Ks ; ", numbuf=" ; Numbuf
   Next
End Sub


'*******************************************************************************
Sub Verackesp32()
   Incr Cntrackesp32
   Cntrackesp32 = Cntrackesp32 Mod 600
   'Tmpw = Cntrackesp32 Mod 10
   'If Tmpw = 0 Then
   '   Print #1 , "CNTRACK=" ; Cntrackesp32
   'End If
   If Cntrackesp32 = 0 Then
      Print #1 , "RST esp32"
      Incr Cntresp32rst
      'Cntresp32rsteep = Cntresp32rst
      Print #1 , "CNTRrst=" ; Cntresp32rst
      'Print #1 , "********** DEBUG *****************************"
      Reset Esp32rst
      Print #1 , "RSTESP=0"
      Wait 1
      Set Esp32rst
      Print #1 , "RSTESP=1"
   End If
End Sub


'*******************************************************************************
'  Subrutina para pintar digito en posicion predeterminada
'  Los digitos son de 32x18 pixels y se puden posicionar en cualquier columna
'  Valdig indica el valor del digito (0-11) y posdig la posicion
'  10 es espacio vaio y 11 el punto

'
Sub Gendig(byval Valor As Byte , Byval Pos As Byte)
   Local Kg As Byte
   Local Ptrtbl As Byte
   Local Ptrini As Byte
   Local Ptrdat As Byte
   Ptrini = Valor * 18
   Ptrdat = Pos - 1
   For Kg = 1 To 18
      Ptrdat = Kg + Pos
      Ptrdat = Ptrdat - 1
      Ptrtbl = Kg + Ptrini
      Tmpw = Lookup(ptrtbl , Tbl_digbig1)
      Call Dat2buf(ptrdat , Tmpw )
      'Print #1 , Ptrdat ; "," ; Bin(tmpw) ; "UP"
      Tmpb = Kg + Ptrini
      Tmpw = Lookup(ptrtbl , Tbl_digbig2)
      Ptrdat = Ptrdat + 96
      Call Dat2buf(ptrdat , Tmpw )
      'Print #1 , Ptrdat ; "," ; Bin(tmpw) ; "DN"
      'Print #1,
   Next
End Sub

Sub Genpunto(byval Pos As Byte)
   Local Kg As Byte
   Local Ptrtbl As Byte
   Local Ptrini As Byte
   Local Ptrdat As Byte
   Ptrini = 0
   Ptrdat = Pos - 1
   For Kg = 1 To 6
      Ptrdat = Kg + Pos
      Ptrdat = Ptrdat - 1
      Ptrtbl = Kg + Ptrini
      Tmpw = Lookup(ptrtbl , Tbl_punto1)
      Call Dat2buf(ptrdat , Tmpw )
      'Print #1 , Ptrdat ; "," ; Bin(tmpw) ; "UP"
      Tmpb = Kg + Ptrini
      Tmpw = Lookup(ptrtbl , Tbl_punto2)
      Ptrdat = Ptrdat + 96
      Call Dat2buf(ptrdat , Tmpw )
      'Print #1 , Ptrdat ; "," ; Bin(tmpw) ; "DN"
      'Print #1,
   Next
End Sub


'*******************************************************************************
'Subrutina de procesamiento de datos seriales
'*******************************************************************************
Sub Procser()
   Print #1 , "S>" ; Serproc
   Numpar = Split(serproc , Cmdsplit(1) , ",")
'   If Numpar > 0 Then
'      For Tmpb = 1 To Numpar
'         Print #1 , Tmpb ; ":" ; Cmdsplit(tmpb)
'      Next
'   End If

   If Len(cmdsplit(1)) = 6 Then
      Cmdtmp = Cmdsplit(1)
      Cmdtmp = Ucase(cmdtmp)
      Cmderr = 255
      Select Case Cmdtmp
         Case "LEEVFW"
            Atsnd = "Version FW: Fecha <"
            Tmpstr52 = Version(1)
            Atsnd = Atsnd + Tmpstr52 + ">, Archivo <"
            Tmpstr52 = Version(3)
            Atsnd = Atsnd + Tmpstr52 + ">"
            Cmderr = 0

            Case "SETBUF"
               Cmderr = 0
               Tmpw = Val(cmdsplit(2))
               If Tmpw < Longbuf_mas_uno Then
                  Cmderr = 0
                  Tmpb = Hexval(cmdsplit(3))
                  Tmpb = Tmpb Xor 255
                  Buffram(tmpw) = Tmpb
                  Atsnd = "Buffram(" + Str(tmpw) + ") = &H " + Hex(tmpb)
               Else
                  Cmderr = 5
               End If

            Case "SETCER"
               Cmderr = 0
               Atsnd = "Encerar buffer"
               For Tmpw4 = 1 To Longbuf
                  'Incr Tmpw3
                  'Call Disp_buffer(tmpw4 , Tmpw3)
                  Buffram(tmpw4) = &HFF
               Next

            Case "SETDIG"
               If Numpar = 3 Then
                  Cmderr = 0
                  Tmpb = Val(cmdsplit(2))
                  Tmpb2 = Val(cmdsplit(3))
                  Atsnd = "Test Digito " + Str(tmpb)
                  Call Gendig(tmpb , Tmpb2)
               Else
                  Cmderr = 5
               End If

            Case "SETDAT"
               If Numpar = 3 Then
                  Tmpb = Val(cmdsplit(2))
                  Tmpw = 0
                  If Tmpb < Longdat_mas_uno Then
                        Tmpw = Hexval(cmdsplit(3))
                        Cmderr = 0
                        Atsnd = "Se configura Bufdata(" + Str(tmpb) + ")=" + Hex(tmpw)

                        Call Dat2buf(tmpb , Tmpw )
                  Else
                     Cmderr = 5
                  End If
               Else
                  Cmderr = 4
               End If

            Case "SETDIR"
               Cmderr = 0
               Atsnd = "Genera dir"
               Tmpw = 0
               For Tmpb = 1 To Longdat
                  Incr Tmpw
                  Call Dat2buf(tmpb , Tmpw)
               Next

            Case "SETPUN"
               If Numpar = 2 Then
                  Tmpb = Val(cmdsplit(2))
                  If Tmpb < 97 Then
                     Cmderr = 0
                     Call Genpunto(tmpb)
                  Else
                     Cmderr = 5
                  End If
               Else
                  Cmderr = 4
               End If

            Case "SETTAB"
               If Numpar = 2 Then
                  Tmpb2 = Split(serproc , Crcsplit(1) , "&")
                  If Tmpb2 = 2 Then
                     'Atsnd = Crcsplit(1)
                     Print #1 , Crcsplit(1)
                     Print #1 , Crcsplit(2)
                     Tmpw = Len(crcsplit(1))
                     Tmpcrc32 = Crc32(crcbuf(1) , Tmpw)
                     Print #1 , Tmpcrc32 ; "," ; Hex(tmpcrc32)
                     Tmpl2 = Hexval(crcsplit(2))
                     Print #1 , Tmpl2 ; "," ; Hex(tmpl2)
                     If Tmpl2 = Tmpcrc32 Then
                        If Len(cmdsplit(2)) = 14 Then
                           Tmpstr52 = Cmdsplit(2)
                           Tmpstr = Mid(tmpstr52 , 1 , 1)
                           Tmpb = Val(tmpstr)
                           'Print #1 , Tmpb
                           Call Gendig(tmpb , 78)
                           Tmpstr = Mid(tmpstr52 , 3 , 1)
                           Tmpb = Val(tmpstr)
                           'Print #1 , Tmpb
                           Call Gendig(tmpb , 48)
                           Tmpstr = Mid(tmpstr52 , 4 , 1)
                           Tmpb = Val(tmpstr)
                           'Print #1 , Tmpb
                           Call Gendig(tmpb , 24)
                           Tmpstr = Mid(tmpstr52 , 5 , 1)
                           Tmpb = Val(tmpstr)
                           'Print #1 , Tmpb
                           Call Gendig(tmpb , 2)
                           Call Genpunto(69)
                           Atsnd = "$OK," + Cmdsplit(2)
                        Else
                           Cmderr = 5
                        End If
                     Else
                        Print #1 , "$ERR"
                     End If
                  End If
               Else
                  Cmderr = 4
               End If


            Case "SETPRE"
               If Numpar = 2 Then
                  If Len(cmdsplit(2)) = 5 Then
                     Cmderr = 0
                     Tmpstr52 = Cmdsplit(2)
                     Tmpstr = Mid(tmpstr52 , 1 , 1)
                     Tmpb = Val(tmpstr)
                     Print #1 , Tmpb
                     Call Gendig(tmpb , 78)
                     Tmpstr = Mid(tmpstr52 , 3 , 1)
                     Tmpb = Val(tmpstr)
                     Print #1 , Tmpb
                     Call Gendig(tmpb , 48)
                     Tmpstr = Mid(tmpstr52 , 4 , 1)
                     Tmpb = Val(tmpstr)
                     Print #1 , Tmpb
                     Call Gendig(tmpb , 24)
                     Tmpstr = Mid(tmpstr52 , 5 , 1)
                     Tmpb = Val(tmpstr)
                     Print #1 , Tmpb
                     Call Gendig(tmpb , 2)
                     Call Genpunto(69)
                     Atsnd = "$OK," + Cmdsplit(2)
                     Preciotab = Cmdsplit(2)
                     Preciotabeep = Preciotab
                  Else
                     Cmderr = 5
                  End If
               Else
                  Cmderr = 4
               End If

            Case "SETESP"
               If Numpar = 2 Then
                  Tmpb = Val(cmdsplit(2))
                  If Tmpb < 2 Then
                     Cmderr = 0
                     If Tmpb = 0 Then
                        Reset Esp32rst
                     Else
                        Set Esp32rst
                     End If
                     Atsnd = "RSTESP32=" + Str(esp32rst)
                  Else
                     Cmderr = 3
                  End If
               Else
                  Cmderr = 4
               End If

            Case "LEEESP"
               Cmderr = 0
               Atsnd = "Cntrackesp32=" + Str(cntrackesp32)

         Case "LEEKTB"
            If Numpar = 1 Then
               Cmderr = 0
               Atsnd = "LEE Keys TB"
               Tmpstr52 = "%LEEKTB,1,2,3"
               Wait 1
               Print #1 , Tmpstr52
            Else
               Cmderr = 4
            End If


         Case "SETKTB"
            If Numpar = 3 Then
               Cmderr = 0
               Atsnd = "Envio KTB a ESP32"
               Tmpstr52 = "%SETKTB," + Cmdsplit(2) + "," + Cmdsplit(3) + ",1"  '%setktb;IDtb;Apiktb
               Print #1 , Tmpstr52
            Else
               Cmderr = 4
            End If

         Case "ACKACK"
            Cmderr = 0
            Cntrackesp32 = 0
            Atsnd = "ESP OK"

         Case Else
            Cmderr = 1


      End Select

   Else
        Cmderr = 2
   End If

   If Cmderr > 0 Then
      Atsnd = Lookupstr(cmderr , Tbl_err)
   End If
   Print #1 , Atsnd

End Sub

'*******************************************************************************
'TABLA DE DATOS
'*******************************************************************************
Tbl_ind:
Data 0                                                      '
Data 4                                                      '1
Data 8                                                      '2
Data 12                                                     '3
Data 16                                                     '4
Data 20                                                     '5
Data 24                                                     '6
Data 28                                                     '7
Data 32
Data 36
Data 40
Data 44
Data 48
Data 52                                                     '13
Data 56                                                     '14
Data 60                                                     '15
Data 64                                                     '16


'*******************************************************************************
'TABLA DE DATOS
'*******************************************************************************
Tabla_estado:
Data &B00000000000000000000000000000000&                    'Estado 0
Data &B00000000000000000000000000000001&                    'Estado 1
Data &B00000000000000000000000000000101&                    'Estado 2
Data &B00000000000000000000000000010101&                    'Estado 3
Data &B00000000000000000000000001010101&                    'Estado 4
Data &B00000000000000000000000101010101&                    'Estado 5
Data &B00000000000000000000010101010101&                    'Estado 6
Data &B00000000000000000001010101010101&                    'Estado 7
Data &B00000000000000000101010101010101&                    'Estado 8
Data &B00000000000000010101010101010101&                    'Estado 9
Data &B00000000000001110111011101110111&                    'Estado 10

Tbl_err:
Data "OK"                                                   '0
Data "Comando no reconocido"                                '1
Data "Longitud comando no valida"                           '2
Data "Numero de usuario no valido"                          '3
Data "Numero de parametros invalido"                        '4
Data "Error longitud parametro 1"                           '5
Data "Error longitud parametro 2"                           '6
Data "Parametro no valido"                                  '7
Data "ERROR8"                                               '8
Data "ERROR SD. Intente de nuevo"                           '9



Tbl_txser:
Data 0%                                                     ' Dummy Data
Data 188%
Data 184%
Data 180%
Data 176%
Data 172%
Data 168%
Data 164%
Data 160%
Data 156%
Data 152%
Data 148%
Data 144%
Data 140%
Data 136%
Data 132%
Data 128%
Data 124%
Data 120%
Data 116%
Data 112%
Data 108%
Data 104%
Data 100%
Data 96%
Data 92%
Data 88%
Data 84%
Data 80%
Data 76%
Data 72%
Data 68%
Data 64%
Data 60%
Data 56%
Data 52%
Data 48%
Data 44%
Data 40%
Data 36%
Data 32%
Data 28%
Data 24%
Data 20%
Data 16%
Data 12%
Data 8%
Data 4%
Data 0%


Tbl_tx2:
Data 0%
Data 380%
Data 376%
Data 372%
Data 368%
Data 364%
Data 360%
Data 356%
Data 352%
Data 348%
Data 344%
Data 340%
Data 336%
Data 332%
Data 328%
Data 324%
Data 320%
Data 316%
Data 312%
Data 308%
Data 304%
Data 300%
Data 296%
Data 292%
Data 288%
Data 284%
Data 280%
Data 276%
Data 272%
Data 268%
Data 264%
Data 260%
Data 256%
Data 252%
Data 248%
Data 244%
Data 240%
Data 236%
Data 232%
Data 228%
Data 224%
Data 220%
Data 216%
Data 212%
Data 208%
Data 204%
Data 200%
Data 196%
Data 192%


Tbl_digbig2:
Data &B0000000000000000%                                    '0
Data &B1111111111111000%
Data &B1111111111111100%
Data &B1111111111111110%
Data &B0000000000001110%
Data &B0000000000001110%
Data &B0000000000001110%
Data &B0000000000001110%
Data &B0000000000001110%
Data &B0000000000001110%
Data &B0000000000001110%
Data &B0000000000001110%
Data &B0000000000001110%
Data &B0000000000001110%
Data &B0000000000001110%
Data &B0000000000001110%
Data &B1111111111111110%
Data &B1111111111111100%
Data &B1111111111111000%


Data        &B0000000000000000%
Data        &B0000000000000000%
Data        &B0000000000000000%
Data        &B0000000000000000%
Data        &B0000000000000000%
Data        &B0000000000000000%
Data        &B1111111111111110%
Data        &B1111111111111110%
Data        &B1111111111111110%
Data        &B0000000000011100%
Data        &B0000000000011000%
Data        &B0000000000010000%
Data        &B0000000000000000%
Data        &B0000000000000000%
Data        &B0000000000000000%
Data        &B0000000000000000%
Data        &B0000000000000000%
Data        &B0000000000000000%


Data &B0111111111111000%                                    '2
Data &B1111111111111100%
Data &B1111111111111110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B0000000000001110%
Data &B0000000000001110%
Data &B0000000000001110%


Data &B0111111111111000%                                    '3
Data &B1111111111111100%
Data &B1111111111111110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B0000000000001110%
Data &B0000000000001110%
Data &B0000000000001110%
Data &B0000000000001110%
Data &B0000000000001110%


Data &B1111111111111110%                                    '4
Data &B1111111111111110%
Data &B1111111111111110%
Data &B1000000000000000%
Data &B1000000000000000%
Data &B1000000000000000%
Data &B1000000000000000%
Data &B1000000000000000%
Data &B1000000000000000%
Data &B1000000000000000%
Data &B1000000000000000%
Data &B1000000000000000%
Data &B1000000000000000%
Data &B1000000000000000%
Data &B1000000000000000%
Data &B1111111111111100%
Data &B1111111111111100%
Data &B1111111111111100%


Data &B0000000000000000%                                    '5
Data &B0000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1111111111111110%
Data &B1111111111111110%
Data &B1111111111111110%


Data &B0000000000000000%                                    '6
Data &B0000000000000000%
Data &B1000000000000000%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1111111111111110%
Data &B1111111111111100%
Data &B1111111111111000%


Data &B1111111111111000%                                    '7
Data &B1111111111111100%
Data &B1111111111111110%
Data &B0000000000001110%
Data &B0000000000001110%
Data &B0000000000001110%
Data &B0000000000001110%
Data &B0000000000001110%
Data &B0000000000001110%
Data &B0000000000001110%
Data &B0000000000001110%
Data &B0000000000001110%
Data &B0000000000001110%
Data &B0000000000001110%
Data &B0000000000001110%
Data &B0000000000001110%
Data &B0000000000001110%
Data &B0000000000000000%


Data &B0111111111111000%                                    '8
Data &B1111111111111100%
Data &B1111111111111110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1111111111111110%
Data &B1111111111111100%
Data &B0111111111111000%


Data &B1111111111111000%                                    '9
Data &B1111111111111100%
Data &B1111111111111110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1000000000001110%
Data &B1100000000001110%
Data &B1111111111111110%
Data &B1111111111111100%
Data &B1111111111111000%


Data &B0000000000000000%                                    '10
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%

Tbl_punto2:
Data &B0000000000000000%
Data &B0000000000000000%                                    '11
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%


Tbl_digbig1:
Data &B0000000000000000%
Data &B0001111111111111%
Data &B0011111111111111%
Data &B0111111111111111%
Data &B0111000000000000%
Data &B0111000000000000%
Data &B0111000000000000%
Data &B0111000000000000%
Data &B0111000000000000%
Data &B0111000000000000%
Data &B0111000000000000%
Data &B0111000000000000%
Data &B0111000000000000%
Data &B0111111111111100%
Data &B0111111111111100%
Data &B0111000000000000%
Data &B0111111111111111%
Data &B0011111111111111%
Data &B0001111111111111%

Data        &B0000000000000000%
Data        &B0000000000000000%
Data        &B0000000000000000%
Data        &B0000000000000000%
Data        &B0000000000000000%
Data        &B0000000000000000%
Data        &B0111111111111111%
Data        &B0111111111111111%
Data        &B0111111111111111%
Data        &B0000000000000000%
Data        &B0000000000000000%
Data        &B0000000000000000%
Data        &B0000000000000000%
Data        &B0000000000000000%
Data        &B0000000000000000%
Data        &B0000000000000000%
Data        &B0000000000000000%
Data        &B0000000000000000%



Data &B0111000000000000%
Data &B0111000000000000%
Data &B0111000000000001%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111111111111111%
Data &B0111111111111110%
Data &B0111111111111100%


Data &B0001111111111000%
Data &B0011111111111100%
Data &B0111111111111111%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000000%
Data &B0111000000000000%
Data &B0111000000000000%
Data &B0111000000000000%
Data &B0111000000000000%


Data &B0111111111111111%
Data &B0111111111111111%
Data &B0111111111111111%
Data &B0000000000000011%
Data &B0000000000000011%
Data &B0000000000000011%
Data &B0000000000000011%
Data &B0000000000000011%
Data &B0000000000000011%
Data &B0000000000000011%
Data &B0000000000000011%
Data &B0000000000000011%
Data &B0000000000000011%
Data &B0000000000000011%
Data &B0000000000000011%
Data &B0000000000000011%
Data &B0000000000000001%
Data &B0000000000000000%


Data &B0001111111111110%
Data &B0011111111111111%
Data &B0111111111111111%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%


Data &B0001111111111110%
Data &B0011111111111111%
Data &B0111111111111111%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111111111111111%
Data &B0011111111111111%
Data &B0001111111111111%


Data &B0111111111111111%
Data &B0111111111111111%
Data &B0111111111111111%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%


Data &B0001111111111100%
Data &B0011111111111110%
Data &B0111111111111111%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111111111111111%
Data &B0011111111111110%
Data &B0001111111111100%


Data &B0001111111111111%
Data &B0011111111111111%
Data &B0111111111111111%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000011%
Data &B0111000000000001%
Data &B0000000000000000%


Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%

Tbl_punto1:
Data &B0000000000000000%
Data &B0001110000000000%
Data &B0011111000000000%
Data &B0111111100000000%
Data &B0111111100000000%
Data &B0011111000000000%
Data &B0001110000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%
Data &B0000000000000000%



Loaded_arch: