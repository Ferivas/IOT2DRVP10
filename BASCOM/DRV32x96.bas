'DRV32X192_SD.bas
'
'                 WATCHING Soluciones Tecnológicas
'                    Fernando Vásquez - 25.06.15
'
' Programa para manejar matriz de 32x192 pixels
' Puede manejar animaciones almacenadas en una memoria SD
' Reloj en tiempo real
' Incluye recepcion de datos de GPS por puerto serial 2

$version 0 , 1 , 157
$regfile = "m328pdef.dat"
$crystal = 11059200
$baud = 9600

$hwstack = 140
$swstack = 140
$framesize = 140


'Declaracion de constantes


'Configuracion de entradas/salidas

'Led1 Alias Portc.2                                          'LED VERDE
'Config Led1 = Output

'Led2 Alias Portc.1                                          'LED AMARILLO
'Config Led2 = Output

Const Nummatriz = 6                                         'Una matriz esunmodulo P10 de 16x32
Const Longbuf = Nummatriz * 64
Const Longdat = Nummatriz * 32
Const Longdat_mas_uno = Longdat + 1
Const Longbuf_mas_uno = Longbuf + 1
Const Numtxser = Longbuf / 4
Const Numtxser_2 = Numtxser / 2

Ledout Alias Portb.0
Config Ledout = Output


'DRIVER P10
Sela Alias Portc.0
Config Sela = Output

Selb Alias Portc.1
Config Selb = Output

Sck Alias Portc.2
Config Sck = Output

Datos Alias Portc.3
Config Datos = Output

Oena2 Alias Portb.1
Config Oena2 = Output

Lena2 Alias Portb.2
Config Lena2 = Output

Oena Alias Portd.2
Config Oena = Output

Lena Alias Portd.3
Config Lena = Output

Esp32rst Alias Portb.5
Set Esp32rst
Config Esp32rst = Output

'Configuración de Interrupciones
'TIMER0
'Configuración de Interrupciones
'TIMER0
Config Timer0 = Timer , Prescale = 1024
On Timer0 Tim0_isr
'Enable Timer0
Start Timer0

' Puerto serial 1
Open "com1:" For Binary As #1
On Urxc At_ser1
Enable Urxc

Enable Interrupts


'*******************************************************************************
'* Archivos incluidos
'*******************************************************************************

$include "DRV32X96_archivos.bas"

Call Inivar()

Enable Timer0

Do

   If Sernew = 1 Then                                       'DATOS SERIAL 1
      Reset Sernew
      Print #1 , "SER1=" ; Serproc
      Call Procser()
   End If

   If Newseg = 1 Then
      Reset Newseg
      Call Verackesp32()
      Incr Cntrseg
      Cntrseg = Cntrseg Mod 60
      If Cntrseg = 0 Then
         Atsnd = "ACK,1,2,3"
         Tmpw = Len(atsnd)
         Tmpcrc32 = Crc32(atsnd , Tmpw)
         Atsnd = Atsnd + "&" + Hex(tmpcrc32) + Chr(10)
         Print #1 , "%" ; Atsnd;
      End If
   End If

Loop