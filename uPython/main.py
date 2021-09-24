

#PROGRAMA PRINCIPAL
# Se utilizoModulo TTGO-LORA con el serial 2configurado asi:
# ser = UART(2,tx=12,rx=13,timeout=1)
#
# Las APikeys esta alamcenadas en un diccionario listakeys
import wifimgr
from machine import UART
from machine import Pin
import machine
import ubinascii
import network
import time
import urequests
import ntptime 

listakeys={"6E756E6B776FFF00140E":"IKJ9CR7AVH9W2MT2"}

ledwifi=Pin(22,Pin.OUT)
ledwifi.value(0)

wlan = wifimgr.get_connection()
if wlan is None:
    print("Could not initialize the network connection.")
    while True:
        pass  # you shall not pass :D

ledwifi.value(1)
# Main Code goes here
print("EMPIEZO COMEDOG TB")
#apitb="FH9OPBQI9PTN1EEI"
#idtb=32870
#apikeymdc="FH9OPBQI9PTN1EEI"
cntrmdc=0

#****************************************************************
#   Pines y configuracion
#****************************************************************
#led=Pin(22,Pin.OUT)
led=Pin(2,Pin.OUT)
ser = UART(2,tx=16,rx=17,timeout=0)
timer = machine.Timer(1) 

#****************************************************************
#   FUNCIONES
#****************************************************************
# ENVIA DATOS A IOT
def sndts(apikey,valfeed,fecha,hora):
    url='http://iot.watching.com.ec:3000/update?api_key='+apikey
    cntr=0
    while cntr<8:
        if valfeed[cntr]!='':
            tmpstr=str(cntr+1)
            url=url+'&field'+tmpstr+"="+valfeed[cntr]
        cntr+=1
    if fecha!="" and hora!="":
      fechastr='20'+fecha[6:8]+'-'+fecha[3:5]+'-'+fecha[0:2]+'T'+hora+'UTC-05'
      url=url+'&created_at='+fechastr
    response = urequests.get(url)
    #print(response)
    if response.status_code == 200:
        print('TX IOT OK')
    else:
        print("Error code %s" % response.status_code)
        
def exetb(apitb,idtb):
    url='https://api.thingspeak.com/talkbacks/'+idtb+'/commands/execute.json?api_key='+apitb
    print(url)
    r = urequests.post(url)
    results = r.json()
    if r.status_code==200:
        print("Consulta TB a TS OK")
        results = r.json()
        try:
            comando=results.get('command_string')
        except:
            comando=''
        return comando
    else:
      print("Error ejecutando Talkback (status code:"+str(r.status_code)+")")
      comando='ERR'
      return comando
      


KEYS_TB = 'keys_tb.txt'

def write_tb(idtb,apiktb):
    with open(KEYS_TB, "w") as f:
        salida=idtb+','+apiktb
        f.write(salida)
        f.close()
        
def read_tb(KEYS_TB):
    with open(KEYS_TB) as f:
        llaves=f.readlines()
        keys=llaves[0].split(',')
        return keys
#****************************************************************
#INTERRUPCIONES
#****************************************************************
timerflag=False
cntrt=0
def handleInterrupt(timer):

  global tled
  global timerflag
  global cntrt
  cntrt=cntrt+1
  cntrt=cntrt%200  #Int Timer cada 60 segundos
  if cntrt==0:
    timerflag=True
  tled=tled+1
  tled=tled%16
  if tled <1:
    led.value(1)
  else:
    led.value(0) 

#****************************************************************
#   PROGRAMA MAIN
#****************************************************************

print("MDC CMDG")
led.value(1)
try:
  keystb=read_tb(KEYS_TB)
  idtb=keystb[0]
  apitb=keystb[1]
  print('IDTB',idtb)
  print('Ktb',apitb)
except:
  idtb='1'
  apitb='2'
  print('Sin configurar TB')

ser.init(9600, bits=8, parity=None, stop=1,timeout=1)
timer = machine.Timer(1) 
tled=0
timer.init(period=100, mode=machine.Timer.PERIODIC, callback=handleInterrupt)

arx=""
while True:
  try:
    arxb=ser.readline()
  except:
    arxb=None
  if arxb!=None:
    try:
      arx=str(arxb.decode("ascii"))
    except:
      arxb=None
    if len(arx)>0:
      print(arx)
      if arx[0]=='$':
        #print(arx)
        #salida="$OK"
        arx1=arx[1:]
        lista=arx1.split('&')
        if len(lista)==2:
          print(lista[1])
          datob=str.encode(lista[0])
          crccalc=ubinascii.crc32(datob)
          print("CRCcal=",crccalc)    
          crcval='0x'+lista[1]
          try:
            crcdec=int(crcval,0)
          except:
            crcdec=0
          print("CRCrx=",crcdec)
          if crccalc==crcdec:
            print('CRC OK')
            salida='%OK\r\n'
            ser.write(salida)
            trama=lista[0].split(',')
            campos=["","","","","","","",""]
            if len(trama)==12:
              #apikey=trama[3]
              #apikey=APIKEYWIRELESS
              apikey=listakeys.get(trama[3])
              if apikey==None:
                apikey=trama[3]
              print("Apikey>",apikey)
              cntr=0
              while cntr<8:
                ptr=cntr+4
                if trama[ptr]!="":
                  campos[cntr]=trama[ptr]
                cntr=cntr+1
              print(campos)
              try:
                sndts(apikey,campos,trama[1],trama[2])
              except:
                print('ERROR TX IOT')
            else:

              print('Trama no val')
          else:
            print('CRC ERROR')
            salida='%ERR\r\n'
            ser.write(salida)          
        else:
          print('SIN CRC')
      else:
        #salida='%ERR\r\n'
        #ser.write(salida)

        print('Sin $')
        if arx[0]=='%':
          arx1=arx[1:]
          lista=arx1.split(',')
          if len(lista)==4:
              cmdmain=lista[0].upper()
              if cmdmain=='SETKTB':
                print('KEYS TB >',lista[1],lista[2])
                idtb=lista[1]
                apitb=lista[2]
                try:
                  write_tb(idtb,apitb)
                  print('Act. keys TB OK')
                  salida='$OK,'+idtb+','+apitb+',D\r\n'
                  ser.write(salida)
                except:
                  print('ERR saving keys TB')
              if cmdmain=='LEEKTB':
                salida='$'+cmdmain+","+idtb+','+apitb+'\r\n'
                ser.write(salida)
              if cmdmain=="ACK":
                #salida='$'+cmdmain+'\r\n'
                salida='$'+'ACKACK'+'\r\n'
                ser.write(salida)                
                

  if timerflag:
 #   print(RTC().datetime())
    timerflag=False
    cntrmdc=cntrmdc+1
    print('Int',cntrmdc)
    try:
      comando=exetb(apitb,idtb)
      print(comando)
      if comando!=None:
        salida='$'+comando+'\r\n'
        ser.write(salida)

    except:
      print("ERROR TALKBACK")






