# IOT2DRVP10
Envío de datos a driver de matrices P10 por medio de módulo IOT basado en el ESP32

## CONFIGURAR DATOS A LAS MATRICES
El MDC basado en el ESP-32 consulta un Talkback configurado en la cuenta Agua Manta con el ID 43799 y el Apikey Talkback FQSTIKZUZBK49MSB.<br>
El valor se  configura con el comando

*SETPRE,m.nop*

En donde m n o p son digitos en decimal que indican el precio.

Por ejemplo<br>

SETPRE,1.457

Mostrara el valor de 1.457 en el display




