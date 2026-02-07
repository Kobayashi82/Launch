
<div align="center">

![WIP](https://img.shields.io/badge/work%20in%20progress-yellow?style=for-the-badge)
![KerboScript](https://img.shields.io/badge/Kerbo%20Script-brown?style=for-the-badge)
![kOS](https://img.shields.io/badge/kOS-Autopilot-blue?style=for-the-badge)
![KSP](https://img.shields.io/badge/Kerbal-Space%20Program-orange?style=for-the-badge)

*Script de lanzamiento automatico a orbita*

</div>

<div align="center">
  <img src="/Launch.png">
</div>

# Launch Autopilot

[README in English](README.md)

`Launch` es un piloto automatico de ascenso para `Kerbal Space Program` (KSP) escrito en `kOS` (Kerbal Operating System). Permite fijar apoapsis e inclinacion, gestiona el programa de giro y permite auto-staging y circularizar si se solicita.

`NOTAS`: Este README es solo una plantilla y no representa el estado actual del proyecto. Además, no está terminado.

## ✨ Caracteristicas

- `Ascenso dirigido`: Define apoapsis e inclinacion antes del despegue
- `Auto-Stage`: Staging automatico durante el ascenso
- `Auto-Circularize`: Crea y ejecuta el nodo de circularizacion
- `Asistencia a objetivo`: Ajusta parametros para igualar un objetivo en orbita
- `Programa de giro dinamico`: Se adapta al TWR y a la atmosfera
- `Telemetria en vivo`: Delta-v por etapa, delta-v gastado y datos de orbita

## 🖥️ Requisitos

- `Kerbal Space Program` con `mod kOS` instalado

## 🔧 Instalacion

1. Instala el mod `kOS` para Kerbal Space Program
2. Clona o descarga este repositorio
3. Copia todos los archivos `.ks` a tu carpeta `Ships/Script` de KSP o cargalos en el procesador kOS de tu nave

## 🎮 Uso

```kerboscript
run launch.
```

### Controles interactivos

- `Flechas Arriba/Abajo`: Cambiar apoapsis objetivo (1.000 m)
- `P` / `Shift+P`: Cambiar apoapsis objetivo (10.000 m)
- `Flechas Izquierda/Derecha`: Cambiar inclinacion objetivo (1 grado)
- `A`: Alternar auto-stage
- `C`: Alternar auto-circularize
- `T` / `Shift+T`: Recorrer objetivos disponibles
- `Enter`: Iniciar el lanzamiento

## 📚 Solucion de problemas

### No hay motores
- El script se detiene si no encuentra motores activos en staging

### Ya esta en orbita
- Si la nave ya esta en orbita, el script se cierra para evitar conflictos

## 📄 Licencia

Este proyecto esta licenciado bajo la WTFPL – [Do What the Fuck You Want to Public License](http://www.wtfpl.net/about/).

---

<div align="center">

**🚀 Desarrollado por Kobayashi82 🚀**

*"The Kraken has been notified of our departure"*

</div>
