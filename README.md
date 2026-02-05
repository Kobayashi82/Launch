
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

[README en Español](README_es.md)

`Launch` is a guided ascent autopilot for `Kerbal Space Program` (KSP) written in `kOS` (Kerbal Operating System). It targets a chosen apoapsis and inclination, manages pitch program, and can auto-stage and circularize when requested.

`NOTES`: This README is only a template and does not represent the current state of the project. It is also not finished.

## ✨ Features

- `Targeted Ascent`: Set apoapsis and inclination before launch
- `Auto-Stage`: Optional staging during ascent
- `Auto-Circularize`: Builds a circularization node and executes it
- `Target Assist`: Match orbit parameters with an existing target vessel
- `Dynamic Pitch Program`: Adjusts turn based on TWR and atmosphere
- `Live Telemetry`: Stage delta-v, total delta-v spent, and orbit info

## 🖥️ Requirements

- `Kerbal Space Program` with `kOS mod` installed

## 🔧 Installation

1. Install kOS mod for Kerbal Space Program
2. Clone or download this repository
3. Copy all `.ks` files to your KSP `Ships/Script` folder or load them onto your craft's kOS processor

## 🎮 Usage

### Start the Script

```kerboscript
run Launch.
```

### Interactive Controls

- `Up/Down Arrow Keys`: Change target apoapsis (1,000 m)
- `P` / `Shift+P`: Change target apoapsis (10,000 m)
- `Left/Right Arrow Keys`: Change target inclination (1 deg)
- `A`: Toggle auto-stage
- `C`: Toggle auto-circularize
- `T` / `Shift+T`: Cycle available orbit targets
- `Enter`: Start the launch

## 📚 Troubleshooting

### No Engines
- The script stops if it cannot find active engines in staging

### Already in Orbit
- If the vessel is already in orbit, the script exits to avoid conflicts

## 📄 License

This project is licensed under the WTFPL – [Do What the Fuck You Want to Public License](http://www.wtfpl.net/about/).

---

<div align="center">

**🚀 Desarrollado por Kobayashi82 🚀**

*"The Kraken has been notified of our departure"*

</div>
