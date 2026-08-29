# autotiling-rs

`autotiling-rs` is an automatic layout switcher for Sway and i3 that toggles split orientation between horizontal and vertical based on container window dimensions, creating a Dwindle or Fibonacci spiral pattern.

## Installation

* **Debian / gLinux (Work)**:
  Install using Cargo to compile the binary directly into `~/.cargo/bin`:
  ```bash
  cargo install --git https://github.com/ammgws/autotiling-rs
  ```
* **Fedora (Personal)**:
  Install via Cargo using the Fedora Rust toolchain:
  ```bash
  sudo dnf install cargo
  cargo install --git https://github.com/ammgws/autotiling-rs
  ```
* **Python Alternative (`autotiling`)**:
  If building from source via Cargo is inconvenient, you can install the equivalent Python version via package managers:
  ```bash
  sudo apt install autotiling  # Debian / gLinux
  pip install autotiling       # Fedora / general
  ```
