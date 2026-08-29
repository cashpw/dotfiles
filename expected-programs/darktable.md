# darktable

`darktable` is an open-source photography workflow application and RAW developer.

## Installation

*   **Debian / gLinux (Work)**:
    ```bash
    sudo apt install darktable clinfo
    ```
*   **Fedora (Personal)**:
    ```bash
    sudo dnf install darktable clinfo
    ```

## OpenCL Hardware Acceleration Setup

Darktable uses **OpenCL** to accelerate image rendering, demosaicing, and export operations using the GPU.

### 1. Install OpenCL Runtime Drivers

*   **NVIDIA GPUs**:
    *   *Debian / gLinux*: `sudo apt install nvidia-opencl-icd`
    *   *Fedora*: `sudo dnf install xorg-x11-drv-nvidia-cuda`
*   **Intel GPUs (Broadwell or newer / Arc)**:
    *   *Debian / gLinux*: `sudo apt install intel-opencl-icd`
    *   *Fedora*: `sudo dnf install intel-compute-runtime`
*   **AMD GPUs**:
    *   *Debian / gLinux*: `sudo apt install mesa-opencl-icd` or ROCm driver
    *   *Fedora*: `sudo dnf install mesa-ocl-drivers` or `rocm-clinfo`

### 2. Verify GPU OpenCL Detection

Run `clinfo` in your terminal to verify that your GPU device and OpenCL platform are detected:
```bash
clinfo | grep -E "Platform Name|Device Name"
```

### 3. Test & Enable OpenCL in Darktable

Launch Darktable from the terminal with OpenCL debugging enabled to test performance:
```bash
darktable -d opencl
```

Inside Darktable, ensure OpenCL is enabled:
1. Open **Preferences** (gear icon at top right).
2. Go to the **Processing** tab.
3. Under **CPU/GPU/Memory**, check **activate OpenCL support**.
4. Set **opencl scheduling profile** to `default` or `very fast GPU`.
