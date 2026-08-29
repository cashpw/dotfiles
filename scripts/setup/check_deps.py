#!/usr/bin/env python3
"""
Dotfiles Dependency Resolver & Package Command Generator
Checks system programs and fonts against packages.json manifest and outputs merged package manager commands.
"""

import sys
import os
import json
import shutil
import subprocess
import argparse
from pathlib import Path

MANIFEST_PATH = Path(__file__).resolve().parent.parent.parent / "packages.json"

def detect_os():
    os_info = {"id": "unknown", "name": "Linux", "like": []}
    os_release = Path("/etc/os-release")
    if os_release.exists():
        with open(os_release, "r", encoding="utf-8") as f:
            for line in f:
                if "=" in line:
                    k, v = line.strip().split("=", 1)
                    v = v.strip('"\'')
                    if k == "ID":
                        os_info["id"] = v.lower()
                    elif k == "NAME":
                        os_info["name"] = v
                    elif k == "ID_LIKE":
                        os_info["like"] = [x.lower() for x in v.split()]

    pkg_mgr = "apt"
    if os_info["id"] in ["fedora", "rhel", "centos"] or "fedora" in os_info["like"]:
        pkg_mgr = "dnf"
    elif os_info["id"] in ["arch", "manjaro"] or "arch" in os_info["like"]:
        pkg_mgr = "pacman"
    elif os_info["id"] in ["debian", "ubuntu", "glinux"] or "debian" in os_info["like"] or "ubuntu" in os_info["like"]:
        pkg_mgr = "apt"

    is_work = Path("/usr/local/google/home/cashweaver").exists()
    profile = "Work (gLinux)" if is_work else f"Personal ({os_info['name']})"

    return {
        "id": os_info["id"],
        "name": os_info["name"],
        "pkg_mgr": pkg_mgr,
        "is_work": is_work,
        "profile": profile
    }

def check_binary(binaries):
    for b in binaries:
        if shutil.which(b) is not None:
            return True
    return False

def check_env_var(var_name):
    if var_name in os.environ and os.environ[var_name]:
        return True
    # Fallback common directory check for NVM
    if var_name == "NVM_DIR" and (Path.home() / ".config" / "nvm").exists():
        return True
    return False

def check_font(pattern):
    if not shutil.which("fc-list"):
        return False
    try:
        res = subprocess.run(["fc-list", ":", "family"], capture_output=True, text=True, check=False)
        return pattern.lower() in res.stdout.lower()
    except Exception:
        return False

def check_script(script_cmd):
    try:
        res = subprocess.run(script_cmd, shell=True, capture_output=True, check=False)
        return res.returncode == 0
    except Exception:
        return False

def check_flatpak(app_id):
    if not shutil.which("flatpak"):
        return False
    try:
        res = subprocess.run(["flatpak", "info", app_id], capture_output=True, check=False)
        return res.returncode == 0
    except Exception:
        return False

def resolve_dependencies(manifest, os_ctx):
    pkg_mgr = os_ctx["pkg_mgr"]

    installed_progs = []
    missing_progs = []

    for prog in manifest.get("programs", []):
        is_installed = False

        if "check_script" in prog and prog["check_script"]:
            is_installed = check_script(prog["check_script"])
        elif "env_var" in prog and prog["env_var"]:
            is_installed = check_env_var(prog["env_var"])
        elif "binary" in prog and prog["binary"]:
            is_installed = check_binary(prog["binary"])

        # Fallback: Check Flatpak installation if package specifies Flatpak
        if not is_installed and "packages" in prog and "flatpak" in prog["packages"]:
            fp_val = prog["packages"]["flatpak"]
            app_id = fp_val.split()[-1]
            if check_flatpak(app_id):
                is_installed = True

        if is_installed:
            installed_progs.append(prog)
        else:
            missing_progs.append(prog)

    installed_fonts = []
    missing_fonts = []

    for font in manifest.get("fonts", []):
        if check_font(font.get("font_pattern", "")):
            installed_fonts.append(font)
        else:
            missing_fonts.append(font)

    return {
        "installed_progs": installed_progs,
        "missing_progs": missing_progs,
        "installed_fonts": installed_fonts,
        "missing_fonts": missing_fonts,
    }

def generate_commands(missing_progs, missing_fonts, os_ctx):
    pkg_mgr = os_ctx["pkg_mgr"]

    repo_enables = set()
    native_packages = []
    flatpak_packages = []
    cargo_packages = []
    pip_packages = []
    custom_installs = []

    # Helper to resolve source for a program based on per-program preferences
    def resolve_source_for_prog(prog):
        pkgs = prog.get("packages", {})

        # Default global fallback order if no preferred_sources declared
        default_order = [pkg_mgr, "flatpak", "copr", "cargo", "pip", "custom"]
        pref_order = prog.get("preferred_sources", default_order)

        # Ensure native pkg_mgr is tried if present in packages
        for source in pref_order:
            if source == pkg_mgr and pkg_mgr in pkgs:
                return pkg_mgr, pkgs[pkg_mgr]
            elif source == "flatpak" and "flatpak" in pkgs:
                return "flatpak", pkgs["flatpak"]
            elif source == "cargo" and "cargo" in pkgs:
                return "cargo", pkgs["cargo"]
            elif source == "pip" and "pip" in pkgs:
                return "pip", pkgs["pip"]
            elif source == "custom" and "custom_install" in prog:
                return "custom", prog["custom_install"]

        # Final fallback: check any available package
        if pkg_mgr in pkgs:
            return pkg_mgr, pkgs[pkg_mgr]
        if "flatpak" in pkgs:
            return "flatpak", pkgs["flatpak"]
        if "cargo" in pkgs:
            return "cargo", pkgs["cargo"]
        if "pip" in pkgs:
            return "pip", pkgs["pip"]
        if "custom_install" in prog:
            return "custom", prog["custom_install"]

        return None, None

    for prog in missing_progs:
        source_type, val = resolve_source_for_prog(prog)
        if not source_type:
            continue

        if source_type == pkg_mgr:
            if isinstance(val, dict):
                pkg_name = val.get("package")
                if "repo_enable" in val:
                    repo_enables.add(val["repo_enable"])
                native_packages.append(pkg_name)
            else:
                native_packages.append(val)
        elif source_type == "flatpak":
            flatpak_packages.append(val)
        elif source_type == "cargo":
            cargo_packages.append(val)
        elif source_type == "pip":
            pip_packages.append(val)
        elif source_type == "custom":
            custom_installs.append(val)

    # Process fonts
    for font in missing_fonts:
        pkgs = font.get("packages", {})
        if pkg_mgr in pkgs:
            val = pkgs[pkg_mgr]
            if isinstance(val, dict):
                native_packages.append(val.get("package"))
                if "repo_enable" in val:
                    repo_enables.add(val["repo_enable"])
            else:
                native_packages.append(val)
        elif "custom_install" in font:
            custom_installs.append(font["custom_install"])

    commands = []

    # Primary native package manager joint command
    if native_packages:
        cmd_parts = []
        if repo_enables:
            cmd_parts.extend(sorted(list(repo_enables)))

        pkg_str = " ".join(sorted(list(set(native_packages))))
        if pkg_mgr == "dnf":
            cmd_parts.append(f"sudo dnf install -y {pkg_str}")
        elif pkg_mgr == "apt":
            cmd_parts.append(f"sudo apt update && sudo apt install -y {pkg_str}")
        elif pkg_mgr == "pacman":
            cmd_parts.append(f"sudo pacman -S --noconfirm {pkg_str}")

        commands.append(("Native Package Manager", " && ".join(cmd_parts)))

    if flatpak_packages:
        fp_str = " ".join(flatpak_packages)
        commands.append(("Flatpak", f"flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo && flatpak install -y {fp_str}"))

    if cargo_packages:
        cargo_cmds = [f"cargo install {c}" for c in cargo_packages]
        commands.append(("Cargo", " && ".join(cargo_cmds)))

    if pip_packages:
        pip_str = " ".join(pip_packages)
        commands.append(("Pip", f"pip install {pip_str}"))

    if custom_installs:
        commands.append(("Custom Installers", "\n".join([f"  - {c}" for c in custom_installs])))

    return commands

def print_info(manifest, prog_id):
    for prog in manifest.get("programs", []) + manifest.get("fonts", []):
        if prog["id"] == prog_id:
            print("\n".join(prog.get("doc", [f"# {prog['id']}", prog.get("description", "")])))
            return
    print(f"Error: Program or font '{prog_id}' not found in manifest.", file=sys.stderr)
    sys.exit(1)

def main():
    parser = argparse.ArgumentParser(description="Dotfiles Dependency Checker & Command Generator")
    parser.add_argument("command", nargs="?", choices=["check", "info"], default="check", help="Action to perform")
    parser.add_argument("target", nargs="?", help="Target program ID for 'info' action")
    parser.add_argument("--json", action="store_true", help="Output status in JSON format")
    parser.add_argument("--install", action="store_true", help="Interactively run generated package manager commands")
    parser.add_argument("--exit-code", action="store_true", help="Exit with code 2 if missing dependencies are detected")
    args = parser.parse_args()

    if not MANIFEST_PATH.exists():
        print(f"Error: Manifest file not found at {MANIFEST_PATH}", file=sys.stderr)
        sys.exit(1)

    with open(MANIFEST_PATH, "r", encoding="utf-8") as f:
        manifest = json.load(f)

    if args.command == "info":
        if not args.target:
            print("Error: Specify program ID (e.g. 'check_deps.py info i3status-rs')", file=sys.stderr)
            sys.exit(1)
        print_info(manifest, args.target)
        return

    os_ctx = detect_os()
    results = resolve_dependencies(manifest, os_ctx)

    installed_count = len(results["installed_progs"])
    total_progs = len(manifest.get("programs", []))
    installed_fonts_count = len(results["installed_fonts"])
    total_fonts = len(manifest.get("fonts", []))

    if args.json:
        out = {
            "os": os_ctx,
            "summary": {
                "programs": {"installed": installed_count, "total": total_progs},
                "fonts": {"installed": installed_fonts_count, "total": total_fonts}
            },
            "missing_programs": [p["id"] for p in results["missing_progs"]],
            "missing_fonts": [f["id"] for f in results["missing_fonts"]]
        }
        print(json.dumps(out, indent=2))
        if (results["missing_progs"] or results["missing_fonts"]) and args.exit_code:
            sys.exit(2)
        return

    print("=" * 64)
    print(" Dotfiles System Dependency Check")
    print(f" Profile: {os_ctx['profile']}")
    print(f" Package Manager: {os_ctx['pkg_mgr']}")
    print("=" * 64)
    print(f"  Programs Installed: {installed_count} / {total_progs}")
    print(f"  Fonts Installed:    {installed_fonts_count} / {total_fonts}")

    missing_progs = results["missing_progs"]
    missing_fonts = results["missing_fonts"]

    if not missing_progs and not missing_fonts:
        print("\n [✓] All expected programs and fonts are installed!")
        print("=" * 64)
        return

    print(f"\n [!] Missing Items ({len(missing_progs)} programs, {len(missing_fonts)} fonts):")
    for p in missing_progs:
        print(f"   - {p['id']}: {p.get('description', '')}")
    for f in missing_fonts:
        print(f"   - [Font] {f['id']}: {f.get('description', '')}")

    commands = generate_commands(missing_progs, missing_fonts, os_ctx)

    if commands:
        print("\n" + "-" * 64)
        print(" Generated Joint Package Commands:")
        print("-" * 64)
        for label, cmd in commands:
            print(f"\n>>> {label}:")
            print(f"{cmd}")
        print("-" * 64)

    print("\n" + "-" * 64)
    print(" ℹ Note: Some custom binaries or environment paths (e.g. Doom Emacs,")
    print("   NVM, local scripts like 'wlprop') will only be discovered in $PATH")
    print("   after symlinking dotfiles or executing custom setup scripts.")
    print("-" * 64)

    if args.install and commands:
        print("\nExecuting package installation commands...")
        for label, cmd in commands:
            if label == "Custom Installers":
                print("\nPlease run custom installers manually when needed:")
                print(cmd)
                continue
            confirm = input(f"\nExecute {label} command? [Y/n] ").strip().lower()
            if confirm in ["", "y", "yes"]:
                subprocess.run(cmd, shell=True)
            else:
                print(f"Skipped {label}.")

    if (missing_progs or missing_fonts) and args.exit_code:
        sys.exit(2)

if __name__ == "__main__":
    main()
