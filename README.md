# m3utool

A Common Lisp tool to convert Excel (.xlsx) files to M3U playlist files.

## Features

- Convert Excel spreadsheets to M3U format
- Command-line interface built with Clingon
- Cross-platform support (Linux, macOS, Windows)
- Standalone executable binaries

## Installation

### Pre-built Binaries

Download pre-built binaries from the [Releases](https://github.com/xeonxu/m3utool/releases) page:
- **Linux AMD64**: `m3utool-linux-amd64`
- **Linux ARM64**: `m3utool-linux-arm64`
- **macOS ARM64**: `m3utool-macos-arm64`
- **Windows AMD64**: `m3utool-windows-amd64.exe`

### Building from Source

#### Prerequisites

- **Common Lisp Implementation**: SBCL (Steel Bank Common Lisp)
- **Roswell**: Common Lisp environment manager
- **Qlot**: Project-local library installer for Common Lisp

#### Platform-Specific Setup

##### Linux / macOS

```bash
# Install Roswell
curl -L https://raw.githubusercontent.com/roswell/roswell/release/scripts/install-for-ci.sh | sh

# Install Qlot
ros install fukamachi/qlot

# Install project dependencies
qlot install

# Build the binary
qlot exec ros dump executable m3utool.ros -o m3utool
```

Alternatively, use the Makefile:
```bash
make ros-build
```

##### Windows

On Windows, the build process uses **PowerShell** instead of bash because Roswell's `ros` command requires native Windows shell for proper path handling and subprocess management.

**Option 1: Using PowerShell** (Recommended)
```powershell
# Ensure Roswell and Qlot are installed
# (Install via Chocolatey or manual installation)

# Install project dependencies
qlot install

# Build the binary
qlot exec ros dump executable m3utool.ros -o m3utool.exe
```

**Option 2: Using Make (Git Bash/MSYS2)**
```bash
# Note: This may encounter issues with qlot exec ros on Windows
make ros-build
```

#### Alternative Build Method (Cross-Platform)

If you encounter issues with `ros dump executable` on Windows, you can use the direct ASDF build method:

```bash
# Using the Makefile
make build
```

This method uses:
- `asdf:make` with the `:deploy` library
- Automatically excludes compression-lib to avoid path errors
- Works consistently across all platforms

## Build Process Details

### GitHub Actions CI/CD

The project uses GitHub Actions for automated building and releasing across multiple platforms. The workflow is defined in `.github/workflows/release.yml`.

**Key points:**
- **Cross-platform builds**: Linux (x64, ARM64), macOS (ARM64), Windows (x64)
- **Platform-specific shells**: 
  - Unix systems (Linux/macOS): Uses `bash`
  - Windows: Uses `pwsh` (PowerShell) for reliable Roswell/Qlot execution
- **Automated releases**: Triggered on version tags (e.g., `v0.1.0`)
- **Artifact caching**: Dependencies are cached to speed up builds

### Why PowerShell for Windows?

The Windows build uses PowerShell instead of bash because:

1. **Roswell compatibility**: Roswell on Windows is designed for native Windows shells (PowerShell/CMD), not MSYS2/Git Bash
2. **Path handling**: Windows paths are handled correctly by PowerShell, avoiding Unix/Windows path conversion issues
3. **Subprocess management**: Roswell's subprocess spawning works reliably in PowerShell
4. **Environment variables**: Proper initialization of Lisp runtime environment

Attempting to use `bash` (via Git Bash or MSYS2) for `qlot exec ros` commands on Windows can lead to:
- Path conversion errors
- Shell spawning failures
- Roswell unable to find or execute properly

## Dependencies

The project uses the following Common Lisp libraries (managed via Qlot):

- **clingon**: Command-line argument parsing
- **cl-ppcre**: Regular expressions
- **cl-csv**: CSV file handling
- **cl-excel**: Excel file reading (from Git repository)
- **alexandria**: Utility library
- **str**: String manipulation
- **deploy**: Binary deployment system

## Development

### Project Structure

```
m3utool/
├── .github/
│   └── workflows/
│       └── release.yml      # CI/CD workflow
├── src/
│   ├── m3u-data.lisp        # M3U data structures
│   ├── m3u-xlsx.lisp        # Excel file processing
│   ├── m3u-cli.lisp         # CLI interface
│   └── deploy-settings.lisp # Deployment configuration
├── m3utool.asd              # ASDF system definition
├── m3utool.ros              # Roswell executable script
├── qlfile                   # Qlot dependency specification
├── Makefile                 # Build automation
└── README.md                # This file
```

### Running Tests

```bash
# Load the test system
ros run --eval '(ql:quickload :m3utool/tests)' \
        --eval '(asdf:test-system :m3utool)' \
        --quit
```

## License

LLGPL (Lisp Lesser General Public License)

## Author

Noe (xeonxu@gmail.com)

## Contributing

Contributions are welcome! Please feel free to submit pull requests or open issues.

---

**Note**: This project demonstrates cross-platform Common Lisp development with proper Windows support. The build system handles platform-specific requirements automatically.
