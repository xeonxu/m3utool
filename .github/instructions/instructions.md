---
description: 'Coding guidelines and best practices for the m3utool Common Lisp project'
applyTo: '**/*.lisp, **/*.asd, **/*.ros, **/Makefile'
---

# m3utool Project Guidelines

## Project Overview

m3utool is a Common Lisp tool for converting M3U playlist files to/from Excel (XLSX) format, with features for URL transformation, server proxy stripping, and playlist validation. The tool is built using:

- **Language**: Common Lisp (ANSI CL)
- **Build System**: ASDF (Another System Definition Facility)
- **CLI Framework**: Clingon
- **Package Manager**: Quicklisp with Qlot for dependency locking
- **Runtime**: Roswell (for executable generation and script execution)
- **Testing**: Rove testing framework

## Common Lisp Coding Standards

### Package Definitions

- Always use `eval-when` with `:load-toplevel :compile-toplevel :execute` when loading dependencies at the top of files
- Define packages with explicit `:use`, `:import-from`, and `:export` clauses
- Use keyword symbols (e.g., `:cl`, `:clingon`) in package definitions
- Export only the public API functions; keep internal functions private

### Code Style

- Use lowercase with hyphens for function and variable names (e.g., `transform-uri`, `playlist-item`)
- Use `defclass` for data structures with proper `:initarg`, `:initform`, and `:accessor` slots
- Include docstrings for classes, methods, and complex functions
- Prefer `let*` over nested `let` when bindings depend on previous ones
- Use `when` for single-branch conditionals, `if` for two-branch
- Use descriptive variable names; avoid single-letter names except for common iterators

### Error Handling

- Use `when` guards to check for nil/empty values before processing
- Provide meaningful error messages with context
- Use `return-from` to exit early from error conditions
- Handle edge cases explicitly (empty strings, nil values, missing files)

### Comments

- Use `;;` for inline comments explaining complex logic
- Use section separators like `;; ============== Section Name ==============` for major code sections
- Include "Hot Patch" sections when working around library bugs, with clear documentation
- Add testing notes and examples in comments when behavior is non-obvious

## Project Architecture

### Module Organization

The project is organized into these core modules:

1. **m3u-data** - Core data structures (`playlist-item` class) and URI transformation logic
2. **m3u-xlsx** - Excel file I/O (reading/writing XLSX files with cl-excel)
3. **m3u-check** - URL validation and liveness checking with concurrent workers
4. **m3u-server** - HTTP server functionality (using Hunchentoot)
5. **m3u-cli** - Command-line interface (using Clingon framework)
6. **deploy-settings** - Build and deployment configuration

### Data Flow

- M3U files are parsed into `playlist-item` objects with duration, title, URI, and attributes
- Attributes are stored in hash tables with string keys (e.g., "tvg-id", "group-title")
- URI transformations support proxy stripping and server URL prepending
- XLSX export/import preserves all attributes dynamically

## Build and Test Workflow

### Building

```bash
make prepare      # Install Roswell and Qlot if needed
make build        # Build using ASDF
make ros-build    # Build executable with Roswell
```

### Testing

```bash
make test         # Run all tests with Rove
```

- Always run tests before submitting changes
- Tests use the Rove framework with `deftest` and `testing` macros
- Use `(ok ...)` assertions for test validation
- Create temporary files in `(uiop:temporary-directory)` and clean them up
- Tests may require internet connectivity for URL validation tests (httpbin.org)

### Development

- Use Qlot for dependency management (defined in `qlfile`)
- Lock dependencies with `qlfile.lock` for reproducible builds
- The project uses `m3utool.ros` as the Roswell script entry point
- Binary builds target both Unix (`m3utool`) and Windows (`m3utool.exe`)

## Dependency Management

### Quicklisp Libraries

The project depends on these libraries (see `m3utool.asd`):

- `clingon` - CLI argument parsing
- `cl-ppcre` - Regular expressions
- `cl-excel` - Excel file handling (custom git dependency in qlfile)
- `alexandria` - Utility library
- `str` - String manipulation
- `dexador` - HTTP client
- `lparallel` - Parallel processing
- `bordeaux-threads` - Threading abstraction
- `hunchentoot` - Web server

### Adding Dependencies

1. Add to `:depends-on` in `m3utool.asd`
2. If using a git dependency, add to `qlfile`
3. Run `qlot install` to update `qlfile.lock`
4. Test thoroughly as Quicklisp library compatibility can vary

## M3U Format Specifics

### Parsing

- M3U files use `#EXTINF` tags for metadata
- Format: `#EXTINF:duration,title`
- Attributes can be quoted or unquoted (e.g., `tvg-id="channel1"` or `tvg-id=channel1`)
- Handle mixed separators (spaces, commas between attributes)
- Store all attribute keys in lowercase for consistency

### URI Transformation

- Support proxy stripping with regex pattern matching (e.g., extract `udp://IP:Port` from proxy URLs)
- Support prepending new server URLs
- Handle both HTTP and non-HTTP protocols (rtp://, udp://)
- Preserve original URI if no transformation applies

## File Organization

- Source files in `src/` directory
- Test files in `tests/` directory
- ASDF system definition in `m3utool.asd`
- Roswell entry point in `m3utool.ros`
- Build configuration in `Makefile`
- Sample files (`sample.m3u`, `sample.xlsx`) for testing

## Testing Requirements

**IMPORTANT**: Please test and make sure the implemented functions are well suited to the requirements before raising any PR.

- Write unit tests for all new functions
- Test edge cases (empty inputs, nil values, malformed data)
- Verify URI transformations with various input patterns
- Test concurrent operations (m3u-check module)
- Clean up temporary test files
- Ensure tests pass both locally and in CI environment

## Platform Considerations

### Cross-Platform Support

- Code must work on Linux, macOS, and Windows
- Use UIOP for portable file operations
- Windows uses `m3utool.exe`, Unix uses `m3utool`
- Makefile handles platform differences (check `OS` variable)
- Windows Qlot may not support parallel operations or symlinks

### Roswell Configuration

- PATH must include Roswell binary directories
- Support both global Quicklisp and Qlot environments
- Script auto-detects Qlot setup and loads it if present
- Falls back to global Quicklisp with warning

## Best Practices

1. **Minimize external dependencies** - Only add libraries when necessary
2. **Test before committing** - Run `make test` to validate changes
3. **Handle errors gracefully** - Check for nil, validate inputs
4. **Document complex logic** - Add comments for regex patterns, transformations
5. **Preserve data integrity** - Ensure round-trip M3U → XLSX → M3U preserves all data
6. **Concurrent safety** - Use thread-safe operations when using lparallel
7. **Performance** - Consider using parallel processing for I/O-bound operations
8. **Compatibility** - Test on multiple CL implementations if possible (SBCL is primary target)
