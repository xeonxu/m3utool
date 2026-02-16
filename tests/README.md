# M3U Tool - Unit Tests

This directory contains comprehensive unit tests for the m3utool project using the [Rove](https://github.com/fukamachi/rove) testing framework.

## Running Tests

### Prerequisites

- Roswell (Common Lisp implementation manager)
- Qlot (project dependency manager)

### Installation

```bash
# Install dependencies
make install
```

### Running Tests Locally

```bash
# Run all tests
qlot exec ros run \
  --eval '(ql:quickload :m3utool/tests)' \
  --eval '(asdf:test-system :m3utool)' \
  --quit
```

### Running Tests in CI

Tests run automatically via GitHub Actions on:
- Every push to any branch
- Every pull request

## Test Coverage

### M3U Parsing Tests (`test-playlist-item-*`)
- Playlist item creation with properties
- EXTINF line parsing with quoted attributes
- EXTINF line parsing with unquoted attributes
- Attribute key normalization (lowercase)
- Special character handling in titles

### URI Transformation Tests (`test-transform-uri-*`)
- Proxy stripping from URIs
- Server URL addition to URIs
- Combined proxy strip and server add
- No transformation scenarios
- Both UDP and RTP protocol support

### Data Conversion Tests (`test-flexible-csv-*`, `test-collect-*`)
- CSV row to playlist-item conversion
- Case-insensitive header handling
- Attribute collection from multiple items

### I/O Tests (`test-parse-and-save-*`)
- Round-trip M3U file parsing and saving
- Platform-independent temporary file handling
- File content verification

### Edge Cases
- Empty attribute handling
- Invalid duration values
- Special characters (Chinese, symbols)

## Test Structure

All tests follow the Rove framework conventions:

```lisp
(deftest test-name
  (testing "Description of what is being tested"
    (ok (predicate result))
    (ng (predicate-should-fail result))))
```

## Adding New Tests

To add new tests:

1. Add test cases to `main.lisp`
2. Follow the naming convention: `test-<feature>-<specific-behavior>`
3. Include a descriptive `testing` block
4. Use appropriate assertions (`ok`, `ng`, etc.)
5. Clean up any resources (files, etc.) after tests

## Test Dependencies

Tests depend on:
- `:m3utool` - The main system being tested
- `:rove` - Testing framework (specified in m3utool.asd)

## Platform Compatibility

Tests are designed to be platform-independent and will run on:
- Linux
- macOS
- Windows (with proper Common Lisp environment)

All temporary files are created using `uiop:temporary-directory` for cross-platform compatibility.
