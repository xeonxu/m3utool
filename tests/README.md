# M3UTool Unit Tests

This directory contains unit tests for the m3utool library.

## Overview

The tests are written using the [Rove](https://github.com/fukamachi/rove) testing framework and cover the following exported interfaces:

### m3u-data Package

- `transform-uri` - Tests for URI transformation including:
  - Basic URI pass-through (no transformation)
  - Stripping proxy addresses from udp/rtp URLs
  - Appending server URLs
  - Combined strip and server operations

### m3u-xlsx Package

- `save-xlsx` - Tests for saving playlist items to XLSX format:
  - Basic XLSX saving with item preservation
  - URI transformation during save
  
- `load-xlsx` - Tests for loading playlist items from XLSX:
  - Basic XLSX loading with item reconstruction

## Running Tests

### Using Make

The easiest way to run tests is using the provided Makefile target:

```bash
make test
```

This will:
1. Install dependencies (Roswell, qlot, etc.)
2. Load the test system
3. Run all tests

### Manual Testing

If you already have Roswell and dependencies installed:

```bash
qlot exec ros run \
  --load m3utool.asd \
  --eval '(ql:quickload :m3utool/tests)' \
  --eval '(asdf:test-system :m3utool)' \
  --eval '(quit)'
```

### From REPL

```lisp
(ql:quickload :m3utool/tests)
(asdf:test-system :m3utool)
```

## Test Structure

Each test file follows this pattern:

1. **Setup**: Create test data (playlist items, test files, etc.)
2. **Execute**: Call the function being tested
3. **Verify**: Check the results using `ok` assertions
4. **Cleanup**: Delete temporary files

## Adding New Tests

To add new tests:

1. Add a new `deftest` form to `tests/main.lisp`
2. Use the `testing` macro for descriptive test names
3. Use `ok` assertions to verify expected behavior
4. Clean up any temporary resources

Example:

```lisp
(deftest test-new-feature
  (testing "Description of what is being tested"
    (let ((result (my-function "input")))
      (ok (string= "expected" result)))))
```

## Notes

- Tests use `/tmp` directory for temporary files
- All tests clean up after themselves
- Tests are designed to be independent and can run in any order
