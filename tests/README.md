# Tests

Run all tests:
```bash
python3 -m unittest discover -s tests
```

## What the tests cover
- `tests/test_common.py`
  - Link extraction from HTML
  - File filtering by extension
  - Dry-run behavior for downloader
  - Error handling when no downloadable links are found

- `tests/test_acquire_scripts.py`
  - Each acquisition script writes an `access_log.json`
  - Scripts run in a mocked environment (no network required)
