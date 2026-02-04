import pathlib
import sys
import tempfile
import unittest
from unittest import mock

ROOT = pathlib.Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "scripts" / "python"))

import acquire_atop  # noqa: E402
import acquire_desta  # noqa: E402
import acquire_ieadb  # noqa: E402
import acquire_unctad_iia  # noqa: E402
import acquire_wto_rta  # noqa: E402


class TestAcquireScripts(unittest.TestCase):
    def _run_main_with_patch(self, module):
        payload = {"source_name": "Test", "landing_url": "https://example.com", "access_date": "2026-01-31"}
        with tempfile.TemporaryDirectory() as tmp:
            out_dir = pathlib.Path(tmp)
            with mock.patch.object(module, "download_from_landing", return_value=payload):
                with mock.patch.object(module, "print_summary", return_value=None):
                    argv_backup = sys.argv
                    try:
                        sys.argv = ["prog", "--out", str(out_dir)]
                        exit_code = module.main()
                    finally:
                        sys.argv = argv_backup
            self.assertEqual(exit_code, 0)
            log_path = out_dir / "access_log.json"
            self.assertTrue(log_path.exists())

    def test_acquire_desta(self):
        self._run_main_with_patch(acquire_desta)

    def test_acquire_wto_rta(self):
        self._run_main_with_patch(acquire_wto_rta)

    def test_acquire_unctad_iia(self):
        self._run_main_with_patch(acquire_unctad_iia)

    def test_acquire_atop(self):
        self._run_main_with_patch(acquire_atop)

    def test_acquire_ieadb(self):
        self._run_main_with_patch(acquire_ieadb)


if __name__ == "__main__":
    unittest.main()
