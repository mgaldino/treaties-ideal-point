import pathlib
import sys
import tempfile
import unittest
from unittest import mock

ROOT = pathlib.Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "scripts" / "python"))

import _common  # noqa: E402


class TestCommonUtils(unittest.TestCase):
    def test_extract_links_resolves_relative(self):
        html = '<a href="file.csv">CSV</a><img src="/img/logo.png">'
        base = "https://example.com/path/"
        links = _common.extract_links(html, base)
        self.assertIn("https://example.com/path/file.csv", links)
        self.assertIn("https://example.com/img/logo.png", links)

    def test_filter_links_ext(self):
        links = [
            "https://example.com/a.csv",
            "https://example.com/b.zip",
            "https://example.com/c.txt",
        ]
        filtered = _common.filter_links(links, [".csv", ".zip"])
        self.assertEqual(set(filtered), {"https://example.com/a.csv", "https://example.com/b.zip"})

    def test_download_from_landing_dry_run(self):
        html = '<a href="data.csv">CSV</a>'
        with mock.patch.object(_common, "fetch_html", return_value=html):
            with tempfile.TemporaryDirectory() as tmp:
                payload = _common.download_from_landing(
                    source_name="Test",
                    landing_url="https://example.com/",
                    out_dir=pathlib.Path(tmp),
                    exts=[".csv"],
                    dry_run=True,
                )
        self.assertIn("Dry run", " ".join(payload.get("notes", [])))
        self.assertIn("https://example.com/data.csv", payload.get("candidate_links", []))

    def test_download_from_landing_no_links(self):
        html = "<html><body>No files here</body></html>"
        with mock.patch.object(_common, "fetch_html", return_value=html):
            with tempfile.TemporaryDirectory() as tmp:
                payload = _common.download_from_landing(
                    source_name="Test",
                    landing_url="https://example.com/",
                    out_dir=pathlib.Path(tmp),
                    exts=[".csv"],
                    dry_run=False,
                )
        self.assertTrue(payload.get("errors"))
        self.assertIn("No downloadable files", " ".join(payload.get("errors", [])))


if __name__ == "__main__":
    unittest.main()
