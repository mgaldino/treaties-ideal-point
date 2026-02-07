import pathlib
import sys
import tempfile
import unittest

ROOT = pathlib.Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "scripts" / "python"))

import acquire_wits_tariffs_batch as batch  # noqa: E402


class TestWitsTariffBatch(unittest.TestCase):
    def test_parse_reporters_default(self):
        reps = batch.parse_reporters("")
        self.assertIn("BRA", reps)
        self.assertIn("USA", reps)
        self.assertNotIn("EU", reps)

    def test_parse_reporters_custom(self):
        reps = batch.parse_reporters("bra, usa CHN;deu")
        self.assertEqual(reps, ["BRA", "USA", "CHN", "DEU"])

    def test_should_skip(self):
        with tempfile.TemporaryDirectory() as tmp:
            out_dir = pathlib.Path(tmp)
            self.assertFalse(batch.should_skip(out_dir))
            (out_dir / "tariffs_partner_000.json").write_text("{}", encoding="utf-8")
            self.assertTrue(batch.should_skip(out_dir))

    def test_classify_status(self):
        self.assertEqual(batch.classify_status(0, [], False), "success")
        self.assertEqual(
            batch.classify_status(1, ["Data not found"], False),
            "no_data",
        )
        self.assertEqual(
            batch.classify_status(1, ["HTTP Error 403"], False),
            "auth_error",
        )
        self.assertEqual(
            batch.classify_status(2, ["Partner 000: HTTP Error 502"], False),
            "partial_error",
        )

    def test_is_transient_error(self):
        self.assertTrue(batch.is_transient_error(["HTTP Error 502"]))
        self.assertFalse(batch.is_transient_error(["Data not found"]))


if __name__ == "__main__":
    unittest.main()
