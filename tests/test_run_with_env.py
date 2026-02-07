import pathlib
import sys
import unittest
from unittest import mock

ROOT = pathlib.Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "scripts" / "python"))

import run_with_env  # noqa: E402


class TestRunWithEnv(unittest.TestCase):
    def test_requires_separator(self):
        argv_backup = sys.argv
        try:
            sys.argv = ["prog"]
            self.assertEqual(run_with_env.main(), 2)
        finally:
            sys.argv = argv_backup

    def test_requires_command(self):
        argv_backup = sys.argv
        try:
            sys.argv = ["prog", "--"]
            self.assertEqual(run_with_env.main(), 2)
        finally:
            sys.argv = argv_backup

    def test_runs_command(self):
        argv_backup = sys.argv
        try:
            sys.argv = ["prog", "--", "echo", "ok"]
            with mock.patch.object(run_with_env, "_load_env", return_value=None):
                with mock.patch.object(run_with_env.subprocess, "call", return_value=0) as call_mock:
                    self.assertEqual(run_with_env.main(), 0)
            call_mock.assert_called_once_with(["echo", "ok"])
        finally:
            sys.argv = argv_backup

    def test_command_not_found(self):
        argv_backup = sys.argv
        try:
            sys.argv = ["prog", "--", "missingcmd"]
            with mock.patch.object(run_with_env, "_load_env", return_value=None):
                with mock.patch.object(
                    run_with_env.subprocess, "call", side_effect=FileNotFoundError
                ):
                    self.assertEqual(run_with_env.main(), 127)
        finally:
            sys.argv = argv_backup


if __name__ == "__main__":
    unittest.main()
